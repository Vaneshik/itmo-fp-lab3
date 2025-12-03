(ns itmo-fp-lab3.core
  (:gen-class)
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.tools.cli :refer [parse-opts]]
   [clojure.core.async :as async
    :refer [chan >!! <!! mult tap close! go <! >!]]))

; ---- параметры командной строки ----

(def cli-options
  [["-s" "--step STEP" "Шаг дискретизации (число с плавающей точкой)"
    :default 1.0
    :parse-fn #(Double/parseDouble %)
    :validate [pos? "Шаг должен быть положительным числом"]]

   ["-w" "--window N" "Размер окна для полиномиальной интерполяции (>=2)"
    :default nil
    :parse-fn #(Long/parseLong %)
    :validate [#(or (nil? %) (>= % 2)) "Размер окна должен быть не менее 2"]]

   ["-a" "--algo ALGO" "Метод интерполяции: linear, lagrange или несколько через запятую"
    :default "linear"
    :parse-fn str
    :validate [#(every? #{"linear" "lagrange"} (str/split % #","))
               "Допустимые методы: 'linear', 'lagrange' или их комбинация через запятую"]]

   ["-h" "--help" "Вывести справку"]])

; ---- проверка аргументов ----

(defn terminate
  ([exit-code]
   (System/exit exit-code))
  ([exit-code msg]
   (println msg)
   (System/exit exit-code)))

(defn validate-args! [{:keys [options errors summary]}]
  (when (:help options)
    (terminate 0 summary))

  (when (seq errors)
    (terminate 1 (str "Ошибки разбора аргументов:\n" (str/join "\n" errors) "\n\n" summary)))

  (let [methods-str (:algo options)
        methods (str/split methods-str #",")
        methods-set (set methods)
        opts (assoc options :algos methods)]

    ; если есть lagrange - окно обязательно
    (when (and (contains? methods-set "lagrange")
               (nil? (:window opts)))
      (terminate 1 "ОШИБКА: параметр --window обязателен при использовании lagrange"))

    ; linear без lagrange - окно запрещено
    (when (and (= methods-set #{"linear"})
               (some? (:window opts)))
      (terminate 1 "ОШИБКА: параметр --window не требуется для линейной интерполяции"))

    opts))

; ---- алгоритмы интерполяции ----

(defn grid-points
  "Генерирует точки сетки, кратные шагу step, на отрезке [l, r]."
  [l r step]
  (let [tolerance (* 1e-9 step)                ; маленький допуск
        start-k (long (Math/ceil  (/ (- l tolerance) step)))
        end-k (long (Math/floor (/ (+ r tolerance) step)))]
    (map #(* step %) (range start-k (inc end-k)))))

(defn linear-interpolation [points step l r]
  (let [[p1 p2] points
        x1 (:x p1) y1 (:y p1)
        x2 (:x p2) y2 (:y p2)
        x-values (grid-points l r step)]
    (map (fn [x]
           (let [t (/ (- x x1) (- x2 x1))]
             {:x (double x)
              :y (+ y1 (* t (- y2 y1)))}))
         x-values)))

(defn lagrange-polynomial [points x]
  (let [n (count points)]
    (reduce
     (fn [acc i]
       (let [pi (nth points i)
             xi (:x pi)
             yi (:y pi)
             li (reduce
                 (fn [li j]
                   (if (not= i j)
                     (let [pj (nth points j)
                           xj (:x pj)]
                       (* li (/ (- x xj) (- xi xj))))
                     li))
                 1.0
                 (range n))]
         (+ acc (* li yi))))
     0.0
     (range n))))

(defn lagrange-interpolation [points step l r]
  (let [x-values (grid-points l r step)]
    (map (fn [x] {:x (double x)
                  :y (lagrange-polynomial points x)})
         x-values)))

; ----- утилиты окна -----

(defn slide-window
  "Сдвигает окно добавлением новой точки p. При превышении размера удаляет старейшую."
  [window max-size p]
  (let [current-size (count window)]
    (if (< current-size max-size)
      ; окно ещё не заполнено - просто добавляем
      (conj window p)
      ; окно заполнено - сдвигаем и добавляем в конец
      (conj (subvec window 1) p))))

(defn middle-point
  "Находит ближайшую к центру интервала точку, кратную шагу."
  [points step]
  (let [x-start (:x (first points))
        x-end (:x (peek points))
        center (/ (+ x-start x-end) 2.0)        ; центр отрезка
        k (Math/floor (/ center step))          ; нижний индекс кратной точки
        lower (* k step)                        ; кратная точка снизу
        upper (+ lower step)                    ; кратная точка сверху
        dist-lower (Math/abs (- center lower))
        dist-upper (Math/abs (- center upper))]
    (if (<= dist-lower dist-upper)
      (double lower)
      (double upper))))

; ---- валидация -----

(defn check-order
  "Если x новой точки меньше предыдущего x, выбрасывает исключение."
  [prev-x {:keys [x] :as p}]
  (when (and (some? prev-x) (< x prev-x))
    (throw (ex-info "Input x is not nondecreasing"
                    {:prev-x prev-x
                     :x      x
                     :point  p}))))

(defn validate-input-order!
  "Проверяет монотонность x-координат входных точек."
  [input-chan output-chan]
  (go
    (loop [last-x nil]
      (if-let [point (<! input-chan)]
        (do
          (check-order last-x point)
          (>! output-chan point)
          (recur (:x point)))
        (close! output-chan)))))

; ---- обработка сегментов интерполяции -----

(defn process-initial-segment
  [points interpolate-fn step]
  (let [x-start (:x (first points))
        x-middle (middle-point points step)]
    (interpolate-fn points x-start x-middle)))

(defn process-next-segment
  [prev-points curr-points interpolate-fn step]
  (let [prev-middle (middle-point prev-points step)
        curr-middle (middle-point curr-points step)
        segment-start (+ prev-middle step)]
    (when (<= segment-start curr-middle)
      (interpolate-fn curr-points segment-start curr-middle))))

(defn process-final-segment
  [points interpolate-fn step]
  (let [middle (middle-point points step)
        x-final (:x (peek points))
        segment-start (+ middle step)]
    ; Всегда обрабатываем финальную часть если есть точки после середины
    (when (<= segment-start x-final)
      (interpolate-fn points segment-start x-final))))

; ---- обработка ввода -----

(defn parse-point [line]
  (let [[xs ys :as parts] (str/split line #"[;\t\s]+")]
    (when (not= 2 (count parts))
      (throw (ex-info "Ожидается два числа: x y"
                      {:line line :parts parts})))
    {:x (Double/parseDouble xs)
     :y (Double/parseDouble ys)}))

(defn read-input-stream
  "Читает входные данные и отправляет их в канал."
  [output-chan]
  (with-open [reader (io/reader *in*)]
    (doseq [line (line-seq reader)
            :when (not (str/blank? line))]
      (>!! output-chan (parse-point line)))))

; ---- обработка вывода ----

(defn output-writer!
  "Асинхронный процесс для записи результатов в stdout."
  [output-chan]
  (go
    (loop []
      (when-let [[algorithm x-val y-val] (<! output-chan)]
        (println (format "> %s: %.2f %.2f" algorithm x-val y-val))
        (recur)))))

; ---- процессор интерполяции (полиморфная реализация) ----

; Мультиметоды для полиморфизма алгоритмов
(defmulti window-size
  "Возвращает размер окна для алгоритма интерполяции."
  (fn [algo _] algo))

(defmethod window-size "linear" [_ _] 2)
(defmethod window-size "lagrange" [_ window-param] window-param)
(defmethod window-size :default [algo _]
  (throw (ex-info "Неизвестный алгоритм" {:algo algo})))

(defmulti create-interpolator
  "Создает функцию интерполяции для заданного алгоритма."
  (fn [algo _] algo))

(defmethod create-interpolator "linear" [_ step]
  (fn [pts l r] (linear-interpolation pts step l r)))

(defmethod create-interpolator "lagrange" [_ step]
  (fn [pts l r] (lagrange-interpolation pts step l r)))

(defmethod create-interpolator :default [algo _]
  (throw (ex-info "Неизвестный алгоритм" {:algo algo})))

(defn handle-filling
  "Обработка первого заполнения окна."
  [updated-data interpolator step]
  (let [results (process-initial-segment updated-data interpolator step)]
    {:data updated-data :ready? true :results results}))

(defn handle-sliding
  "Обработка сдвига окна."
  [data updated-data interpolator step]
  (let [results (process-next-segment data updated-data interpolator step)]
    {:data updated-data :ready? true :results results}))

(defn handle-finalization
  "Финальная обработка данных."
  [state interpolator step]
  (when (:ready? state)
    (process-final-segment (:data state) interpolator step)))

(defn interpolation-processor!
  "Обрабатывает поток точек выбранным алгоритмом интерполяции."
  [algorithm processor-chan step window-param output-chan]
  (let [max-size (window-size algorithm window-param)
        interpolator (create-interpolator algorithm step)]
    (go
      (loop [state {:data [] :ready? false}]
        (if-let [point (<! processor-chan)]
          ; пришла новая точка
          (let [{:keys [data ready?]} state
                updated-data (slide-window data max-size point)
                is-full? (= (count updated-data) max-size)]
            (cond
              ; 1) окно ещё не заполнено
              (not is-full?)
              (recur (assoc state :data updated-data))

              ; 2) первое заполнение окна -> init
              (not ready?)
              (let [new-state (handle-filling updated-data interpolator step)]
                (when-let [results (:results new-state)]
                  (doseq [{:keys [x y]} results]
                    (>! output-chan [algorithm x y])))
                (recur (dissoc new-state :results)))

              ; 3) steady-state -> step
              :else
              (let [new-state (handle-sliding data updated-data interpolator step)]
                (when-let [results (:results new-state)]
                  (doseq [{:keys [x y]} results]
                    (>! output-chan [algorithm x y])))
                (recur (dissoc new-state :results)))))
          ; processor-chan закрыт -> финализируем
          (do
            (when-let [results (handle-finalization state interpolator step)]
              (doseq [{:keys [x y]} results]
                (>! output-chan [algorithm x y])))
            ; завершение go-блока
            nil))))))

; ---- основная функция ----

(defn -main [& args]
  (let [{:keys [options errors summary]}
        (parse-opts args cli-options)

        {:keys [algos step window]}
        (validate-args! {:options options
                         :errors  errors
                         :summary summary})

        raw-input-chan (chan 100)
        input-chan (chan 100)
        validation-done (validate-input-order! raw-input-chan input-chan)
        multiplexer (mult input-chan)
        output-chan (chan 100)
        writer-done (output-writer! output-chan)]

    (try
      ; запускаем процессоры для каждого алгоритма
      (let [processor-dones
            (doall
             (for [algorithm algos]
               (let [processor-chan (chan 100)]
                 (tap multiplexer processor-chan true)
                 (interpolation-processor! algorithm processor-chan step window output-chan))))]

        ; читаем stdin и льём точки в сырой входной канал
        (read-input-stream raw-input-chan)

        ; закрываем сырой вход (валидатор сам закроет input-chan)
        (close! raw-input-chan)

        ; ждём завершения проверки порядка
        (<!! validation-done)

        ; ждём завершения всех алгоритмов
        (doseq [processor processor-dones]
          (<!! processor))

        ; все алгоритмы закончили - закрываем выходной канал
        (close! output-chan)

        ; ждём, пока допечатает writer
        (<!! writer-done)
        (flush))

      (catch clojure.lang.ExceptionInfo e
        (binding [*out* *err*]
          (println "ERROR:" (.getMessage e))
          (println "Details:" (ex-data e)))
        (System/exit 1)))))
