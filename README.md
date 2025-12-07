# Лабораторная работа №3

Выполнил: Чусовлянов Максим Сергеевич  
Группа: P3307

## Описание

Потоковая интерполяция данных с использованием core.async

## Запуск

```bash
# Линейная интерполяция
lein run -- --algo linear --step 0.5 < data/data.csv

# Лагранж
lein run -- --algo lagrange --window 4 --step 0.5 < data/data.csv

# Тесты
lein test
```
