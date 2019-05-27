# Game Server for strategy game Polyana

_Описание API_

Для регистрации и получения токена:

```curl -X POST http://127.0.0.1:8000/polyana/api/registration/ -d "nickname=your_nickname&password=your_password"```


Для логина и получения токена:

```curl -X POST http://127.0.0.1:8000/polyana/api/login/ -d "nickname=your_nickname&password=your_password"```


Для получения статистики:

```curl -X GET http://127.0.0.1:8000/polyana/api/stats/\?nickname\=your_nickname\&token\=your_token```



_Запуск игрового сервера_

1. Установить в окружение проекта зависимости из Pipfile (pipenv install; для сборки psycopg2, возможно, придется установить libpq) либо установить зависимости из requirements.txt.
2. Из каталога, в котором находится manage.py выполнить команды:

```bash
python manage.py migrate
```

3. Запустить сервер командой:

```bash
python manage.py runserver
```

4. Перейти на страницу http://127.0.0.1:8000/polyana/registration/
