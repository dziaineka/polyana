# Game Server for strategy game Polyana

Для регистрации и получения токена:

```curl -X POST http://127.0.0.1:8000/polyana/api/registration/ -d "nickname=your_nickname&password=your_password"```


Для логина и получения токена:

```curl -X POST http://127.0.0.1:8000/polyana/api/login/ -d "nickname=your_nickname&password=your_password"```


Для получения статистики:

```curl -X GET http://127.0.0.1:8000/polyana/api/stats/\?nickname\=your_nickname\&token\=your_token```
