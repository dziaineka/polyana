боевой сервер игры polyana
=====

API
---

    PING - ping
    AUTH login password - аутентификация на боевом сервере
    AUTH token - аутентификация на боевом сервере
    STOP - уйти с сервера
    BATTLE sum currency_type - начать бой один против одного со ставкой в
        размере не выше sum и видом валюты currency_type.
        Доступно два вида валюты: 'GOLD' и 'SILVER'.
    ROYALE sum currency_type - начать бой с четырьмя игроками со ставкой в
        размере не выше sum и видом валюты currency_type.
        Доступно два вида валюты: 'GOLD' и 'SILVER'.

    Команды в бою:

    u - шаг вверх
    d - шаг вниз
    l - шаг налево
    r - шаг направо