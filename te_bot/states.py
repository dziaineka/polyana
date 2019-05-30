from aiogram.dispatcher.filters.state import State, StatesGroup


class Form(StatesGroup):
    login_reg = State()
    password_reg = State()
    login_auth = State()
    password_auth = State()
    reg_battle_bid = State()
    reg_battle_currency = State()
    reg_battle = State()
    move = State()
