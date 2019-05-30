import asyncio
import re
import time

import requests
from aiogram import Bot, Dispatcher, types
from aiogram.contrib.fsm_storage.memory import MemoryStorage
from aiogram.dispatcher import FSMContext
from aiogram.utils import executor

import keyboards as kb
import storage as connect
from states import Form

API_TOKEN = '584143393:AAED-5U6R6XHoMtONSuLt_s2VE0v32TT_04'

loop = asyncio.get_event_loop()
bot = Bot(token=API_TOKEN, loop=loop)
storage = MemoryStorage()
dp = Dispatcher(bot, storage=storage)
connect_manager = connect.Storage()


@dp.message_handler(commands=['start'])
async def cmd_start(message: types.Message):
    await message.reply("Привет, с чего начнем?",
                        reply_markup=kb.inline_kb_start)
    await Form.idle.set()


@dp.callback_query_handler(lambda c: c.data == 'button_reg',
                           state=Form.idle)
async def process_callback_button1(callback_query: types.CallbackQuery):
    await bot.answer_callback_query(callback_query.id)
    await Form.login_reg.set()

    await bot.send_message(callback_query.from_user.id,
                           'Регистрация: введите логин')


@dp.message_handler(state=Form.login_reg)
async def process_login(message: types.Message, state: FSMContext):
    async with state.proxy() as data:
        data['login_reg'] = message.text

    await Form.password_reg.set()
    await message.reply("Регистрация: введите пароль")


@dp.message_handler(state=Form.password_reg)
async def process_password(message: types.Message, state: FSMContext):
    async with state.proxy() as data:
        data['password_reg'] = message.text

    await message.reply("Пробуем зарегистрироваться...")

    async with state.proxy() as data:
        login = data['login_reg']
        password = data['password_reg']
        API_ENDPOINT = "http://127.0.0.1:8000/polyana/api/registration/"
        credentials = {'nickname': login, 'password': password}
        r = requests.post(API_ENDPOINT, credentials)
        Answ = re.compile(r'{"([a-zA-z]*)')
        Answ = Answ.search(r.text)
        Answ = Answ.group(1)
        time.sleep(0.2)

        if Answ == 'response':
            time.sleep(0.2)

            await bot.send_message(message.chat.id,
                                   text="Успешно зарегистрирован!")

            if await authentication(message.chat.id, login, password):
                time.sleep(0.1)

                await message.reply("Успешно вошли!",
                                    reply_markup=kb.inline_kb_battle)

                await Form.authenticated_idle.set()
            else:
                await message.reply("Вход не удался!",
                                    reply_markup=kb.inline_kb_start)
                await Form.idle.set()
        else:
            await bot.send_message(
                message.chat.id,
                text=("Что-то пошло не так, скорее всего, пользователь с " +
                      "таким ником уже существует, попробуйте еще раз!"),
                reply_markup=kb.inline_kb_start)

            await Form.idle.set()


@dp.callback_query_handler(lambda c: c.data == 'button_auth',
                           state=Form.idle)
async def process_callback_button2(callback_query: types.CallbackQuery):
    await bot.answer_callback_query(callback_query.id)
    await Form.login_auth.set()
    await bot.send_message(callback_query.from_user.id, 'Вход: введите логин')


@dp.message_handler(state=Form.login_auth)
async def process_login_auth(message: types.Message, state: FSMContext):
    async with state.proxy() as data:
        data['login'] = message.text

    await Form.password_auth.set()
    await bot.send_message(message.chat.id, "Вход: введите пароль")


@dp.message_handler(state=Form.password_auth)
async def process_password_auth(message: types.Message, state: FSMContext):
    async with state.proxy() as data:
        data['password'] = message.text

    await Form.next()
    await message.reply("Входим...")

    async with state.proxy() as data:
        login = data['login']
        password = data['password']

        if await authentication(message.chat.id, login, password):
            time.sleep(0.1)

            await bot.send_message(message.chat.id,
                                   "Успешно вошли!",
                                   reply_markup=kb.inline_kb_battle)

            await Form.authenticated_idle.set()
        else:
            await bot.send_message(message.chat.id,
                                   "Неправильный логин или пароль!",
                                   reply_markup=kb.inline_kb_start)
            await Form.idle.set()


@dp.callback_query_handler(lambda c: c.data == 'button_battle',
                           state=Form.authenticated_idle)
async def process_callback_button_battle(callback_query: types.CallbackQuery,
                                         state: FSMContext):
    await bot.answer_callback_query(callback_query.id)
    await Form.reg_battle_bid.set()

    async with state.proxy() as data:
        data['battle_type'] = 'head_to_head'

    await bot.send_message(callback_query.from_user.id,
                           'Входим в игровую комнату...')

    await bot.send_message(callback_query.from_user.id,
                           'Введите вашу ставку')


@dp.callback_query_handler(lambda c: c.data == 'button_royale',
                           state=Form.authenticated_idle)
async def process_callback_button_royale(callback_query: types.CallbackQuery,
                                         state: FSMContext):
    await bot.answer_callback_query(callback_query.id)
    await Form.reg_battle_bid.set()

    async with state.proxy() as data:
        data['battle_type'] = 'battle_royale'

    await bot.send_message(callback_query.from_user.id,
                           'Входим в игровую комнату...')

    await bot.send_message(callback_query.from_user.id,
                           'Введите вашу ставку')


@dp.message_handler(lambda message: not message.text.isdigit(),
                    state=Form.reg_battle_bid)
async def failed_process_bid(message: types.Message):
    return await message.reply("Ставка должна быть числом!.\n" +
                               "Введите вашу ставку")


@dp.message_handler(lambda message: message.text.isdigit(),
                    state=Form.reg_battle_bid)
async def process_reg_battle_bid(message: types.Message, state: FSMContext):
    await Form.reg_battle_currency.set()
    await state.update_data(reg_battle_bid=int(message.text))

    await message.reply("В какой валюте ставим?",
                        reply_markup=kb.inline_kb_currency)


@dp.message_handler(state=Form.move)
async def process_battle_waiting(message: types.Message, state: FSMContext):
    await message.reply("Нужно сделать ход 👈👆👇👉")


@dp.message_handler(state=Form.reg_battle)
async def process_battle_waiting(message: types.Message, state: FSMContext):
    await message.reply("Ожидаем начало боя...")


@dp.callback_query_handler(lambda c: c.data == 'currency_silver',
                           state=Form.reg_battle_currency)
async def process_silver(callback_query, state: FSMContext):
    await Form.reg_battle.set()

    await bot.send_message(callback_query.from_user.id,
                           "Пробуем начать бой...")

    tn = connect_manager.get_connect(callback_query.from_user.id)
    time.sleep(0.1)

    async with state.proxy() as data:
        data['reg_battle_currency'] = 'SILVER'

        data['reg_battle'] = True
        bid = data['reg_battle_bid']
        currency = data['reg_battle_currency']

        if data['battle_type'] == 'battle_royale':
            tn.write(b'ROYALE ' + str(bid).encode('ascii') + b' ' +
                     currency.encode('ascii') + b'\r\n')
        else:  # head_to_head
            tn.write(b'BATTLE ' + str(bid).encode('ascii') + b' ' +
                     currency.encode('ascii') + b'\r\n')

    time.sleep(0.1)
    await check_oponent(callback_query.from_user.id)


@dp.callback_query_handler(lambda c: c.data == 'currency_gold',
                           state=Form.reg_battle_currency)
async def process_silver(callback_query, state: FSMContext):
    await Form.reg_battle.set()

    await bot.send_message(callback_query.from_user.id,
                           "Пробуем начать бой...")

    async with state.proxy() as data:
        data['reg_battle_currency'] = 'GOLD'

        data['reg_battle'] = True
        bid = data['reg_battle_bid']
        currency = data['reg_battle_currency']

    tn = connect_manager.get_connect(callback_query.from_user.id)
    time.sleep(0.1)

    tn.write(b'BATTLE ' + str(bid).encode('ascii') + b' ' +
             currency.encode('ascii') + b'\r\n')

    time.sleep(0.1)
    await check_oponent(callback_query.from_user.id)


async def authentication(chat_id, login, password):
    connect_manager.add_connect(chat_id)
    tn = connect_manager.get_connect(chat_id)
    time.sleep(0.2)

    tn.write(b'AUTH ' + login.encode('ascii') + b' ' +
             password.encode('ascii') + b'\r\n')

    time.sleep(0.2)
    answ1 = tn.read_eager()

    if answ1 == b'SUCCESS\n':
        return True
    else:
        return False


async def get_battle_map(answ1, id):
    tn = connect_manager.get_connect(id)
    answ = tn.read_eager()
    battle_map = answ1 + answ
    found_battle = True
    while found_battle:
        while answ != b'':
            answ = tn.read_eager()
            time.sleep(0.2)
            battle_map += answ
        else:
            if b'Congratulations!' in battle_map.split():
                await re_battle(id, battle_map)
                await Form.authenticated_idle.set()
            else:
                battle_map = render_battle_map(battle_map)

                if battle_map != '':
                    if (battle_map != 'NOT YOUR MOVE! WAIT PLEASE!') and \
                            (battle_map != 'ERROR, please try once more'):
                        await bot.send_message(id,
                                               battle_map,
                                               reply_markup=kb.inline_kb_move)

                        await Form.move.set()
                        await check_oponent(id)
                    else:
                        await bot.send_message(id,
                                               battle_map,
                                               reply_markup=kb.inline_kb_move)

                        await Form.move.set()

        await asyncio.sleep(0.2)
        found_battle = False


async def check_oponent(id):
    tn = connect_manager.get_connect(id)
    battle_map = b''
    while battle_map == b'' or \
            battle_map == b'Searching the opponent...\n' or \
            battle_map == b'TO BATTLE!\n':
        answ = tn.read_eager()
        battle_map = answ

        while answ != b'' and answ != b'Searching the opponent...\n':
            if answ == b'TO BATTLE!\n':
                await bot.send_message(id, "Вы находитесь в очереди, ожидайте")
                answ = tn.read_eager()
                time.sleep(0.2)
            elif answ == b'YOU HAVE NOT ENOUGH MONEY\n':
                await bot.send_message(id,
                                       "Недостаточно денег",
                                       reply_markup=kb.inline_kb_battle)

                await Form.authenticated_idle.set()
                return
            else:
                answ = tn.read_eager()
                time.sleep(0.2)
                battle_map += answ
        await asyncio.sleep(0.2)
    else:
        if b'Congratulations!' in battle_map.split():
            await re_battle(id, battle_map)
            await Form.authenticated_idle.set()
        else:
            await get_battle_map(battle_map, id)


def render_battle_map(battle_map):
    battle_map = battle_map.decode('ascii')
    battle_map = battle_map.replace(' A ', '😎')
    battle_map = battle_map.replace(' B ', '😈')
    battle_map = battle_map.replace(' C ', '😱')
    battle_map = battle_map.replace(' D ', '😸')
    battle_map = battle_map.replace('player A', 'player 😎')
    battle_map = battle_map.replace('player B', 'player 😈')
    battle_map = battle_map.replace('player C', 'player 😱')
    battle_map = battle_map.replace('player D', 'player 😸')
    battle_map = battle_map.replace('A wins', '😎 wins')
    battle_map = battle_map.replace('B wins', '😈 wins')
    battle_map = battle_map.replace('C wins', '😱 wins')
    battle_map = battle_map.replace('D wins', '😸 wins')
    battle_map = battle_map.replace(' X ', '💩')
    battle_map = battle_map.replace(' F ', '💩')
    battle_map = battle_map.replace('   ', '🌎')
    battle_map = battle_map.replace('|', '')
    battle_map = battle_map.replace('-', '')

    return battle_map


async def re_battle(id, battle_map):
    await Form.next()
    await bot.send_message(id,
                           render_battle_map(battle_map),
                           reply_markup=kb.inline_kb_battle)


@dp.callback_query_handler(lambda c: c.data == 'button_up', state=Form.move)
async def process_callback_button_up(callback_query: types.CallbackQuery):
    await bot.answer_callback_query(callback_query.id)
    tn = connect_manager.get_connect(callback_query.from_user.id)
    tn.write(b'u\r\n')
    time.sleep(0.2)
    await get_battle_map(b'', callback_query.from_user.id)


@dp.callback_query_handler(lambda c: c.data == 'button_left', state=Form.move)
async def process_callback_button_left(callback_query: types.CallbackQuery):
    await bot.answer_callback_query(callback_query.id)
    tn = connect_manager.get_connect(callback_query.from_user.id)
    tn.write(b'l\r\n')
    time.sleep(0.2)
    await get_battle_map(b'', callback_query.from_user.id)


@dp.callback_query_handler(lambda c: c.data == 'button_right', state=Form.move)
async def process_callback_button_right(callback_query: types.CallbackQuery):
    await bot.answer_callback_query(callback_query.id)
    tn = connect_manager.get_connect(callback_query.from_user.id)
    tn.write(b'r\r\n')
    time.sleep(0.2)
    await get_battle_map(b'', callback_query.from_user.id)


@dp.callback_query_handler(lambda c: c.data == 'button_down', state=Form.move)
async def process_callback_button_down(callback_query: types.CallbackQuery):
    await bot.answer_callback_query(callback_query.id)
    tn = connect_manager.get_connect(callback_query.from_user.id)
    tn.write(b'd\r\n')
    time.sleep(0.2)
    await get_battle_map(b'', callback_query.from_user.id)


@dp.callback_query_handler(lambda c: c.data == 'button_quit',
                           state='*')
async def process_callback_button_quit(callback_query: types.CallbackQuery):
    await bot.answer_callback_query(callback_query.id)

    await bot.send_message(callback_query.from_user.id,
                           "Чтобы вернуться, пришли мне что-нибудь.")

    tn = connect_manager.get_connect(callback_query.from_user.id)

    if tn:
        tn.close()

    await Form.idle.set()


@dp.callback_query_handler(state='*')
async def process_callback_button_quit(callback_query: types.CallbackQuery):
    await bot.answer_callback_query(callback_query.id)
    await bot.send_message(callback_query.from_user.id,
                           "Действие неактуально.")


@dp.message_handler(state=Form.authenticated_idle)
async def process_idle_typing(message: types.Message, state: FSMContext):
    await message.reply("Лец батл", reply_markup=kb.inline_kb_battle)


@dp.message_handler(state='*')
async def process_idle_typing(message: types.Message, state: FSMContext):
    await cmd_start(message)


if __name__ == '__main__':
    executor.start_polling(dp, loop=loop, skip_updates=True)
