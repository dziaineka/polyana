from aiogram.types import ReplyKeyboardRemove, \
    ReplyKeyboardMarkup, KeyboardButton, \
    InlineKeyboardMarkup, InlineKeyboardButton

inline_btn_reg = InlineKeyboardButton('Регистрация!', callback_data='button_reg')
inline_btn_auth = InlineKeyboardButton('Аунтефикация!', callback_data='button_auth')
inline_btn_battle = InlineKeyboardButton('В БОЙ!', callback_data='button_battle')
inline_btn_quit = InlineKeyboardButton('Выйти', callback_data='button_quit')
##Move button
inline_btn_up = InlineKeyboardButton('Вверх', callback_data='button_up')
inline_btn_down = InlineKeyboardButton('Вниз', callback_data='button_down')
inline_btn_left = InlineKeyboardButton('Влево', callback_data='button_left')
inline_btn_right = InlineKeyboardButton('Вправо', callback_data='button_right')


inline_kb_start = InlineKeyboardMarkup().add(inline_btn_reg,inline_btn_auth)
inline_kb_re_auth = InlineKeyboardMarkup(row_width=2).add(inline_btn_auth)
inline_kb_battle = InlineKeyboardMarkup(row_width=1).add(inline_btn_battle,inline_btn_quit)
inline_kb_move = InlineKeyboardMarkup(row_width=2).add(inline_btn_up,inline_btn_down,inline_btn_left,inline_btn_right)
