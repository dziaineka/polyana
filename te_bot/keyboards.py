from aiogram.types import InlineKeyboardMarkup, InlineKeyboardButton

inline_btn_reg = InlineKeyboardButton('Регистрация!',
                                      callback_data='button_reg')

inline_btn_auth = InlineKeyboardButton('Аутентификация!',
                                       callback_data='button_auth')

inline_btn_battle = InlineKeyboardButton('1 на 1',
                                         callback_data='button_battle')

inline_btn_royale = InlineKeyboardButton('4 игрока',
                                         callback_data='button_royale')

inline_btn_quit = InlineKeyboardButton('Выйти', callback_data='button_quit')

# Move button
inline_btn_up = InlineKeyboardButton('👆', callback_data='button_up')
inline_btn_down = InlineKeyboardButton('👇', callback_data='button_down')
inline_btn_left = InlineKeyboardButton('👈', callback_data='button_left')
inline_btn_right = InlineKeyboardButton('👉', callback_data='button_right')


inline_kb_start = InlineKeyboardMarkup().add(inline_btn_reg, inline_btn_auth)
inline_kb_re_auth = InlineKeyboardMarkup(row_width=2).add(inline_btn_auth)
inline_kb_quite = InlineKeyboardMarkup(row_width=1).add(inline_btn_quit)

inline_kb_battle = InlineKeyboardMarkup(row_width=2).add(inline_btn_battle,
                                                         inline_btn_royale,
                                                         inline_btn_quit)

inline_kb_move = InlineKeyboardMarkup(row_width=4).add(inline_btn_left,
                                                       inline_btn_up,
                                                       inline_btn_down,
                                                       inline_btn_right,
                                                       inline_btn_quit)

inline_btn_silver = InlineKeyboardButton('Серебро',
                                         callback_data='currency_silver')

inline_btn_gold = InlineKeyboardButton('Золото', callback_data='currency_gold')

inline_kb_currency = InlineKeyboardMarkup(row_width=2).add(inline_btn_silver,
                                                           inline_btn_gold)
