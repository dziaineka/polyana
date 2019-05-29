from polyana.models import Player
from hashlib import md5
from .token import get_token, get_token_expiration


def create_player(user, nickname, password):
    player = Player()
    player.nickname = nickname
    player.password = md5(password.encode()).hexdigest()
    player.token = get_token()
    player.token_expiration = get_token_expiration()
    player.user_id = user
    player.save()
    return player
