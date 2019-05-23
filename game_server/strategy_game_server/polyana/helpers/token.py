from uuid import uuid4
from datetime import datetime, timedelta


def get_token():
    return uuid4()


def get_token_expiration():
    return datetime.now() + timedelta(hours=2)
