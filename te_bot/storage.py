import telnetlib
import time


class Storage:
    def __init__(self):
        self.connect = {}

    def add_connect(self, user_id):
        tn = telnetlib.Telnet("localhost", 1234)
        time.sleep(0.1)
        self.connect[user_id] = tn

    def get_connect(self, user_id):
        try:
            return self.connect[user_id]
        except KeyError:
            return None
