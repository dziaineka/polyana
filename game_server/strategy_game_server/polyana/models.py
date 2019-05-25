from django.contrib.postgres.fields import JSONField
from django.db import models
from django.contrib.auth.models import User


class Player(models.Model): 
    user_id = models.ForeignKey(User, on_delete=models.CASCADE, default=None)
    nickname = models.CharField(max_length=200, null=False, blank=False, unique=True)
    password = models.CharField(max_length=200, null=False, blank=False)
    token = models.CharField(max_length=200, null=True, blank=True, unique=True)
    token_expiration = models.DateTimeField(null=True, blank=True)
    played_battles = models.IntegerField(null=False, default=0)
    battles_won = models.IntegerField(null=False, default=0)
    winrate = models.FloatField(null=False, default=0)

    def __str__(self):
        return self.nickname


class Event(models.Model):
    EVENT_TYPE = (
        ('registration', 'registration'),
        ('battle_start', 'battle_start'),
        ('battle_end', 'battle_end'),
        ('money_buying', 'money_buying'),
        ('achievement', 'achievement'),
        ('login', 'login'),
        ('logout', 'logout'),
    )

    player_id = models.ForeignKey(Player, on_delete=models.DO_NOTHING)
    event_type = models.CharField(max_length=200, null=False, blank=False, choices=EVENT_TYPE, db_column='type')
    source = models.IntegerField(null=True, blank=False)
    payload = JSONField(default=dict)
    created = models.DateTimeField(auto_now_add=True)
