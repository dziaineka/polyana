from django.contrib.postgres.fields import JSONField, ArrayField
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

    player = models.ForeignKey(Player, on_delete=models.DO_NOTHING)
    event_type = models.CharField(max_length=200, null=False, blank=False, choices=EVENT_TYPE, db_column='type')
    source = models.IntegerField(null=True, blank=False)
    payload = JSONField(default=dict, null=True)
    created = models.DateTimeField(auto_now_add=True)


class Currency(models.Model):
    CURRENCY_TYPE = (
        ('GOLD', 'GOLD'),
        ('SILVER', 'SILVER'),
    )

    currency_type = models.CharField(max_length=200, null=False, blank=False, choices=CURRENCY_TYPE, db_column='type')
    rate = models.FloatField(null=False)


class Transaction(models.Model):
    event = models.ForeignKey(Event, on_delete=models.DO_NOTHING)
    player = models.ForeignKey(Player, on_delete=models.DO_NOTHING)
    currency = models.ForeignKey(Currency, on_delete=models.DO_NOTHING)
    amount = models.IntegerField(null=False) # TODO


class Achievement(models.Model):
    ACHIEVEMENT_TYPE = (
        ('5_battles', '5 battles'),
        ('5_wins', '5 wins'),
        ('first_win', 'First win'),
    )

    achievement_type = models.CharField(max_length=200, null=False, blank=False, choices=ACHIEVEMENT_TYPE, db_column='type')
    player = models.ForeignKey(Player, on_delete=models.CASCADE)
    created = models.DateTimeField(auto_now_add=True)


class Money(models.Model):
    player = models.ForeignKey(Player, on_delete=models.CASCADE)
    currency = models.ForeignKey(Currency, on_delete=models.CASCADE)
    amount = models.IntegerField(null=False) # TODO


class Battle(models.Model):
    currency = models.ForeignKey(Currency, on_delete=models.DO_NOTHING)
    bid = models.IntegerField(null=True)
    winner = models.ForeignKey(Player, null=True, on_delete=models.DO_NOTHING, related_name='winner')
    participants = ArrayField(models.IntegerField(), null=True)
    created = models.DateTimeField(auto_now_add=True)
