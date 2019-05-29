from polyana.models import Transaction, Money, Currency, Money


def transaction_save(event, player, currency_id, amount):
    transaction = Transaction()
    transaction.event_id = event
    transaction.player_id = player
    transaction.currency_id = currency_id
    transaction.amount = amount
    transaction.save()


def money_save(player, currency_id, amount):
    try:
        money = Money.objects.get(player_id=player, currency_id=currency_id)
        money.amount += amount
    except Money.DoesNotExist:
        money = Money()
        money.amount = amount

        money.player_id = player
        money.currency_id = currency_id
    money.save()


def account_replenishment_after_registration(event, player):
    currency_g = Currency.objects.get(currency_type='GOLD')
    transaction_save(event, player, currency_g, 100)
    money_save(player, currency_g, 100)

    currency_s = Currency.objects.get(currency_type='SILVER')
    transaction_save(event, player, currency_s, 200)
    money_save(player, currency_s, 200)
