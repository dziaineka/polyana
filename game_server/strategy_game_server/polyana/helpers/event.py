from polyana.models import Event


def event_save(player, event_type):
    event = Event()
    event.player = player
    event.event_type = event_type
    event.save()
    return event
