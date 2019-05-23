from datetime import datetime, timezone
from django.http import HttpResponse
from django.contrib.auth.models import User
from django.views import View
from hashlib import md5
import json

from .forms import (
    ApiRegistrationForm,
    ApiLoginForm,
    ApiStatsForm
)
from .helpers.token import get_token, get_token_expiration
from .helpers.event import event_save
from .models import Player, Event


class ApiRegistrationView(View):

    def post(self, request):
        form = ApiRegistrationForm(request.POST)
        if form.is_valid():
            player = form.save(commit=False)
            player.password = md5(player.password.encode()).hexdigest() 
            player.token = get_token()
            player.token_expiration = get_token_expiration()
            player.save()

            user = User()
            user.username = player.nickname
            user.password = player.password
            user.save()
            event_save(player, 'registration')

            return HttpResponse(
                json.dumps({
                    "response": "ok",
                    "token": str(player.token),
                }),
                content_type="application/json",
                status=200
            )
        return HttpResponse(
            json.dumps({
                "errors": form.errors,
            }),
            content_type="application/json",
            status=400
        )

    def get(self, request):
        return HttpResponse(
            json.dumps({
                "response": "method_not_allowed",
            }),
            content_type="application/json",
            status=405
        )


class ApiLoginView(View):
    def post(self, request):

        form = ApiLoginForm(request.POST)

        if not form.is_valid():
            return HttpResponse(
                json.dumps({
                    "response": "invalid_credentials",
                    "error": form.errors
                }),
                content_type="application/json",
                status=403
            )

        try:
            player = Player.objects.get(
                nickname=form.cleaned_data['nickname'], 
                password=md5(form.cleaned_data['password'].encode()).hexdigest()
            )

        except Player.DoesNotExist:
            return HttpResponse(
                json.dumps({
                    "response": "invalid_credentials",
                }),
                content_type="application/json",
                status=403
            )

        if player.token_expiration < datetime.now(timezone.utc):
            player.token = get_token()

        player.token_expiration = get_token_expiration()
        player.save()
        event_save(player, 'login')

        return HttpResponse(
            json.dumps({
                "response": "ok",
                "token": str(player.token)
            }),
            content_type="application/json",
            status=200
        )

    def get(self, request):
        return HttpResponse(
            json.dumps({
                "response": "method_not_allowed",
            }),
            content_type="application/json",
            status=405
        )


class ApiStatsView(View):

    def post(self, request):
        return HttpResponse(
            json.dumps({
                "response": "method_not_allowed",
            }),
            content_type="application/json",
            status=405
        )

    def get(self, request):

        form = ApiStatsForm(request.GET)

        if not form.is_valid():
            return HttpResponse(
                json.dumps({
                    "response": "bad_request",
                    "error": form.errors
                }),
                content_type="application/json",
                status=400
            )

        try:
            player = Player.objects.get(
                nickname=form.cleaned_data['nickname'], 
                token=form.cleaned_data['token']
            )

        except Player.DoesNotExist:
            return HttpResponse(
                json.dumps({
                    "response": "invalid_credentials",
                }),
                content_type="application/json",
                status=403
            )

        if player.token_expiration > datetime.now(timezone.utc):
            player.token_expiration = get_token_expiration()
            player.save()
            return HttpResponse(
                json.dumps({
                    "response": "ok",
                    "nickname": player.nickname,
                    "played_battles": player.played_battles,
                    "battles_won": player.battles_won,
                    "winrate": player.winrate
                }),
                content_type="application/json",
                status=200
            )
        return HttpResponse(
            json.dumps({
                "response": "invalid_token",
            }),
            content_type="application/json",
            status=403
        )
