from datetime import datetime, timezone
from django.http import HttpResponse, HttpResponseRedirect
from django.contrib.auth import login, authenticate
from django.contrib.auth.decorators import login_required
from django.contrib.auth.views import LoginView, LogoutView
from django.contrib.auth.models import User
from django.views import View
from django.shortcuts import render, redirect
from django.utils.decorators import method_decorator

from hashlib import md5
import json

from .constants import GOLD, SILVER
from .forms import (
    ApiRegistrationForm,
    ApiLoginForm,
    ApiStatsForm,
    RegistrationForm,
    PaymentForm,
)

from .helpers.player import create_player
from .helpers.token import get_token, get_token_expiration
from .helpers.transaction_money import (
    transaction_save, 
    money_save, 
    account_replenishment_after_registration
    )

from .helpers.event import event_save
from .models import Player, Event, Transaction, Money, Currency


class ApiRegistrationView(View):

    def post(self, request):
        form = ApiRegistrationForm(request.POST)
        if form.is_valid():

            user = User()
            user.save()
            player = create_player(user, form.cleaned_data['nickname'], form.cleaned_data['password'])
            user.username = player.nickname
            user.set_password(form.cleaned_data['password'])
            user.save()

            event = event_save(player, 'registration')

            account_replenishment_after_registration(event, player)

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


class RegistrationView(View):
    def post(self, request):
        form = RegistrationForm(request.POST)
        if form.is_valid():
            user = form.save()
            player = create_player(user, form.cleaned_data.get('username'), form.cleaned_data.get('password1'))
            event = event_save(player, 'registration')
            account_replenishment_after_registration(event, player)

            raw_password = form.cleaned_data.get('password1')
            user = authenticate(username=user.username, password=raw_password)
            login(request, user)
            return HttpResponseRedirect('../homepage')
        return render(request, 'registration/signup.html', {'form': form})

    def get(self, request):
        form = RegistrationForm()
        return render(request, 'registration/signup.html', {'form': form})


class HomePageView(View):
    @method_decorator(login_required)
    def dispatch(self, request, *args, **kwargs):
        return super(HomePageView, self).dispatch(request, *args, **kwargs)

    def get(self, request):
        try:
            player = Player.objects.get(user_id=self.request.user)
            leaders = Player.objects.all().order_by('-winrate')[0:10]
            money = Money.objects.filter(player_id=player)
        except Player.DoesNotExist as e:
            return HttpResponse('Error 404')

        return render(request, 'polyana/home.html', {'player': player, 'leaders': leaders, 'money': money})



class MyLoginView(LoginView):
    def form_valid(self, form):
        response = super().form_valid(form)

        player = Player.objects.get(user_id=self.request.user)

        if player.token_expiration < datetime.now(timezone.utc):
            player.token = get_token()

        player.token_expiration = get_token_expiration()
        player.save()

        event_save(player, 'login')

        return response


class MyLogoutView(LogoutView):
    def dispatch(self, request, *args, **kwargs):
        event_save(Player.objects.get(user_id=self.request.user), 'logout')
        response = super().dispatch(request, *args, **kwargs)
        return response


class ShopView(View):

    def get(self, request):
        return render(request, 'polyana/shop.html', {'gold': GOLD, 'silver': SILVER})


class PaymentView(View):

    def validate_product_id_param(self, product_id):
        if product_id in GOLD:
            game_currency, money_amount = GOLD[product_id]
            return {'currency': 'GOLD', 'game_currency': game_currency}
        elif product_id in SILVER:
            game_currency, money_amount = SILVER[product_id]
            return {'currency': 'SILVER', 'game_currency': game_currency}
        else:
            return None

    def post(self, request, key):
        data = request.POST.copy()
        data.update({'key_currency': key})

        form = PaymentForm(data)
        if form.is_valid():
            product = self.validate_product_id_param(data['key_currency'])
            game_currency = product['game_currency']
            if product is None:
                return render(request, 'polyana/temp_errors/404.html', context={'error': 'No such product'}, status=400)

            player = Player.objects.get(user_id=self.request.user)
            event = event_save(player, 'money_buying')
            currency = Currency.objects.get(currency_type=product['currency'])
            transaction_save(event, player, currency, game_currency)
            money_save(player, currency, game_currency)

            return redirect('home')

        return render(request, 'polyana/payment.html', {'form': form})

    def get(self, request, key):
        form = PaymentForm()
        return render(request, 'polyana/payment.html', {'form': form})
