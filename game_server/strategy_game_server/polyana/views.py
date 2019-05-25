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

from .forms import (
    ApiRegistrationForm,
    ApiLoginForm,
    ApiStatsForm,
    RegistrationForm
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

            player.user_id = user
            player.save()

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


class RegistrationView(View):
    def post(self, request):
        form = RegistrationForm(request.POST)
        if form.is_valid():
            user = form.save()

            player = Player()
            player.nickname = form.cleaned_data.get('username')
            player.password = md5(form.cleaned_data.get('password1').encode()).hexdigest()
            player.token = get_token()
            player.token_expiration = get_token_expiration()
            player.user_id = user
            player.save()
            event_save(player, 'registration')

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
            leaders = Player.objects.order_by('-winrate')[0:10]
        except Player.DoesNotExist as e:
            return HttpResponse('Error 404')

        return render(request, 'polyana/home.html', {'player': player, 'leaders': leaders})



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
