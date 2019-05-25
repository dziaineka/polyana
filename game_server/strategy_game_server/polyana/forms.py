from django import forms
from django.contrib.auth.forms import UserCreationForm
from django.contrib.auth.models import User
from .models import Player


class ApiRegistrationForm(forms.ModelForm):
    class Meta:
        model = Player
        fields = ('nickname', 'password')


class ApiLoginForm(forms.Form):
    nickname = forms.fields.CharField()
    password = forms.fields.CharField()


class ApiStatsForm(forms.Form):
    nickname = forms.fields.CharField()
    token = forms.fields.CharField()


class RegistrationForm(UserCreationForm):
    class Meta:
        model = User
        fields = ('username', 'password1', 'password2')
