from django import forms
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
