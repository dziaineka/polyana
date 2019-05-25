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
    # first_name = forms.CharField(max_length=30, required=False)
    # last_name = forms.CharField(max_length=30, required=False)
    # email = forms.EmailField(max_length=254)

    class Meta:
        model = User
        fields = ('username', 'password1', 'password2')

    # def __init__(self, *args, **kwargs):
    #     super().__init__(*args, **kwargs)
    #     for visible in self.visible_fields():
    #         visible.field.widget.attrs['class'] = 'form-control'
