import datetime
from django import forms
from django.contrib.auth.forms import UserCreationForm
from django.contrib.auth.models import User
from .models import Player

YEAR_BEGIN = datetime.datetime.now().year
YEAR_LAST = YEAR_BEGIN + 10


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



class PaymentForm(forms.Form):

    card_number = forms.fields.IntegerField()
    validity_month = forms.fields.ChoiceField(choices=[[x, x] for x in range(1, 13)])
    validity_year = forms.fields.ChoiceField(choices=[[x, x] for x in range(YEAR_BEGIN, YEAR_LAST)])
    cvv = forms.fields.IntegerField()
    key_currency = forms.fields.CharField(initial='0')

    def clean(self):
        cleaned_data = super().clean()
        valid_month = int(cleaned_data.get('validity_month'))
        valid_year = int(cleaned_data.get('validity_year'))

        today = datetime.datetime.now()
        month = today.month
        year = today.year

        if year > valid_year:
            raise forms.ValidationError('Invalid expiration date')
        if year == valid_year and month > valid_month:
            raise forms.ValidationError('Invalid expiration date')
        return cleaned_data
