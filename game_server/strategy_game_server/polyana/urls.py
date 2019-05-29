from django.urls import path
from .views import (
    ApiRegistrationView,
    ApiLoginView,
    ApiStatsView,
    RegistrationView,
    HomePageView,
    MyLoginView,
    MyLogoutView,
    ShopView,
    PaymentView,
)
from django.views.decorators.csrf import csrf_exempt


urlpatterns = [

    path('api/registration/', csrf_exempt(ApiRegistrationView.as_view())),
    path('api/login/', csrf_exempt(ApiLoginView.as_view())),
    path('api/stats/', ApiStatsView.as_view()),

    path('registration/', RegistrationView.as_view(), name='registration'),
    path('login/', MyLoginView.as_view(), name='login'),
    path('logout/', MyLogoutView.as_view(), name='logout'),

    path('homepage/', HomePageView.as_view(), name='home'),
    path('shop/', ShopView.as_view(), name='shop'),
    path('payment/<key>/', PaymentView.as_view(), name='payment'),

]
