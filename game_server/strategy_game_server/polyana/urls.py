from django.urls import path
from . import views
from django.views.decorators.csrf import csrf_exempt


urlpatterns = [
    path('api/registration/', csrf_exempt(views.ApiRegistrationView.as_view())),
    path('api/login/', csrf_exempt(views.ApiLoginView.as_view())),
    path('api/stats/', views.ApiStatsView.as_view()),

    # path('registration/', views.RegistrationView.as_view()),
]
