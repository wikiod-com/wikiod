---
title: "Django and Social Networks"
slug: "django-and-social-networks"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

## Parameters
| Setting | Does |
| ------ | ------ |
| Some Configurations | Handy basic settings that go with Django-Allauth (that I use most of the time). For more configuration options, see [Configurations][1]
| ACCOUNT_AUTHENTICATION_METHOD (=”username” or “email” or “username_email”)   |Specifies the login method to use – whether the user logs in by entering their username, e-mail address, or either one of both. Setting this to “email” requires ACCOUNT_EMAIL_REQUIRED=True
| ACCOUNT_EMAIL_CONFIRMATION_EXPIRE_DAYS (=3)   | Determines the expiration date of email confirmation mails (# of days).
|ACCOUNT_EMAIL_REQUIRED (=False) | The user is required to hand over an e-mail address when signing up. This goes in tandem with the `ACCOUNT_AUTHENTICATION_METHOD` setting
| ACCOUNT_EMAIL_VERIFICATION (=”optional”) | Determines the e-mail verification method during signup – choose one of "mandatory", "optional", or "none". When set to “mandatory” the user is blocked from logging in until the email address is verified. Choose “optional” or “none” to allow logins with an unverified e-mail address. In case of “optional”, the e-mail verification mail is still sent, whereas in case of “none” no e-mail verification mails are sent.
| ACCOUNT_LOGIN_ATTEMPTS_LIMIT (=5) | Number of failed login attempts. When this number is exceeded, the user is prohibited from logging in for the specified ACCOUNT_LOGIN_ATTEMPTS_TIMEOUT seconds. While this protects the allauth login view, it does not protect Django’s admin login from being brute forced.
| ACCOUNT_LOGOUT_ON_PASSWORD_CHANGE (=False) | Determines whether or not the user is automatically logged out after changing or setting their password. 
| SOCIALACCOUNT_PROVIDERS (= dict) | Dictionary containing provider specific settings.

  [1]: https://django-allauth.readthedocs.io/en/latest/configuration.html

## Easy way: python-social-auth

**python-social-auth is a framework that simplifies the social authentication and authorization mechanism. It contains many social backends (Facebook, Twitter, Github, LinkedIn, etc.)**


**INSTALL**

First we need to install the python-social-auth package with

    pip install python-social-auth

or [download][1] the code from github. Now is a good time to add this to your `requirements.txt` file.

**CONFIGURING settings.py**

In the settings.py add:

    INSTALLED_APPS = (
        ...
        'social.apps.django_app.default',
        ...
    )
**CONFIGURING BACKENDS**

AUTHENTICATION_BACKENDS contains the backends that we will use, and we only have to put what's we need.

    AUTHENTICATION_BACKENDS = (
        'social.backends.open_id.OpenIdAuth',
        'social.backends.google.GoogleOpenId',
        'social.backends.google.GoogleOAuth2',
        'social.backends.google.GoogleOAuth',
        'social.backends.twitter.TwitterOAuth',
        'social.backends.yahoo.YahooOpenId',
        ...
        'django.contrib.auth.backends.ModelBackend',
    )

Your project `settings.py` may not yet have an `AUTHENTICATION_BACKENDS` field. If that is the case add the field. Be sure not to miss `'django.contrib.auth.backends.ModelBackend',` as it handles login by username/password.

If we use for example Facebook and Linkedin Backends we need to add the API keys 

    SOCIAL_AUTH_FACEBOOK_KEY = 'YOURFACEBOOKKEY'
    SOCIAL_AUTH_FACEBOOK_SECRET = 'YOURFACEBOOKSECRET'
and 

    SOCIAL_AUTH_LINKEDIN_KEY = 'YOURLINKEDINKEY'
    SOCIAL_AUTH_LINKEDIN_SECRET = 'YOURLINKEDINSECRET'

**Note**: You can Obtain the nedded keys in [Facebook developers][2] and [Linkedin developers][3] and  [here][4] you can see the full list and his respective way to especify the API key and the key Secret. 

**Note on Secret Keys:** Secret keys should be kept secret. [Here][5] is a Stack Overflow explanation that is helpful. [This tutorial][6] is helpful for learning about enviromental variables.

TEMPLATE_CONTEXT_PROCESSORS will help to redirections, backends and other things, but at beginning we only need these:

    TEMPLATE_CONTEXT_PROCESSORS = (
        ...
        'social.apps.django_app.context_processors.backends',
        'social.apps.django_app.context_processors.login_redirect',
        ...
    )

In Django 1.8 setting up `TEMPLATE_CONTEXT_PREPROCESSORS` as shown above was deprecated. If this is the case for you you'll add it inside of the `TEMPLATES` dict. Yours should look something similar to this:

    TEMPLATES = [
        {
            'BACKEND': 'django.template.backends.django.DjangoTemplates',
            'DIRS': [os.path.join(BASE_DIR, "templates")],
            'APP_DIRS': True,
            'OPTIONS': {
                'context_processors': [
                    'django.template.context_processors.debug',
                    'django.template.context_processors.request',
                    'django.contrib.auth.context_processors.auth',
                    'django.contrib.messages.context_processors.messages',
                    'social.apps.django_app.context_processors.backends',
                    'social.apps.django_app.context_processors.login_redirect',
                ],
            },
        },
    ] 
    
**USING A CUSTOM USER**

If you are using a custom User Model and want to asociate with it, just add the following line (still in **settings.py**)

    SOCIAL_AUTH_USER_MODEL = 'somepackage.models.CustomUser'    

`CustomUser` is a model which inherit or Abstract from default User.

**CONFIGURING urls.py**

    # if you haven't imported inlcude make sure you do so at the top of your file
    from django.conf.urls import url, include

    urlpatterns = patterns('',
        ...
        url('', include('social.apps.django_app.urls', namespace='social'))
        ...
    )
        
Next need to sync database to create needed models:

    ./manage.py migrate

**Finally we can play!**

in some template you need to add something like this:

        <a href="{% url 'social:begin' 'facebook' %}?next={{ request.path }}">Login with Facebook</a>
        <a href="{% url 'social:begin' 'linkedin' %}?next={{ request.path }}">Login with Linkedin</a>

if you use another backend just change 'facebook' by the backend name.

**Logging users out**

Once you have logged users in you'll likely want to create the functionality to log them back out. In some template, likely near where the log in template was shown, add the following tag:

    <a href="{% url 'logout' %}">Logout</a>
or

    <a href="/logout">Logout</a>

You'll want to edit your `urls.py` file with code similar to:

    url(r'^logout/$', views.logout, name='logout'),

Lastly edit your views.py file with code similar to:

    def logout(request):
        auth_logout(request)
        return redirect('/')





  [1]: https://github.com/omab/python-social-auth
  [2]: http://developers.facebook.com/
  [3]: https://developer.linkedin.com/
  [4]: http://psa.matiasaguirre.net/docs/backends/index.html
  [5]: http://stackoverflow.com/questions/14786072/keep-secret-keys-out-with-environment-variables/14786138#14786138
  [6]: http://www.marinamele.com/taskbuster-django-tutorial/settings-different-environments-version-control

## Using Django Allauth
For all my projects, Django-Allauth remained one that is easy to setup, and comes out of the box with many features including but not limited to:

 - Some 50+ social networks authentications
 - Mix signup of both local and social accounts 
 - Multiple social accounts 
 - Optional instant-signup for social accounts – no questions asked
 - E-mail address management (multiple e-mail addresses, setting a
   primary) 
 - Password forgotten flow E-mail address verification flow

If you're interested in getting your hands dirty, Django-Allauth gets out of the way, with additional configurations to tweak the process and use of your authentication system.

The steps below assume you're using Django 1.10+

**Setup steps:**

`pip install django-allauth`

In your `settings.py` file, make the following changes:

    # Specify the context processors as follows:
    TEMPLATES = [
        {
            'BACKEND': 'django.template.backends.django.DjangoTemplates',
            'DIRS': [],
            'APP_DIRS': True,
            'OPTIONS': {
                'context_processors': [
                    # Already defined Django-related contexts here
    
                    # `allauth` needs this from django. It is there by default,
                    # unless you've devilishly taken it away.
                    'django.template.context_processors.request',
                ],
            },
        },
    ]
    
    AUTHENTICATION_BACKENDS = (
        # Needed to login by username in Django admin, regardless of `allauth`
        'django.contrib.auth.backends.ModelBackend',
    
        # `allauth` specific authentication methods, such as login by e-mail
        'allauth.account.auth_backends.AuthenticationBackend',
    )

    INSTALLED_APPS = (
    # Up here is all your default installed apps from Django

    # The following apps are required:
    'django.contrib.auth',
    'django.contrib.sites',

    'allauth',
    'allauth.account',
    'allauth.socialaccount',

    # include the providers you want to enable:
    'allauth.socialaccount.providers.google',
    'allauth.socialaccount.providers.facebook',
    )

    # Don't forget this little dude.
    SITE_ID = 1

Done with the changes in `settings.py` file above, move onto the `urls.py` file. It can be your `yourapp/urls.py` or your `ProjectName/urls.py`. Normally, I prefer the `ProjectName/urls.py`.

    urlpatterns = [
        # other urls here
        url(r'^accounts/', include('allauth.urls')),
        # other urls here
    ]

Simply adding the `include('allauth.urls')`, gives you these urls for free:

    ^accounts/ ^ ^signup/$ [name='account_signup']
    ^accounts/ ^ ^login/$ [name='account_login']
    ^accounts/ ^ ^logout/$ [name='account_logout']
    ^accounts/ ^ ^password/change/$ [name='account_change_password']
    ^accounts/ ^ ^password/set/$ [name='account_set_password']
    ^accounts/ ^ ^inactive/$ [name='account_inactive']
    ^accounts/ ^ ^email/$ [name='account_email']
    ^accounts/ ^ ^confirm-email/$ [name='account_email_verification_sent']
    ^accounts/ ^ ^confirm-email/(?P<key>[-:\w]+)/$ [name='account_confirm_email']
    ^accounts/ ^ ^password/reset/$ [name='account_reset_password']
    ^accounts/ ^ ^password/reset/done/$ [name='account_reset_password_done']
    ^accounts/ ^ ^password/reset/key/(?P<uidb36>[0-9A-Za-z]+)-(?P<key>.+)/$ [name='account_reset_password_from_key']
    ^accounts/ ^ ^password/reset/key/done/$ [name='account_reset_password_from_key_done']
    ^accounts/ ^social/
    ^accounts/ ^google/
    ^accounts/ ^twitter/
    ^accounts/ ^facebook/
    ^accounts/ ^facebook/login/token/$ [name='facebook_login_by_token']

Finally, do `python ./manage.py migrate` to commit the migrates of Django-allauth into Database.

As usual, to be able to log into your app using any social network you've added, you'll have to add the social account details of the network.

Login to the Django Admin (`localhost:8000/admin`) and under `Social Applications` in the  add your social account details.

You might need accounts at each auth provider in order to obtain details to fill in at the Social Applications sections.

For detailed configurations of what you can have and tweak, see the [Configurations page][1].


  [1]: https://django-allauth.readthedocs.io/en/latest/configuration.html

