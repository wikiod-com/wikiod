---
title: "Extending or Substituting User Model"
slug: "extending-or-substituting-user-model"
draft: false
images: []
weight: 9928
type: docs
toc: true
---

## Custom user model with email as primary login field.
models.py :

    from __future__ import unicode_literals
    from django.db import models
    from django.contrib.auth.models import (
            AbstractBaseUser, BaseUserManager, PermissionsMixin)
    from django.utils import timezone
    from django.utils.translation import ugettext_lazy as _

    
    class UserManager(BaseUserManager):
        def _create_user(self, email,password, is_staff, is_superuser, **extra_fields):
            now = timezone.now()
            if not email:
                raise ValueError('users must have an email address')
            email = self.normalize_email(email)
            user = self.model(email = email,
                                is_staff = is_staff,
                                is_superuser = is_superuser,
                                last_login = now,
                                date_joined = now,
                                **extra_fields)
            user.set_password(password)
            user.save(using = self._db)
            return user

        def create_user(self, email, password=None, **extra_fields):
            user = self._create_user(email, password, False, False, **extra_fields)
            return user

        def create_superuser(self, email, password, **extra_fields):
            user = self._create_user(email, password, True, True, **extra_fields)
            return user

    class User(AbstractBaseUser,PermissionsMixin):
        """My own custom user class"""

        email = models.EmailField(max_length=255, unique=True, db_index=True, verbose_name=_('email address'))
        date_joined = models.DateTimeField(auto_now_add=True)
        is_active = models.BooleanField(default=True)
        is_staff = models.BooleanField(default=False)

        objects = UserManager()

        USERNAME_FIELD = 'email'
        REQUIRED_FIELDS = []

        class Meta:
            verbose_name = _('user')
            verbose_name_plural = _('users')

        def get_full_name(self):
        """Return the email."""
            return self.email

        def get_short_name(self):
        """Return the email."""
            return self.email

forms.py : 

    from django import forms
    from django.contrib.auth.forms import UserCreationForm
    from .models import User
    

    class RegistrationForm(UserCreationForm):
        email = forms.EmailField(widget=forms.TextInput(
            attrs={'class': 'form-control','type':'text','name': 'email'}),
            label="Email")
        password1 = forms.CharField(widget=forms.PasswordInput(
            attrs={'class':'form-control','type':'password', 'name':'password1'}),
            label="Password")
        password2 = forms.CharField(widget=forms.PasswordInput(
            attrs={'class':'form-control','type':'password', 'name': 'password2'}),
            label="Password (again)")

        '''added attributes so as to customise for styling, like bootstrap'''
        class Meta:
            model = User
            fields = ['email','password1','password2']
            field_order = ['email','password1','password2']

        def clean(self):
        """
        Verifies that the values entered into the password fields match
        NOTE : errors here will appear in 'non_field_errors()'
        """
            cleaned_data = super(RegistrationForm, self).clean()
            if 'password1' in self.cleaned_data and 'password2' in self.cleaned_data:
                if self.cleaned_data['password1'] != self.cleaned_data['password2']:
                    raise forms.ValidationError("Passwords don't match. Please try again!")
            return self.cleaned_data

        def save(self, commit=True):
            user = super(RegistrationForm,self).save(commit=False)
            user.set_password(self.cleaned_data['password1'])
            if commit:
                user.save()
            return user

    #The save(commit=False) tells Django to save the new record, but dont commit it to the database yet

    class AuthenticationForm(forms.Form): # Note: forms.Form NOT forms.ModelForm
        email = forms.EmailField(widget=forms.TextInput(
            attrs={'class': 'form-control','type':'text','name': 'email','placeholder':'Email'}), 
            label='Email')
        password = forms.CharField(widget=forms.PasswordInput(
            attrs={'class':'form-control','type':'password', 'name': 'password','placeholder':'Password'}),
            label='Password')

        class Meta:
            fields = ['email', 'password']

views.py :

    from django.shortcuts import redirect, render, HttpResponse
    from django.contrib.auth import login as django_login, logout as django_logout, authenticate as django_authenticate
    #importing as such so that it doesn't create a confusion with our methods and django's default methods

    from django.contrib.auth.decorators import login_required
    from .forms import AuthenticationForm, RegistrationForm


    def login(request):
        if request.method == 'POST':
            form = AuthenticationForm(data = request.POST)
            if form.is_valid():
                email = request.POST['email']
                password = request.POST['password']
                user = django_authenticate(email=email, password=password)
                if user is not None:
                    if user.is_active:
                        django_login(request,user)
                        return redirect('/dashboard') #user is redirected to dashboard
        else:
            form = AuthenticationForm()

        return render(request,'login.html',{'form':form,})

    def register(request):
        if request.method == 'POST':
            form = RegistrationForm(data = request.POST)
            if form.is_valid():
                user = form.save()
                u = django_authenticate(user.email = user, user.password = password)
                django_login(request,u)
                return redirect('/dashboard')
        else:
            form = RegistrationForm()

        return render(request,'register.html',{'form':form,})

    def logout(request):
        django_logout(request)
        return redirect('/')

    @login_required(login_url ="/")
    def dashboard(request):
        return render(request, 'dashboard.html',{})

settings.py :

    AUTH_USER_MODEL = 'myapp.User'

admin.py

    from django.contrib import admin
    from django.contrib.auth.admin import UserAdmin as BaseUserAdmin
    from django.contrib.auth.models import Group
    from .models import User


    class UserAdmin(BaseUserAdmin):
        list_display = ('email','is_staff')
        list_filter = ('is_staff',)
        fieldsets = ((None, 
                      {'fields':('email','password')}), ('Permissions',{'fields':('is_staff',)}),)
        add_fieldsets = ((None, {'classes': ('wide',), 'fields': ('email', 'password1', 'password2')}),)
        search_fields =('email',)
        ordering = ('email',)
        filter_horizontal = ()

    admin.site.register(User, UserAdmin)
    admin.site.unregister(Group)







## Use the `email` as username and get rid of the `username` field
If you want to get rid of the `username` field and use `email` as unique user identifier, you will have to create a custom `User` model extending `AbstractBaseUser` instead of `AbstractUser`. Indeed, `username` and `email` are defined in `AbstractUser` and you can't override them. This means you will also have to redefine all fields you want that are defined in `AbstractUser`.

    from django.contrib.auth.models import (
        AbstractBaseUser, PermissionsMixin, BaseUserManager,
    )
    from django.db import models
    from django.utils import timezone
    from django.utils.translation import ugettext_lazy as _

    class UserManager(BaseUserManager):

        use_in_migrations = True

        def _create_user(self, email, password, **extra_fields):
            if not email:
                raise ValueError('The given email must be set')
            email = self.normalize_email(email)
            user = self.model(email=email, **extra_fields)
            user.set_password(password)
            user.save(using=self._db)
            return user

        def create_user(self, email, password=None, **extra_fields):
            extra_fields.setdefault('is_staff', False)
            extra_fields.setdefault('is_superuser', False)
            return self._create_user(email, password, **extra_fields)

        def create_superuser(self, email, password, **extra_fields):
            extra_fields.setdefault('is_staff', True)
            extra_fields.setdefault('is_superuser', True)

            if extra_fields.get('is_staff') is not True:
                raise ValueError('Superuser must have is_staff=True.')
            if extra_fields.get('is_superuser') is not True:
                raise ValueError('Superuser must have is_superuser=True.')

        return self._create_user(email, password, **extra_fields)


    class User(AbstractBaseUser, PermissionsMixin):
        """PermissionsMixin contains the following fields:
            - `is_superuser`
            - `groups`
            - `user_permissions`
         You can omit this mix-in if you don't want to use permissions or
         if you want to implement your own permissions logic.
         """

        class Meta:
            verbose_name = _("user")
            verbose_name_plural = _("users")
            db_table = 'auth_user'
            # `db_table` is only needed if you move from the existing default
            # User model to a custom one. This enables to keep the existing data.

        USERNAME_FIELD = 'email'
        """Use the email as unique username."""

        REQUIRED_FIELDS = ['first_name', 'last_name']

        GENDER_MALE = 'M'
        GENDER_FEMALE = 'F'
        GENDER_CHOICES = [
            (GENDER_MALE, _("Male")),
            (GENDER_FEMALE, _("Female")),
        ]

        email = models.EmailField(
            verbose_name=_("email address"), unique=True,
            error_messages={
                'unique': _(
                    "A user is already registered with this email address"),
            },
        )
        gender = models.CharField(
            max_length=1, blank=True, choices=GENDER_CHOICES,
            verbose_name=_("gender"),
        )
        first_name = models.CharField(
            max_length=30, verbose_name=_("first name"),
        )
        last_name = models.CharField(
            max_length=30, verbose_name=_("last name"),
        )
        is_staff = models.BooleanField(
            verbose_name=_("staff status"),
            default=False,
            help_text=_(
                "Designates whether the user can log into this admin site."
            ),
        )
        is_active = models.BooleanField(
            verbose_name=_("active"),
            default=True,
            help_text=_(
                "Designates whether this user should be treated as active. "
                "Unselect this instead of deleting accounts."
            ),
        )
        date_joined = models.DateTimeField(
            verbose_name=_("date joined"), default=timezone.now,
        )

        objects = UserManager()

## Extend Django User Model Easily
**Our `UserProfile` class**

Create a `UserProfile` model class with the relationship of `OneToOne` to the default `User` model:

    from django.db import models
    from django.contrib.auth.models import User
    from django.db.models.signals import post_save
    
    class UserProfile(models.Model):
        user = models.OneToOneField(User, related_name='user')
        photo = FileField(verbose_name=_("Profile Picture"),
                          upload_to=upload_to("main.UserProfile.photo", "profiles"),
                          format="Image", max_length=255, null=True, blank=True)
        website = models.URLField(default='', blank=True)
        bio = models.TextField(default='', blank=True)
        phone = models.CharField(max_length=20, blank=True, default='')
        city = models.CharField(max_length=100, default='', blank=True)
        country = models.CharField(max_length=100, default='', blank=True)
        organization = models.CharField(max_length=100, default='', blank=True)

**Django Signals at work**

Using Django Signals, create a new `UserProfile` immediately a `User` object is created. This function can be tucked beneath the `UserProfile` model class in the same file, or place it wherever you like. I don't care, as along as you reference it properly.

    def create_profile(sender, **kwargs):
        user = kwargs["instance"]
        if kwargs["created"]:
            user_profile = UserProfile(user=user)
            user_profile.save()
    post_save.connect(create_profile, sender=User)

**`inlineformset_factory` to the rescue**

Now for your `views.py`, you might have something like this:

    from django.shortcuts import render, HttpResponseRedirect
    from django.contrib.auth.decorators import login_required
    from django.contrib.auth.models import User
    from .models import UserProfile
    from .forms import UserForm
    from django.forms.models import inlineformset_factory
    from django.core.exceptions import PermissionDenied
    @login_required() # only logged in users should access this
    def edit_user(request, pk):
        # querying the User object with pk from url
        user = User.objects.get(pk=pk)
    
        # prepopulate UserProfileForm with retrieved user values from above.
        user_form = UserForm(instance=user)
    
        # The sorcery begins from here, see explanation https://blog.khophi.co/extending-django-user-model-userprofile-like-a-pro/
        ProfileInlineFormset = inlineformset_factory(User, UserProfile, fields=('website', 'bio', 'phone', 'city', 'country', 'organization'))
        formset = ProfileInlineFormset(instance=user)
    
        if request.user.is_authenticated() and request.user.id == user.id:
            if request.method == "POST":
                user_form = UserForm(request.POST, request.FILES, instance=user)
                formset = ProfileInlineFormset(request.POST, request.FILES, instance=user)
    
                if user_form.is_valid():
                    created_user = user_form.save(commit=False)
                    formset = ProfileInlineFormset(request.POST, request.FILES, instance=created_user)
    
                    if formset.is_valid():
                        created_user.save()
                        formset.save()
                        return HttpResponseRedirect('/accounts/profile/')
    
            return render(request, "account/account_update.html", {
                "noodle": pk,
                "noodle_form": user_form,
                "formset": formset,
            })
        else:
            raise PermissionDenied

**Our Template**

Then spit everything to your template `account_update.html` as so:


    {% load material_form %}
    <!-- Material form is just a materialize thing for django forms -->
    <div class="col s12 m8 offset-m2">
          <div class="card">
            <div class="card-content">
            <h2 class="flow-text">Update your information</h2>
              <form action="." method="POST" class="padding">
                {% csrf_token %} {{ noodle_form.as_p }}
                <div class="divider"></div>
                {{ formset.management_form }}
                    {{ formset.as_p }}
                <button type="submit" class="btn-floating btn-large waves-light waves-effect"><i class="large material-icons">done</i></button>
                <a href="#" onclick="window.history.back(); return false;" title="Cancel" class="btn-floating waves-effect waves-light red"><i class="material-icons">history</i></a>
     
            </form>
            </div>
        </div>
    </div>

Above snippet taken from [Extending Django UserProfile like a Pro][1]


  [1]: https://blog.khophi.co/extending-django-user-model-userprofile-like-a-pro/

## Specifing a custom User model


## Referencing the User model
Your code will not work in projects where you reference the `User` model (*and where the `AUTH_USER_MODEL` setting has been changed*) directly. 

For example: if you want to create `Post` model for a blog with a customized `User` model, you should specify the custom `User` model like this:

    from django.conf import settings
    from django.db import models
    
    class Post(models.Model):
        author = models.ForeignKey(settings.AUTH_USER_MODEL, on_delete=models.CASCADE)

