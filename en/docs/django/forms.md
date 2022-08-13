---
title: "Forms"
slug: "forms"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

## Defining a Django form from scratch (with widgets)
 Forms can be defined, in a similar manner to models, by subclassing `django.forms.Form`.  
Various field input options are available such as `CharField`, `URLField`, `IntegerField`, etc.

Defining a simple contact form can be seen below:

    from django import forms

    class ContactForm(forms.Form):
        contact_name = forms.CharField(
            label="Your name", required=True,
            widget=forms.TextInput(attrs={'class': 'form-control'}))
        contact_email = forms.EmailField(
            label="Your Email Address", required=True,
            widget=forms.TextInput(attrs={'class': 'form-control'}))
        content = forms.CharField(
            label="Your Message", required=True,
            widget=forms.Textarea(attrs={'class': 'form-control'}))
        
Widget is Django's representation of HTML user-input tags and can be used to render custom html for form fields (eg: as a text box is rendered for the content input here)

`attrs` are attributes that will be copied over as is to the rendered html for the form.

Eg:
    `content.render("name", "Your Name")` gives

    <input title="Your name" type="text" name="name" value="Your Name" class="form-control" />
    
        
    

## ModelForm Example
Create a ModelForm from an existing Model class, by subclassing `ModelForm`:

    from django import forms

    class OrderForm(forms.ModelForm):
        class Meta:
            model = Order
            fields = ['item', 'order_date', 'customer', 'status']

## File Uploads with Django Forms
First of all we need to add `MEDIA_ROOT` and `MEDIA_URL` to our `settings.py` file

    MEDIA_ROOT = os.path.join(BASE_DIR, 'media')
    MEDIA_URL = '/media/'

Also here you will work with `ImageField`, so remember in such cases install Pillow library (`pip install pillow`). Otherwise, you will have such error:

    ImportError: No module named PIL

Pillow is a fork of PIL, the Python Imaging Library, which is no longer maintained.  Pillow is backwards compatible with PIL.

Django comes with two form fields to upload files to the server, `FileField` and `ImageField`, the following is an example of using these two fields in our form

**forms.py:**

    from django import forms
    
    
    class UploadDocumentForm(forms.Form):
        file = forms.FileField()
        image = forms.ImageField()

**views.py:**

    from django.shortcuts import render
    from .forms import UploadDocumentForm
    
    
    def upload_doc(request):
        form = UploadDocumentForm()
        if request.method == 'POST':
            form = UploadDocumentForm(request.POST, request.FILES)  # Do not forget to add: request.FILES
            if form.is_valid():
                # Do something with our files or simply save them
                # if saved, our files would be located in media/ folder under the project's base folder
                form.save()
        return render(request, 'upload_doc.html', locals())

**upload_doc.html:**

    <html>
        <head>File Uploads</head>
        <body>
            <form enctype="multipart/form-data" action="" method="post"> <!-- Do not forget to add: enctype="multipart/form-data" -->
                {% csrf_token %}
                {{ form }}
                <input type="submit" value="Save">
            </form>
        </body>
    </html>

## Validation of fields and Commit to model (Change user e-mail)
There are already implemented forms within Django to change the user password, one example being [SetPasswordForm][1]. 

There aren't, however, forms to modify the user e-mail and I think the following example is important to understand how to use a form correctly.

The following example performs the following checks:

 - E-mail have in fact changed - very useful if you need to validate the e-mail or update mail chimp;
 - Both e-mail and confirmation e-mail are the same - the form has two fields for e-mail, so the update is less error prone.

And in the end, it saves the new e-mail in the user object (updates the user e-mail). Notice that the `__init__()` method requires a user object.

    class EmailChangeForm(forms.Form):
        """
        A form that lets a user change set their email while checking for a change in the 
        e-mail.
        """
        error_messages = {
            'email_mismatch': _("The two email addresses fields didn't match."),
            'not_changed': _("The email address is the same as the one already defined."),
        }

        new_email1 = forms.EmailField(
            label=_("New email address"),
            widget=forms.EmailInput,
        )

        new_email2 = forms.EmailField(
            label=_("New email address confirmation"),
            widget=forms.EmailInput,
        )

        def __init__(self, user, *args, **kwargs):
            self.user = user
            super(EmailChangeForm, self).__init__(*args, **kwargs)

        def clean_new_email1(self):
            old_email = self.user.email
            new_email1 = self.cleaned_data.get('new_email1')
            if new_email1 and old_email:
                if new_email1 == old_email:
                    raise forms.ValidationError(
                        self.error_messages['not_changed'],
                        code='not_changed',
                    )
            return new_email1

        def clean_new_email2(self):
            new_email1 = self.cleaned_data.get('new_email1')
            new_email2 = self.cleaned_data.get('new_email2')
            if new_email1 and new_email2:
                if new_email1 != new_email2:
                    raise forms.ValidationError(
                        self.error_messages['email_mismatch'],
                        code='email_mismatch',
                    )
            return new_email2

        def save(self, commit=True):
            email = self.cleaned_data["new_email1"]
            self.user.email = email
            if commit:
                self.user.save()
            return self.user



    def email_change(request):
        form = EmailChangeForm()
        if request.method=='POST':
            form = Email_Change_Form(user,request.POST)
            if form.is_valid():
                if request.user.is_authenticated:
                    if form.cleaned_data['email1']  == form.cleaned_data['email2']:
                        user = request.user
                        u = User.objects.get(username=user)
                        # get the proper user
                        u.email = form.cleaned_data['email1'] 
                        u.save()
                        return HttpResponseRedirect("/accounts/profile/")
        else:
            return render_to_response("email_change.html", {'form':form},
                                       context_instance=RequestContext(request))




  [1]: https://docs.djangoproject.com/en/dev/topics/auth/default/#django.contrib.auth.forms.SetPasswordForm

## Removing a modelForm's field based on condition from views.py
If we have a Model as following,
    
    from django.db import models
    from django.contrib.auth.models import User
  
    class UserModuleProfile(models.Model):
        user = models.OneToOneField(User)
        expired = models.DateTimeField()
        admin = models.BooleanField(default=False)
        employee_id = models.CharField(max_length=50)
        organisation_name = models.ForeignKey('Organizations', on_delete=models.PROTECT)
        country = models.CharField(max_length=100)
        position = models.CharField(max_length=100)

        def __str__(self):
            return self.user

And a model form which uses this model as following,
  
    from .models import UserModuleProfile, from django.contrib.auth.models import User
    from django import forms
    
    class UserProfileForm(forms.ModelForm):
        admin = forms.BooleanField(label="Make this User Admin",widget=forms.CheckboxInput(),required=False)
        employee_id = forms.CharField(label="Employee Id ")
        organisation_name = forms.ModelChoiceField(label='Organisation Name',required=True,queryset=Organizations.objects.all(),empty_label="Select an Organization")
        country = forms.CharField(label="Country")
        position = forms.CharField(label="Position")

        class Meta:
            model = UserModuleProfile
            fields = ('admin','employee_id','organisation_name','country','position',)

        def __init__(self, *args, **kwargs):
            admin_check = kwargs.pop('admin_check', False)
            super(UserProfileForm, self).__init__(*args, **kwargs)
            if not admin_check:
                del self.fields['admin']

Notice that below the Meta class in form I added a  __init__ function which we can  use while initializing the form from views.py to delete a form field (or some other actions). I will explain this later.

So This form can be used by for user registration purposes and we want all the fields defined in the Meta class of the form. But what if we want to use the same form when we edit the user but when we do we don't want to show the admin field of the form? 

We can simply send an additional argument when we initialize the form based on some logic and delete the admin field from backend.

    def edit_profile(request,user_id):
        context = RequestContext(request)
        user = get_object_or_404(User, id=user_id)
        profile = get_object_or_404(UserModuleProfile, user_id=user_id)
        admin_check = False
        if request.user.is_superuser:
            admin_check = True
        # If it's a HTTP POST, we're interested in processing form data.
        if request.method == 'POST':
            # Attempt to grab information from the raw form information.
            profile_form = UserProfileForm(data=request.POST,instance=profile,admin_check=admin_check)
            # If the form is valid...
            if profile_form.is_valid():
                form_bool = request.POST.get("admin", "xxx")
                if form_bool == "xxx":
                    form_bool_value = False
                else:
                    form_bool_value = True
                profile = profile_form.save(commit=False)
                profile.user = user
                profile.admin = form_bool_value
                profile.save()
                edited = True
            else:
                print profile_form.errors

        # Not a HTTP POST, so we render our form using ModelForm instance.
        # These forms will be blank, ready for user input.
        else:
            profile_form = UserProfileForm(instance = profile,admin_check=admin_check)

        return render_to_response(
                'usermodule/edit_user.html',
                {'id':user_id, 'profile_form': profile_form, 'edited': edited, 'user':user},
                context)

As you can see I have shown here a simple edit example using the form we created earlier. Notice when I initialized the form i passed an additional `admin_check` variable which contains either `True` or `False`.

    profile_form = UserProfileForm(instance = profile,admin_check=admin_check)

Now If you notice the form we wrote earlier you can see that in the __init__ we try to catch the `admin_check` param that we pass from here. If the value is False we simply delete the `admin` Field from the form and use it. And Since this is a model form admin field could not be null in the model we simply check if the form post had admin field in the form post, if not we set it to `False` in the view code in following code of the view.

    form_bool = request.POST.get("admin", "xxx")
    if form_bool == "xxx":
        form_bool_value = False
    else:
        form_bool_value = True

