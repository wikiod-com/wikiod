---
title: "Django Built-in forms"
slug: "django-built-in-forms"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Django is shipped with several views that require forms. These forms are, naturally, built-in. 
A good example are [Authentication Built-in forms][1].

This topic intends to bring documentation on how to work with these forms.


  [1]: https://docs.djangoproject.com/en/dev/topics/auth/default/#module-django.contrib.auth.forms

## Add custom CSS classes
Built-in forms are great but sometimes there is a need to customize them, adding new fields or simply changing CSS attributes.
 
This example is applicable to several use cases but here it is presented regarding [PasswordChangeForm][1] and its use in a [Bootstrap][2] website.

The solution is to create another Form that inerits ``PasswordChangeForm`` update the [Widget][3]:

    class PasswordChangeCustomForm(PasswordChangeForm):
        def __init__(self, user, *args, **kwargs):
            super(PasswordChangeCustomForm, self).__init__(user,*args, **kwargs)
            for field in self.fields:
                self.fields[field].widget.attrs['class'] = 'form-control'

If you only pretend to change certain fields you may do:

    class PasswordChangeCustomForm(PasswordChangeForm):
        def __init__(self, user, *args, **kwargs):
            super(PasswordChangeCustomForm, self).__init__(user, *args, **kwargs)
            self.fields['old_password'].widget.attrs.update({'class': 'form-control'})
            self.fields['new_password1'].widget.attrs.update({'class': 'form-control'})
            self.fields['new_password2'].widget.attrs.update({'class': 'form-control'})

Note: all bootstrap forms require the class ``form-control`` to keep the website look and feel.

  [1]: https://docs.djangoproject.com/en/dev/topics/auth/default/#django.contrib.auth.forms.PasswordChangeForm
  [2]: http://getbootstrap.com/
  [3]: https://docs.djangoproject.com/en/dev/ref/forms/widgets/

