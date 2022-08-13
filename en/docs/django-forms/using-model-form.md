---
title: "Using Model Form"
slug: "using-model-form"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

Django [ModelForm][1] enables the creation of a Form class from a Django model.



  [1]: https://docs.djangoproject.com/en/1.10/topics/forms/modelforms/#modelform

## Making fields not editable
Django ``1.9`` added the [Field.disabled][1] attribute:

> The disabled boolean argument, when set to True, disables a form field
> using the disabled HTML attribute so that it won’t be editable by
> users. Even if a user tampers with the field’s value submitted to the
> server, it will be ignored in favor of the value from the form’s
> initial data.

And so you only need to do: 

    MyChangeForm(ModelForm): 

        def __init__(self, *args, **kwargs): 
            super(MyChangeForm, self).__init__(*args, **kwargs)                       
            self.fields['<field_to_disable>'].disabled = True

And creating the form you need:

    MyChangeForm(initial={'<field_to_disable>': "something"})

Before version ``1.9`` you had to:

    class MyChangeForm(ModelForm):
        def __init__(self, *args, **kwargs):
            super(ItemForm, self).__init__(*args, **kwargs)
            instance = getattr(self, 'instance', None)
        if instance and instance.id:
            self.fields['<field_to_disable>'].required = False
            self.fields['<field_to_disable>'].widget.attrs['disabled'] = True

    def clean_<field_to_disable>(self):
        # As shown in the above answer.
        instance = getattr(self, 'instance', None)
        if instance:
            return instance.<field_to_disable>
        else:
            return self.cleaned_data.get('<field_to_disable>', None)

And creating the form you need:
    
    MyChangeForm(instance=MyChange.objects.get_or_create(<field_to_disable>="something"))
    

This example was based on [this question][2].


  [1]: https://docs.djangoproject.com/en/1.9/ref/forms/fields/#disabled
  [2]: http://stackoverflow.com/questions/324477/in-a-django-form-how-do-i-make-a-field-readonly-or-disabled-so-that-it-cannot

## Using Django Model Form with Django Class Based View.
Django Model Form with [Django Class Based][1] view is a classic way of building pages to do create/update operations in django application quickly. Within the form we can put methods to execute tasks. Its a cleaner way to put tasks in forms rather than putting in views/models.


To give an example using Django Model Form, first we need to define our Model.

    class MyModel(models.Model):
       name = models.CharField(
              verbose_name = 'Name',
              max_length = 255)

Now let us make a form using this model:

    class MyModelForm(forms.ModelForm):
    
        class Meta:
            model = MyModel
            fields = '__all__'

Lets add a method to print hello world in it.

    class MyModelForm(forms.ModelForm):
        class Meta:
            model = MyModel
            fields = '__all__'
        
        def print_hello_world(self):
             print('Hello World')

Lets make a template to display the form:

    <form method="post" action="">
        {% csrf_token %}
    <ul>
        {{ form.as_p }}
    </ul>
    <input type="submit" value="Submit Form"/>
    </form>

Now we will use this form in three different views which will respectively Create and Update tasks.

    from django.views.generic.edit import CreateView, UpdateView
    from myapp.models import MyModel
    
    class MyModelCreate(CreateView):
        model = MyModel
        fields = ['name']
        form_class = MyModelForm
        template_name = 'my_template.html'

        def form_valid(self, form):
            # This method is called when valid form data has been POSTed.
            # It should return an HttpResponse.
            form.print_hello_world() # This method will print hello world in console
            return super(MyModelCreate, self).form_valid(form)
        
    
    class MyModelUpdate(UpdateView):
        model = MyModel
        fields = ['name']
        form_class = MyModelForm
        template_name = 'my_template.html'

Now lets create a urls for accessing those views.

    from django.conf.urls import url
    from myapp.views import MyModelCreate, MyModelUpdate
    
    urlpatterns = [
        # ...
        url(r'mymodel/add/$', MyModelCreate.as_view(), name='author-add'),
        url(r'mymodel/(?P<pk>[0-9]+)/$', MyModelUpdate.as_view(), name='author-update')
    ]

Okay, our work has been done. We can access url: `localhost:8000/mymodel/add` for creating entry in the model. Also access `localhost:8000/mymodel/1` to update that entry.



  [1]: https://docs.djangoproject.com/en/1.10/topics/class-based-views/

