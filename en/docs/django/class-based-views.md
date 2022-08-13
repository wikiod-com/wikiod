---
title: "Class based views"
slug: "class-based-views"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

When using CBV we often need to know exactly what methods we can overwrite for each generic class. [This page][1] of the django documentation lists all the generic classes with all of their methods flattened and the class attributes we can use.

In addition, [Classy Class Based View][2] website provides the same information with a nice interactive interface.

  [1]: https://docs.djangoproject.com/en/1.9/ref/class-based-views/flattened-index/
  [2]: https://ccbv.co.uk/

## Form and object creation
Writing a view to create object can be quite boring. You have to display a form, you have to validate it, you have to save the item or return the form with an error. Unless you use one of the [generic editing views][editing_views].

# app/views.py
    from django.core.urlresolvers import reverse_lazy
    from django.views.generic.edit import CreateView, UpdateView, DeleteView
    from .models import Pokemon


    class PokemonCreate(CreateView):
        model = Pokemon
        fields = ['name', 'species']


    class PokemonUpdate(UpdateView):
        model = Pokemon
        fields = ['name', 'species']


    class PokemonDelete(DeleteView):
        model = Pokemon
        success_url = reverse_lazy('pokedex')

`CreateView` and `UpdateView` have two required attribute, `model` and `fields`. By default, both use a template name based on the model name suffixed by '_form'. You can change only the suffix with the attribute template_name_suffix. The DeleteView show a confirmation message before deleting the object.

Both `UpdateView` and `DeleteView` need to fetch on object. They use the same method as `DetailView`, extracting variable from the url and matching the object fields.

# app/templates/app/pokemon_form.html (extract)
    <form action="" method="post">
        {% csrf_token %}
        {{ form.as_p }}
        <input type="submit" value="Save" />
    </form>

`form` contains the form with all needed fields. Here, it will be displayed with a paragraph for each field because of `as_p`.

# app/templates/app/pokemon_confirm_delete.html (extract)
    <form action="" method="post">
        {% csrf_token %}
        <p>Are you sure you want to delete "{{ object }}"?</p>
        <input type="submit" value="Confirm" />
    </form>

The `csrf_token` tag is required because of django protection against request forgery. The attribute action is left empty as the url displaying the form is the same as the one handling the deletion/save.

Two issues remain with the model, if using the same as with the list and detail exemple. First, create and update will complain about a missing redirection url. That can be solved by adding a `get_absolute_url` to the pokemon model. Second issue is the deletion confirmation not displaying meaningful information. To solve this, the easiest solution is to add a string representation.

# app/models.py
    from django.db import models
    from django.urls import reverse
    from django.utils.encoding import python_2_unicode_compatible


    @python_2_unicode_compatible
    class Pokemon(models.Model):
        name = models.CharField(max_length=24)
        species = models.CharField(max_length=48)

        def get_absolute_url(self):
            return reverse('app:pokemon', kwargs={'pk':self.pk})

        def __str__(self):
            return self.name

The class decorator will make sure everything work smoothly under python 2.

[editing_views]: https://docs.djangoproject.com/en/1.9/ref/class-based-views/generic-editing/

## List and Details views
Template views are fine for static page and you could use them for everything with `get_context_data` but it would be barely better than using function as views.

Enter [ListView][list_view] and [DetailView][detail_view]

# app/models.py
    from django.db import models

    class Pokemon(models.Model):
        name = models.CharField(max_length=24)
        species = models.CharField(max_length=48)
        slug = models.CharField(max_length=48)

# app/views.py
    from django.views.generic import ListView, DetailView
    from .models import Pokemon


    class PokedexView(ListView):
        """ Provide a list of Pokemon objects """
        model = Pokemon
        paginate_by = 25

    class PokemonView(DetailView):
        model = Pokemon

That's all you need to generate a view listing all your objects of a models and views of singular item. The list is even paginated. You can provide `template_name` if you want something specific. By default, it's generated from the model name.

# app/templates/app/pokemon_list.html
    <!DOCTYPE html>
    <title>Pokedex</title>
    <ul>{% for pokemon in pokemon_list %}
        <li><a href="{% url "app:pokemon" pokemon.pk %}">{{ pokemon.name }}</a>
            &ndash; {{ pokemon.species }}
    </ul>

The context is populated with the list of object under two name, `object_list` and a second one build from the model name, here `pokemon_list`. If you have paginated the list, you have to take care of next and previous link too. The [Paginator][paginator] object can help with that, it's available in the context data too.

# app/templates/app/pokemon_detail.html
    <!DOCTYPE html>
    <title>Pokemon {{ pokemon.name }}</title>
    <h1>{{ pokemon.name }}</h1>
    <h2>{{ pokemon.species }} </h2>

As before, the context is populated with your model object under the name `object` and `pokemon`, the second one being derived from the model name.

# app/urls.py
    from django.conf.urls import url
    from . import views

    app_name = 'app'
    urlpatterns = [
        url(r'^pokemon/$', views.PokedexView.as_view(), name='pokedex'),
        url(r'^pokemon/(?P<pk>\d+)/$', views.PokemonView.as_view(), name='pokemon'),
    ]

In this snippet, the url for the detail view is built using the primary key. It's also possible to use a slug as argument. This gives a nicer looking url that's easier to remember. However it requires the presence of a field named slug in your model.

    url(r'^pokemon/(?P<slug>[A-Za-z0-9_-]+)/$', views.PokemonView.as_view(), name='pokemon'),
 
If a field called `slug` is not present, you can use the `slug_field` setting in `DetailView` to point to a different field.

For pagination, use a page get parameters or put a page directly in the url.

[list_view]: https://docs.djangoproject.com/en/1.9/ref/class-based-views/generic-display/#listview
[detail_view]: https://docs.djangoproject.com/en/1.9/ref/class-based-views/generic-display/#detailview
[paginator]: https://docs.djangoproject.com/en/1.9/topics/pagination/#django.core.paginator.Paginator


## Class Based Views
Class based views let you focus on what make your views special.

A static about page might have nothing special, except the template used. Use a [TemplateView][template_view]! All you have to do is set a template name. Job done. Next.

# views.py

    from django.views.generic import TemplateView


    class AboutView(TemplateView):
        template_name = "about.html"

# urls.py
    from django.conf.urls import url
    from . import views

    urlpatterns = [
        url('^about/', views.AboutView.as_view(), name='about'),
    ]

Notice how we don't use directly `AboutView` in the url. That's because a callable is expected and that's exactly what `as_view()` return.

[template_view]: https://docs.djangoproject.com/en/1.9/ref/class-based-views/base/#templateview

## Context data
Sometimes, your template need a bit more of information. For example, we would like to have the user in the header of the page, with a link to their profile next to the logout link. In these cases, use the `get_context_data` method.

# views.py
    class BookView(DetailView):
        template_name = "book.html"

        def get_context_data(self, **kwargs)
            """ get_context_data let you fill the template context """
            context = super(BookView, self).get_context_data(**kwargs)
            # Get Related publishers
            context['publishers'] = self.object.publishers.filter(is_active=True)
            return context

You need to call get_context_data method on the super class and it will return the default context instance. Any item that you add to this dictionary will be available to the template.

# book.html

    <h3>Active publishers</h3>
    <ul>
      {% for publisher in publishers %}
        <li>{{ publisher.name }}</li>
      {% endfor %}
    </ul>


## Minimal example


## Django Class Based Views: Example of CreateView


## One View, Multiple Forms
Here is a quick example of using multiple forms in one Django view.

    from django.contrib import messages
    from django.views.generic import TemplateView

    from .forms import AddPostForm, AddCommentForm
    from .models import Comment

    class AddCommentView(TemplateView):

        post_form_class = AddPostForm
        comment_form_class = AddCommentForm
        template_name = 'blog/post.html'

        def post(self, request):
            post_data = request.POST or None
            post_form = self.post_form_class(post_data, prefix='post')
            comment_form = self.comment_form_class(post_data, prefix='comment')

            context = self.get_context_data(post_form=post_form,
                                            comment_form=comment_form)

            if post_form.is_valid():
                self.form_save(post_form)
            if comment_form.is_valid():
                self.form_save(comment_form)

            return self.render_to_response(context)     

        def form_save(self, form):
            obj = form.save()
            messages.success(self.request, "{} saved successfully".format(obj))
            return obj

        def get(self, request, *args, **kwargs):
            return self.post(request, *args, **kwargs)
    

