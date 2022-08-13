---
title: "Administration"
slug: "administration"
draft: false
images: []
weight: 9895
type: docs
toc: true
---

## Dealing with foreign keys referencing large tables
By default, Django renders `ForeignKey` fields as a `<select>` input. This can cause pages to be **load really slowly** if you have thousands or tens of thousand entries in the referenced table. And even if you have only hundreds of entries, it is quite uncomfortable to look for a particular entry among all.

A very handy external module for this is [django-autocomplete-light][1] (DAL). This enables to use autocomplete fields instead of `<select>` fields.

[![django-autocomplete-light example][2]][2]

## views.py

<!-- language lang-python -->
    from dal import autocomplete

    class CityAutocomp(autocomplete.Select2QuerySetView):
        def get_queryset(self):
            qs = City.objects.all()
            if self.q:
                qs = qs.filter(name__istartswith=self.q)
            return qs

## urls.py

<!-- language lang-python -->
    urlpatterns = [
        url(r'^city-autocomp/$', CityAutocomp.as_view(), name='city-autocomp'),
    ]


## forms.py

<!-- language lang-python -->
    from dal import autocomplete

    class PlaceForm(forms.ModelForm):
        city = forms.ModelChoiceField(
            queryset=City.objects.all(),
            widget=autocomplete.ModelSelect2(url='city-autocomp')
        )

        class Meta:
            model = Place
            fields = ['__all__']


## admin.py

<!-- language lang-python -->
    @admin.register(Place)
    class PlaceAdmin(admin.ModelAdmin):
        form = PlaceForm


  [1]: http://django-autocomplete-light.readthedocs.io/
  [2]: http://i.stack.imgur.com/WcP7C.png

## Additional CSS styles and JS scripts for admin page
Suppose you have a simple `Customer` model:  

    class Customer(models.Model):
        first_name = models.CharField(max_length=255)
        last_name = models.CharField(max_length=255)
        is_premium = models.BooleanField(default=False)
You register it in the Django admin and add search field by `first_name` and `last_name`:  

    @admin.register(Customer)
    class CustomerAdmin(admin.ModelAdmin):
        list_display = ['first_name', 'last_name', 'is_premium']
        search_fields = ['first_name', 'last_name']
After you do this, the search fields appear in the admin list page with the default placeholder: "*keyword*". But what if you want to change that placeholder to "*Search by name*"?  

You can do this by passing custom Javascript file into admin `Media`:  

    @admin.register(Customer)
    class CustomerAdmin(admin.ModelAdmin):
        list_display = ['first_name', 'last_name', 'is_premium']
        search_fields = ['first_name', 'last_name']
        
        class Media:
            #this path may be any you want, 
            #just put it in your static folder
            js = ('js/admin/placeholder.js', )
You can use browser debug toolbar to find what id or class Django set to this searchbar and then write your js code:  

    $(function () {
       $('#searchbar').attr('placeholder', 'Search by name')
    })
Also `Media` class allows you to add css files with dictionary object:  

    class Media:
        css = {
            'all': ('css/admin/styles.css',)
             }


For example we need to display each element of `first_name` column in specific color.  
By default Django create table column for every item in `list_display`, all `<td>` tags will have css class like `field-'list_display_name'`, in our case it will `field_first_name`  

    .field_first_name {
         background-color: #e6f2ff;
     }

If you want to customize other behavior by adding JS or some css styles, you can always check id`s and classes of elements in the browser debug tool.

## Change list
Let's say you have a simple `myblog` app with the following model:

<!-- language lang-python -->
    from django.conf import settings
    from django.utils import timezone

    class Article(models.Model):
        title = models.CharField(max_length=70)
        slug = models.SlugField(max_length=70, unique=True)
        author = models.ForeignKey(settings.AUTH_USER_MODEL, models.PROTECT)
        date_published = models.DateTimeField(default=timezone.now)
        is_draft = models.BooleanField(default=True)
        content = models.TextField()

Django Admin's "change list" is the page that lists all objects of a given model.

<!-- language lang-python -->
    from django.contrib import admin
    from myblog.models import Article

    @admin.register(Article)
    class ArticleAdmin(admin.ModelAdmin):
        pass

By default, it will use the `__str__()` method (or `__unicode__()` if you on python2) of your model to display the object "name". This means that if you didn't override it, you will see a list of articles, all named "Article object". To change this behavior, you can set the `__str__()` method:

<!-- language lang-python -->
    class Article(models.Model):
        def __str__(self):
            return self.title

Now, all your articles should have a different name, and more explicit than "Article object".

However you may want to display other data in this list. For this, use `list_display`:

<!-- language lang-python -->
    @admin.register(Article)
    class ArticleAdmin(admin.ModelAdmin):
        list_display = ['__str__', 'author', 'date_published', 'is_draft']

`list_display` is not limited to the model fields and properties. it can also be a method of your `ModelAdmin`:

<!-- language lang-python -->
    from django.forms.utils import flatatt
    from django.urls import reverse
    from django.utils.html import format_html

    @admin.register(Article)
    class ArticleAdmin(admin.ModelAdmin):
        list_display = ['title', 'author_link', 'date_published', 'is_draft']
        
        def author_link(self, obj):
            author = obj.author
            opts = author._meta
            route = '{}_{}_change'.format(opts.app_label, opts.model_name)
            author_edit_url = reverse(route, args=[author.pk])
            return format_html(
                '<a{}>{}</a>', flatatt({'href': author_edit_url}), author.first_name)

        # Set the column name in the change list
        author_link.short_description = "Author"
        # Set the field to use when ordering using this column
        author_link.admin_order_field = 'author__firstname'

