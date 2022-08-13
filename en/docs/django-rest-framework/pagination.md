---
title: "Pagination"
slug: "pagination"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

Pagination is the splitting of large datasets into separated and autonomous pages.

On django rest framework, pagination allows the user to modify the amount of data in each page and the style by which the split is applied.

## [Introductory] Setup Pagination Style Globally
In order to set the pagination style for the entire project, you need to set the `DEFAULT_PAGINATION_CLASS` and `PAGE_SIZE` on the project settings.

To do so, go to `settings.py` and on the `REST_FRAMEWORK` variable, add the following:

    REST_FRAMEWORK = {
        'DEFAULT_PAGINATION_CLASS': 
            'rest_framework.pagination.DESIRED_PAGINATION_STYLE',
        'PAGE_SIZE': 100
    }

In place of the `DESIRED_PAGINATION_STYLE` one of the following must be placed:

 - `PageNumberPagination`: Accepts a single `page` number in the request query parameters.

       http://your_api_url/a_table/?page=2
 - `LimitOffsetPagination`: Accepts a `limit` parameter, which indicates the maximum number of items that will be returned and an `offset` parameter which indicates the starting position of the query in relation to the dataset. `PAGE_SIZE` does not need to be set for this style.

       http://your_api_url/a_table/?limit=50&offset=100

 - `CursorPagination`: Cursor based pagination is more complex than the above styles. It requires that the dataset presents a fixed ordering, and does not allow the client to navigate into arbitrarily positions of the dataset.

 - Custom pagination styles can be defined in place of the above.

   



## [Advanced] Pagination on Non Generic Views/Viewsets
*This is an advanced subject, do not attempt without first understanding the other examples of this page*.

As stated in the official [Django Rest Framework on pagination][1]:

> Pagination is only performed automatically if you're using the generic views or viewsets. If you're using a regular APIView, you'll need to call into the pagination API yourself to ensure you return a paginated response. See the source code for the mixins.ListModelMixin and generics.GenericAPIView classes for an example.

But what if we want to use pagination on a non generic view/viewset?

Well let's go down the rabbit hole:

 1. First stop is the official Django Rest Framework's repository and specifically the 
 [django-rest-framework/rest_framework/generics.py][2]. The specific line this link is pointing at, shows us how the developers of the framework deal with pagination in their generics. <br>
 That is exactly what we are going to use to our view as well!
 2. Let's assume that we have a global pagination setup like the one shown in the [introductory example of this page][3] and lets assume as well that we have an `APIView` which we want to apply pagination to. 
 3. Then on `views.py`:

        from django.conf import settings
        from rest_framework.views import APIView

        class MyView(APIView):
            queryset = OurModel.objects.all()
            serializer_class = OurModelSerializer
            pagination_class = settings.DEFAULT_PAGINATION_CLASS # cool trick right? :)

            # We need to override get method to achieve pagination
            def get(self, request):
                ...
                page = self.paginate_queryset(self.queryset)
                if page is not None:
                    serializer = self.serializer_class(page, many=True)
                    return self.get_paginated_response(serializer.data)

                ... Do other stuff needed (out of scope of pagination)

            # Now add the pagination handlers taken from 
            #  django-rest-framework/rest_framework/generics.py

            @property
            def paginator(self):
                """
                The paginator instance associated with the view, or `None`.
                """
                 if not hasattr(self, '_paginator'):
                     if self.pagination_class is None:
                         self._paginator = None
                     else:
                         self._paginator = self.pagination_class()
                 return self._paginator

             def paginate_queryset(self, queryset):
                 """
                 Return a single page of results, or `None` if pagination is disabled.
                 """
                 if self.paginator is None:
                     return None
                 return self.paginator.paginate_queryset(queryset, self.request, view=self)

             def get_paginated_response(self, data):
                 """
                 Return a paginated style `Response` object for the given output data.
                 """
                 assert self.paginator is not None
                 return self.paginator.get_paginated_response(data)

Now we have an `APIView` that handles pagination!

  [1]: http://www.django-rest-framework.org/api-guide/pagination/
  [2]: https://github.com/encode/django-rest-framework/blob/master/rest_framework/generics.py#L155
  [3]: https://www.wikiod.com/django-rest-framework/pagination#[Introductory] Setup Pagination Style Globally

## [Intermediate] Override Pagination style and setup Pagination per class
**Override Pagination Style:**

Every available pagination style can be overridden by creating a new class that inherits from one of the available styles and then alters its parameters:

    class MyPagination(PageNumberPagination):
        page_size = 20
        page_size_query_param = 'page_size' 
        max_page_size = 200
        last_page_strings = ('the_end',)

 Those parameters (as listed on the [pagination official documentation][1]) are:

*PageNumberPagination*

 - `page_size`: A numeric value indicating the page size. If set, this overrides the `PAGE_SIZE` setting. Defaults to the same value as the `PAGE_SIZE` settings key.
 - `page_query_param`: A string value indicating the name of the query parameter to use for the pagination control.
 - `page_size_query_param`: If set, this is a string value indicating the name of a query parameter that allows the client to set the page size on a per-request basis. Defaults to `None`, indicating that the client may not control the requested page size.
 - `max_page_size`: If set, this is a numeric value indicating the maximum allowable requested page size. This attribute is only valid if `page_size_query_param` is also set.
 - `last_page_strings`: A list or tuple of string values indicating values that may be used with the `page_query_param` to request the final page in the set. Defaults to `('last',)`
 - `template`: The name of a template to use when rendering pagination controls in the browsable API. May be overridden to modify the rendering style, or set to `None` to disable HTML pagination controls completely. Defaults to `"rest_framework/pagination/numbers.html"`.

*LimitOffsetPagination*

 - `default_limit`: A numeric value indicating the limit to use if one is not provided by the client in a query parameter. Defaults to the same value as the `PAGE_SIZE` settings key.
 - `limit_query_param`: A string value indicating the name of the "limit" query parameter. Defaults to `'limit'`.
 - `offset_query_param`: A string value indicating the name of the "offset" query parameter. Defaults to `'offset'`.
 - `max_limit`: If set this is a numeric value indicating the maximum allowable limit that may be requested by the client. Defaults to `None`.
 - `template`: Same as *PageNumberPagination*. 

*CursorPagination*

 - `page_size`: Same as *PageNumberPagination*.
 - `cursor_query_param`: A string value indicating the name of the "cursor" query parameter. Defaults to `'cursor'`.
 - `ordering`: This should be a string, or list of strings, indicating the field against which the cursor based pagination will be applied. For example: `ordering = 'slug'`. Defaults to `-created`. This value may also be overridden by using `OrderingFilter` on the view.
 - `template`: Same as *PageNumberPagination*.

**Setup Pagination per Class:**

In addition to the ability to setup the Pagination style globally, a setup per class is available:

    class MyViewSet(viewsets.GenericViewSet):
        pagination_class = LimitOffsetPagination

Now only `MyViewSet` has a `LimitOffsetPagination` pagination.<br>

A custom pagination style can be used in the same way:

    class MyViewSet(viewsets.GenericViewSet):
        pagination_class = MyPagination
    


  [1]: http://www.django-rest-framework.org/api-guide/pagination/#api-reference

## [Intermediate] Complex Usage Example
Lets assume that we have a complex api, with many generic views and some generic viewsets. We want to enable `PageNumberPagination` to every view, except one (either generic view or viewset, does not make a difference) for which we want a customized case of `LimitOffsetPagination`. 

To achieve that we need to:

 1. On the `settings.py` we will place our default pagination in order to enable it for every generic view/viewset and we will set `PAGE_SIZE` to 50 items:

        REST_FRAMEWORK = {
            'DEFAULT_PAGINATION_CLASS': 
                'rest_framework.pagination.PageNumberPagination',
            'PAGE_SIZE': 50
        }

 2. Now in our `views.py` (or in another `.py` ex: `paginations.py`), we need to override the `LimitOffsetPagination`:

        from rest_framework.pagination import LimitOffsetPagination

        class MyOffsetPagination(LimitOffsetPagination):
            default_limit = 20
            max_limit = 1000
            
     A custom `LimitOffsetPagination` with `PAGE_ZISE` of 20 items and maximum `limit` of 1000 items.

 3. In our `views.py`, we need to define the `pagination_class` of our special view:

        imports ...

        # ===================================
        #    PageNumberPagination classes
        # ===================================

        class FirstView(generics.ListAPIView):
            ...

        class FirstViewSet(viewsets.GenericViewSet):
            ...

        ...

        # ===================================
        #     Our custom Pagination class
        # ===================================

        class IAmSpecialView(generics.ListAPIView):
            pagination_class = MyOffsetPagination
            ...

Now every generic view/viewset in the app has `PageNumberPagination`, except `IAmSpecial` class, which is indeed *special* and has its own customized `LimitOffsetPagination`.

## [Intermediate] Pagination on a function based view
We have seen in those examples ([ex_1][1], [ex_2][2]) how to use and override the pagination classes in any generic class base view. <br>
What happens when we want to use pagination in a function based view?

Lets also assume that we want to create a function based view for `MyModel` with `PageNumberPagination`, responding only to a `GET` request. Then:

    from rest_framework.pagination import PageNumberPagination
    

    @api_view(['GET',])
    def my_function_based_list_view(request):
        paginator = PageNumberPagination()
        query_set = MyModel.objects.all()
        context = paginator.paginate_queryset(query_set, request)
        serializer = MyModelSerializer(context, many=True)
        return paginator.get_paginated_response(serializer.data)
    
<hr>
We can do the above for a custom pagination as well by changing this line:

    paginator = PageNumberPagination()

to this

    paginator = MyCustomPagination()

provided that we have defined `MyCustomPagination` [to override some default pagination][2]


  [1]: https://www.wikiod.com/django-rest-framework/pagination#[Intermediate] Complex Usage Example
  [2]: https://www.wikiod.com/django-rest-framework/pagination#[Intermediate] Override Pagination style and setup Pagination per class

