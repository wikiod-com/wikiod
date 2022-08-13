---
title: "Filters"
slug: "filters"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Filtering Examples, from Simple To More Complex ones
Plain Vanilla Filtering
=======================

To filter any view, override its `get_queryset` method to return a filtered query set

    class HREmployees(generics.ListAPIView):
        def get_queryset(self):
            return Employee.objects.filter(department="Human Resources")

All the API functions will then use the filtered query set for operations. For example, the listing of the above view will only contain employees whose department is Human Resources.

Accessing query parameters in get_queryset
======

Filtering based on request parameters is easy.

`self.request`, `self.args` and `self.kwargs` are available and point to the current request and its parameters for filtering

    def DepartmentEmployees(generics.ListAPIView):
        def get_queryset(self):
            return Employee.objects.filter(department=self.kwargs["department"])

Letting the API parameters decide what to filter
=====
If you want more flexibility and allow the API call to pass in parameters to filter the view, you can plugin filter backends like Django Request Framework(installed via pip)


    from rest_framework import filters  

    class Employees(generics.ListAPIView):
        queryset=Employee.objects.all()
        filter_backends = (filters.DjangoFilterBackend,)
        filter_fields = ("department", "role",)

Now you can make an API call `/api/employees?department=Human Resources` and you'll get a list of employees that belong only to the HR department, or `/api/employees?role=manager&department=Human Resources` to get only managers in the HR department.

You can combine query set filtering with Django Filter Backend, no problemo. The filters will work on the filtered query set returned by `get_queryset`

    from rest_framework import filters  

    class HREmployees(generics.ListAPIView):
        filter_backends = (filters.DjangoFilterBackend,)
        filter_fields = ("department", "role",)

        def get_queryset(self):
            return Employee.objects.filter(department="Human Resources")

FilterSets
====
So far, you can get by with simple type matches in the above cases.

But what if you want something more complex, like a list of HR employees who are between 25 and 32 years in age?

Answer to problem: Filtersets

Filter sets are classes that define how to filter various fields of the model. 

Define em like so

    class EmployeeFilter(django_filters.rest_framework.FilterSet):
        min_age = filters.django_filters.NumberFilter(name="age", lookup_expr='gte')
        max_age = filters.django_filters.NumberFilter(name="price", lookup_expr='lte')     
 
        class Meta:
            model = Employee
            fields = ['age', 'department']

`name` points to the field which you want to filter

`lookup_expr` basically refers to the same names you use while filtering query sets, for example you can do a "starts with" match using `lookup_expr="startswith"` which is equivalent to `Employee.objects.filter(department__startswith="Human")`

Then use them in your view classes by using `filter_class` instead of `filter_fields`

    class Employees(generics.ListAPIView):
        queryset=Employee.objects.all()
        filter_backends = (filters.DjangoFilterBackend,)
        filter_class = EmployeeFilter

Now you can do `/api/employees?department=Human Resources&min_age=25&max_age=32`

Non-exact matches and relationships
=====

Filter classes and expressions are very similar to how you specify filtering in query sets

You can use the "__" notation to filter fields in relationships, For example, if department was a foreign key from employee, you can add

    filter_fields=("department__name",)

and then you can do `/api/employees?department__name=Human Resources`

Or more elegantly, you can create a filter set, add a filter variable called `dept` and set its name to `department__name`, allowing you to do `/api/employees?dept=Human Resources`












