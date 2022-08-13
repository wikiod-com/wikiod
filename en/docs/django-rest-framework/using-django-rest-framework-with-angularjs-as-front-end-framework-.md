---
title: "Using django-rest-framework with AngularJS as front-end framework."
slug: "using-django-rest-framework-with-angularjs-as-front-end-framework"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

In this topic we will look at how to use Django REST Framework as a backend API for a AngularJS app. 
The main issues that arise between using DRF and AngularJS together generally revolve around the HTTP communication between the two technologies, as well as the representation of the data on both ends, and finally how to deploy and architect the application/system.

## Angular Request
    $http.post("\<domain\>/user-registration/", {username: username, password: password})
         .then(function (data) { 
             // Do success actions here
         });

## DRF View
    class UserRegistration(APIView):
        def post(self, request, *args, **kwargs):
            serializer = UserRegistrationSerializer(data=request.data)
            serializer.is_valid(raise_exception=True)
            serializer.save()
            return Response(serializer.to_representation(instance=serializer.instance), status=status.HTTP_201_CREATED)

