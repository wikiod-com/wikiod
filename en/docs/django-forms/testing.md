---
title: "Testing"
slug: "testing"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

One core feature of Django is unit tests.

This topic intends to bring a complete documentation on how to test forms.

## Simple Test
    from django.test import TestCase
    from myapp.forms import MyForm
    
    class MyAppTests(TestCase):
        def test_forms(self):
            form_data = {'field1': 'fieldvalue1'}
            form = MyForm(data=form_data)
            self.assertTrue(form.is_valid())

