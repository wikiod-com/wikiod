---
title: "Formsets"
slug: "formsets"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Syntax
 - NewFormSet = formset_factory( SomeForm, extra=2 )
- formset = NewFormSet( initial = [
{'some_field': 'Field Value',
'other_field': 'Other Field Value',}
])

## Formsets with Initialized and unitialized data
 `Formset` is a way to render multiple forms in one page, like a grid of data.
Ex: This `ChoiceForm` might be associated with some question of sort. like,
Kids are most Intelligent between which age?.

`appname/forms.py`

    from django import forms
    class ChoiceForm(forms.Form):
        choice = forms.CharField()
        pub_date = forms.DateField()

In your views you can use `formset_factory` constructor which takes takes `Form` as a parameter its `ChoiceForm` in this case and `extra` which describes how many extra forms other than initialized form/forms needs to be rendered, and you can loop over the `formset` object just like any other iterable.

If the formset is not initialized with data it prints the number of forms equal to `extra + 1` and if the formset is initialized it prints `initialized + extra` where `extra` number of empty forms other than initialized ones.

`appname/views.py`

    import datetime
    from django.forms import formset_factory
    from appname.forms import ChoiceForm
        ChoiceFormSet = formset_factory(ChoiceForm, extra=2)
        formset = ChoiceFormSet(initial=[
          {'choice': 'Between 5-15 ?',
            'pub_date': datetime.date.today(),}
          ])

if you loop over `formset object` like this
  for form in formset:
    print(form.as_table())

`Output in rendered template`
    
    <tr>
    <th><label for="id_form-0-choice">Choice:</label></th>
    <td><input type="text" name="form-0-choice" value="Between 5-15 ?" id="id_form-0-choice" /></td>
    </tr>
    <tr>
    <th><label for="id_form-0-pub_date">Pub date:</label></th>
    <td><input type="text" name="form-0-pub_date" value="2008-05-12" id="id_form-0-pub_date" /></td>
    </tr>
    <tr>
    <th><label for="id_form-1-choice">Choice:</label></th>
    <td><input type="text" name="form-1-choice" id="id_form-1-choice" /></td>
    </tr>
    <tr>
    <th><label for="id_form-1-pub_date">Pub date:</label></th>
    <td><input type="text" name="form-1-pub_date" id="id_form-1-pub_date" /></td
    </tr>
    <tr>
    <th><label for="id_form-2-choice">Choice:</label></th>
    <td><input type="text" name="form-2-choice" id="id_form-2-choice" /></td>
    </tr>
    <tr>
    <th><label for="id_form-2-pub_date">Pub date:</label></th>
    <td><input type="text" name="form-2-pub_date" id="id_form-2-pub_date" /></td>
    </tr>

