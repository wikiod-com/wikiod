---
title: "Flask-WTF"
slug: "flask-wtf"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

It is a simple integration of Flask and WTForms. It allows for the easier creation and management of web forms, it automatically generates a CRSF token hidden field in your templates. It also features easy form validation functions

## A simple Form
    from flask_wtf import FlaskForm
    from wtforms import StringField, IntegerField
    from wtforms.validators import DataRequired

    class MyForm(FlaskForm):
        name = StringField('name', validators=[DataRequired()])
        age = InterField('age', validators=[DataRequired()])

To render the template you will use something like this:

    <form method="POST" action="/">
        {{ form.hidden_tag() }}
        {{ form.name.label }} {{ form.name(size=20) }}
        <br/>
        {{ form.age.label }} {{ form.age(size=3) }}
        <input type="submit" value="Go">
    </form>

The above simple code will generate our very simple flask-wtf web form with a hidden CRSF token field.



