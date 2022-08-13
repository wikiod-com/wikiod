---
title: "Internationalization"
slug: "internationalization"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Syntax
- gettext(message)
- ngettext(singular, plural, number)
- ugettext(message)
- ungettext(singular, plural, number)
- pgettext(context, message)
- npgettext(context, singular, plural, number)
- gettext_lazy(message)
- ngettext_lazy(singular, plural, number=None)
- ugettext_lazy(message)
- ungettext_lazy(singular, plural, number=None)
- pgettext_lazy(context, message)
- npgettext_lazy(context, singular, plural, number=None)
- gettext_noop(message)
- ugettext_noop(message)

## Introduction to Internationalization
# Setting up

## settings.py

    from django.utils.translation import ugettext_lazy as _

    USE_I18N = True  # Enable Internationalization
    LANGUAGE_CODE = 'en'  # Language in which original texts are written
    LANGUAGES = [  # Available languages
        ('en', _("English")),
        ('de', _("German")),
        ('fr', _("French")),
    ]

    # Make sure the LocaleMiddleware is included, AFTER SessionMiddleware
    # and BEFORE middlewares using internationalization (such as CommonMiddleware)
    MIDDLEWARE_CLASSES = [
       'django.contrib.sessions.middleware.SessionMiddleware',
       'django.middleware.locale.LocaleMiddleware',
       'django.middleware.common.CommonMiddleware',
    ]

# Marking strings as translatable

The first step in translation is is to *mark strings as translatable*. This is passing them through one of the `gettext` functions (See the [Syntax section][1]). For instance, here is an example model definition:

    from django.utils.translation import ugettext_lazy as _
    # It is common to import gettext as the shortcut `_` as it is often used
    # several times in the same file.

    class Child(models.Model):

        class Meta:
            verbose_name = _("child")
            verbose_name_plural = _("children")

        first_name = models.CharField(max_length=30, verbose_name=_("first name"))
        last_name = models.CharField(max_length=30, verbose_name=_("last name"))
        age = models.PositiveSmallIntegerField(verbose_name=_("age"))

All strings encapsulated in `_()` are now marked as translatable. When printed, they will always be displayed as the encapsulated string, whatever the chosen language (since no translation is available yet). 

# Translating strings

This example is sufficient to get started with translation. Most of the time you will only want to mark strings as translatable to **anticipate prospective internationalization** of your project. Thus, this is covered [in another example][2].

[1]: https://www.wikiod.com/django/internationalization
[2]: https://www.wikiod.com/django/internationalization#Translating strings

## Lazy vs Non-Lazy translation
When using non-lazy translation, strings are translated immediately.

    >>> from django.utils.translation import activate, ugettext as _
    >>> month = _("June")
    >>> month
    'June'
    >>> activate('fr')
    >>> _("June")
    'juin'
    >>> activate('de')
    >>> _("June")
    'Juni'
    >>> month
    'June'

When using laziness, translation only occurs when actually used.

    >>> from django.utils.translation import activate, ugettext_lazy as _
    >>> month = _("June")
    >>> month
    <django.utils.functional.lazy.<locals>.__proxy__ object at 0x7f61cb805780>
    >>> str(month)
    'June'
    >>> activate('fr')
    >>> month
    <django.utils.functional.lazy.<locals>.__proxy__ object at 0x7f61cb805780>
    >>> "month: {}".format(month)
    'month: juin'
    >>> "month: %s" % month
    'month: Juni'

You have to use lazy translation in cases where:

  - Translation may not be activated (language not selected) when `_("some string")` is evaluated
  - Some strings may be evaluated only at startup (eg. in class attributes such as model and form fields definitions)

## Translation in templates
To enable translation in templates you must load the `i18n` library.

    {% load i18n %}

Basic translation is made with the `trans` template tag.

    {% trans "Some translatable text" %}
    {# equivalent to python `ugettext("Some translatable text")` #}

The `trans` template tag supports context:

    {% trans "May" context "month" %}
    {# equivalent to python `pgettext("May", "month")` #}

To include placeholders in your translation string, as in:

    _("My name is {first_name} {last_name}").format(first_name="John", last_name="Doe")

You will have to use the `blocktrans` template tag:

    {% blocktrans with first_name="John" last_name="Doe" %}
      My name is {{ first_name }} {{ last_name }}
    {% endblocktrans %}

Of course instead of `"John"` and `"Doe"` you can have variables and filters:

    {% blocktrans with first_name=user.first_name last_name=user.last_name|title %}
      My name is {{ first_name }} {{ last_name }}
    {% endblocktrans %}

If `first_name` and `last_name` are already in your context, you can even omit the `with` clause:

    {% blocktrans %}My name is {{ first_name }} {{ last_name }}{% endblocktrans %}

However, only "top-level" context variables can be use. **This will NOT work:**

    {% blocktrans %}
        My name is {{ user.first_name }} {{ user.last_name }}
    {% endblocktrans %}

This is mainly because the variable name is used as placeholder in translation files.

The `blocktrans` template tag also accepts pluralization.

    {% blocktrans count nb=users|length }}
        There is {{ nb }} user.
    {% plural %}
        There are {{ nb }} users.
    {% endblocktrans %}

Finally, regardless of the `i18n` library, you can pass translatable strings to template tags using the `_("")` syntax.

    {{ site_name|default:_("It works!") }}
    {% firstof var1 var2 _("translatable fallback") %}

This is some magic built-in django template system to mimic a function call syntax but this ain't a function call. `_("It works!")` passed to the `default` template tag as a string `'_("It works!")'` which is then parsed a translatable string, just as `name` would be parsed as a variable and `"name"` would be parsed as a string.

## Translating strings
To translate strings, you will have to create translation files. To do so, django ships with the management command `makemessages`.

    $ django-admin makemessages -l fr
    processing locale fr

The above command will discover all strings marked as translatable within your installed apps and create one language file for each app for french translation. For instance, if you have only one app `myapp` containing translatable strings, this will create a file `myapp/locale/fr/LC_MESSAGES/django.po`. This file may look like the following:

    # SOME DESCRIPTIVE TITLE
    # Copyright (C) YEAR THE PACKAGE'S COPYRIGHT HOLDER
    # This file is distributed under the same license as the PACKAGE package.
    # FIRST AUTHOR <EMAIL@ADDRESS>, YEAR
    #
    #, fuzzy
    msgid ""
    msgstr ""
    "Project-Id-Version: PACKAGE VERSION\n"
    "Report-Msgid-Bugs-To: \n"
    "POT-Creation-Date: 2016-07-24 14:01+0200\n"
    "PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\n"
    "Last-Translator: FULL NAME <EMAIL@ADDRESS>\n"
    "Language-Team: LANGUAGE <LL@li.org>\n"
    "Language: \n"
    "MIME-Version: 1.0\n"
    "Content-Type: text/plain; charset=UTF-8\n"
    "Content-Transfer-Encoding: 8bit\n"

    #: myapp/models.py:22
    msgid "user"
    msgstr ""

    #: myapp/models.py:39
    msgid "A user already exists with this email address."
    msgstr ""

    #: myapp/templates/myapp/register.html:155
    #, python-format
    msgid ""
    "By signing up, you accept our <a href=\"%(terms_url)s\" "
    "target=_blank>Terms of services</a>."
    msgstr ""

You will first have to fill in the placeholders (emphasized with uppercases). Then translate the strings. `msgid` is the string marked as translatable in your code. `msgstr` is where you have to write the translation of the string right above.

When a string contains placeholders, you will have to include them in your translation as well. For instance, you will translate the latest message as the following:

    #: myapp/templates/myapp/register.html:155
    #, python-format
    msgid ""
    "By signing up, you accept our <a href=\"%(terms_url)s\" "
    "target=_blank>Terms of services</a>."
    msgstr ""
    "En vous inscrivant, vous acceptez nos <a href=\"%(terms_url)s\" "
    "target=_blank>Conditions d'utilisation</a>"

Once your translation file is completed, you will have to compile the `.po` files into `.mo` files. This is done by calling the `compilemessages` management command:

    $ django-admin compilemessages

That's it, now translations are available.

To update your translation files when you make changes to your code, you can rerun `django-admin makemessages -l fr`. This will update `.po` files, keeping your existing translations and adding the new ones. Deleted strings will still be available in comments. To update `.po` files for all languages, run `django-admin makemessages -a`. Once your `.po` files are updated, don't forget to run `django-admin compilemessages` again to generate `.mo` files.

## Noop use case
`(u)gettext_noop` allows you to mark a string as translatable without actually translating it.

A typical use case is when you want to log a message for developers (in English) but also want to display it to the client (in the requested language). **You can pass a variable to `gettext`, but its content won't be discovered as a translatable string because it is, per definition, variable.**.

    # THIS WILL NOT WORK AS EXPECTED
    import logging 
    from django.contrib import messages

    logger = logging.getLogger(__name__)

    error_message = "Oops, something went wrong!"
    logger.error(error_message)
    messages.error(request, _(error_message))

The error message won't appear in the `.po` file and you will have to remember it exists to add it manually. To fix this, you can use `gettext_noop`.

    error_message = ugettext_noop("Oops, something went wrong!")
    logger.error(error_message)
    messages.error(request, _(error_message))

Now the string `"Oops, something went wrong!"` will be discovered and available in the `.po` file when generated. And the error will still be logged in English for developers.

## Common pitfalls
## fuzzy translations

Sometimes `makemessages` may think that the string it found for translation is somewhat similar to already existing translation. It will when mark it in the `.po` file with a special `fuzzy` comment like this:

    #: templates/randa/map.html:91
    #, fuzzy
    msgid "Country"
    msgstr "Länderinfo"

Even if translation is correct or you updated it to correct one it will not be used to translate your project unless you remove `fuzzy` comment line.

## Multiline strings

`makemessages` parses files in various formats, from plain text to python code and it is not designed to follow every possible rule for having multi-line strings in those formats. Most of the time it will work just fine with single line strings but if you have construction like this:

    translation = _("firstline"
    "secondline"
    "thirdline")

It will only pick up `firstline` for translation. Solution for this is to avoid using multiline strings when possible.

