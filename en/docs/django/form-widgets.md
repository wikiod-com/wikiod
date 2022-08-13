---
title: "Form Widgets"
slug: "form-widgets"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

## Simple text input widget
The most simple example of widget is custom text input. For instance, to create an `<input type="tel">`, you have to subclass `TextInput` and set `input_type` to `'tel'`.

    from django.forms.widgets import TextInput

    class PhoneInput(TextInput):
        input_type = 'tel'

## Composite widget
You can create widgets composed of multiple widgets using `MultiWidget`.

    from datetime import date

    from django.forms.widgets import MultiWidget, Select
    from django.utils.dates import MONTHS

    class SelectMonthDateWidget(MultiWidget):
        """This widget allows the user to fill in a month and a year.

        This represents the first day of this month or, if `last_day=True`, the
        last day of this month.
        """

        default_nb_years = 10

        def __init__(self, attrs=None, years=None, months=None, last_day=False):
            self.last_day = last_day
 
            if not years:
                this_year = date.today().year
                years = range(this_year, this_year + self.default_nb_years)
            if not months:
                months = MONTHS

            # Here we will use two `Select` widgets, one for months and one for years
            widgets = (Select(attrs=attrs, choices=months.items()),
                       Select(attrs=attrs, choices=((y, y) for y in years)))
            super().__init__(widgets, attrs)

        def format_output(self, rendered_widgets):
            """Concatenates rendered sub-widgets as HTML"""
            return (
                '<div class="row">'
                '<div class="col-xs-6">{}</div>'
                '<div class="col-xs-6">{}</div>'
                '</div>'
            ).format(*rendered_widgets)

        def decompress(self, value):
            """Split the widget value into subwidgets values.
            We expect value to be a valid date formated as `%Y-%m-%d`.
            We extract month and year parts from this string.
            """
            if value:
                value = date(*map(int, value.split('-')))
                return [value.month, value.year]
            return [None, None]

        def value_from_datadict(self, data, files, name):
            """Get the value according to provided `data` (often from `request.POST`)
            and `files` (often from `request.FILES`, not used here)
            `name` is the name of the form field.

            As this is a composite widget, we will grab multiple keys from `data`.
            Namely: `field_name_0` (the month) and `field_name_1` (the year).
            """
            datalist = [
                widget.value_from_datadict(data, files, '{}_{}'.format(name, i))
                for i, widget in enumerate(self.widgets)]
            try:
                # Try to convert it as the first day of a month.
                d = date(day=1, month=int(datelist[0]), year=int(datelist[1]))
                if self.last_day:
                    # Transform it to the last day of the month if needed
                    if d.month == 12:
                        d = d.replace(day=31)
                    else:
                        d = d.replace(month=d.month+1) - timedelta(days=1)
            except (ValueError, TypeError):
                # If we failed to recognize a valid date
                return ''
            else:
                # Convert it back to a string with format `%Y-%m-%d`
                return str(d)

