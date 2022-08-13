---
title: "Forms"
slug: "forms"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Syntax
 - Form::create($this, \_\_FUNCTION\_\_, $fields, $actions, $validator) // standard form creation
 - Form::create(...)->addExtraClass('my-css-class another-class') // add CSS classes to your Form
 - Form::create(...)->loadDataFrom(Member::get()->byID(1)); // populate a form with the details of an object

## Creating a Form
Here is a basic example form with one required text field and one submit button, which submits to a custom function:

    class Page_Controller extends ContentController {

        private static $allowed_actions = array(
            'ExampleForm'
        );

        public function ExampleForm() {
            $fields = FieldList::create(
                TextField::create('Name', 'Your Name')
            );

            $actions = FieldList::create(
                FormAction::create('doExampleFormAction', 'Go')
            );

            $requiredFields = RequiredFields::create('Name');

            $form = Form::create(
                $this,
                'ExampleForm',
                $fields,
                $actions,
                $requiredFields
            );

            return $form;
        }

        public function doExampleFormAction($data, Form $form) {
            $form->sessionMessage('Hello '. $data['Name'], 'success');

            return $this->redirectBack();
        }
    }

To display this form we add `$ExampleForm` to our page template:

    $ExampleForm

## Creating a simple AJAX Form
SilverStripe has reasonably good support for submitting form data using AJAX requests. Below is example code of how to set up a basic Form that accepts submissions by both AJAX and traditional default browser behaviour (as is good practice).

## Adding the form to our controller

First we need to define our form; your `Page_Controller` should look something like this:

```php
class Page_Controller extends ContentController {

    /**
     * A list of "actions" (functions) that are allowed to be called from a URL
     *
     * @var array
     * @config
     */
    private static $allowed_actions = array(
        'Form',
        'complete',
    );

    /**
     * A method to return a Form object to display in a template and to accept form submissions
     *
     * @param $request SS_HTTPRequest
     * @return Form
     */
    public function Form($request) {
        // include our javascript in the page to enable our AJAX behaviour
        Requirements::javascript('framework/thirdparty/jquery/jquery.js');
        Requirements::javascript('mysite/javascript/ajaxforms.js');
        //create the fields we want
        $fields = FieldList::create(
            TextField::create('Name'),
            EmailField::create('Email'),
            TextareaField::create('Message')
        );
        //create the button(s) we want
        $buttons = FieldList::create(
            FormAction::create('doForm', 'Send')
        );
        //add a validator to make sure the fields are submitted with values
        $validator = RequiredFields::create(array(
            'Name',
            'Email',
            'Message',
        ));
        //construct the Form
        $form = Form::create(
            $this,
            __FUNCTION__,
            $fields,
            $buttons,
            $validator
        );

        return $form;
    }

    /**
     * The form handler, this runs after a form submission has been successfully validated
     *
     * @param $data array RAW form submission data - don't use
     * @param $form Form The form object, populated with data
     * @param $request SS_HTTPRequest The current request object
     */
    public function doForm($data, $form, $request) {
        // discard the default $data because it is raw submitted data
        $data = $form->getData();

        // Do something with the data (eg: email it, save it to the DB, etc

        // send the user back to the "complete" action
        return $this->redirect($this->Link('complete'));
    }

    /**
     * The "complete" action to send users to upon successful submission of the Form.
     *
     * @param $request SS_HTTPRequest The current request object
     * @return string The rendered response
     */
    public function complete($request) {
        //if the request is an ajax request, then only render the include
        if ($request->isAjax()) {
            return $this->renderWith('Form_complete');
        }
        //otherwise, render the full HTML response
        return $this->renderWith(array(
            'Page_complete',
            'Page',
        ));
    }

}
```

> Adding these functions to `Page_Controller` will make them available on **all** page types - this may not be desired and you should consider if it would be more appropriate to create a new page type (such as ContactPage) to have this form on

Here we've defined methods to:

 - Create the `Form`
 - A form handler (to save or send the submissions somewhere, this runs after the `Form` has successfully validated it's data)
 - A `complete` action, which  the user will be sent to after successfully completing the form submission.

## Customising out templates for easy content replacement

Next we need to set up our templates - modify your Layout/Page.ss file:

```html
<% include SideBar %>
<div class="content-container unit size3of4 lastUnit">
    <article>
        <h1>$Title</h1>
        <div class="content">$Content</div>
    </article>
    <div class="form-holder">
        $Form
    </div>
        $CommentsForm
</div>
```

This is taken from the default simple theme, with a minor addition that the form is now wrapped in a `<div class="form-holder">` so that we can easily replace the form with a success message.

We also need to create a `Layout/Page_complete.ss` template - this will be the same as above except the `form-holder` `div` will be:

```html
<div class="form-holder">
    <% include Form_complete %>
</div>
```

Next create the `Includes/Form_complete` include - it's important to use an include so that we can render **just** this section of the page for our responses to AJAX requests:

```html
<h2>Thanks, we've received your form submission!</h2>
<p>We'll be in touch as soon as we can.</p>
```

# Creating the javascript form listener

Finally, we need to write our javascript to send the form by AJAX instead of the default browser behaviour (place this in mysite/javascript/ajaxform.js):

```js
(function($) {
    $(window).on('submit', '.js-ajax-form', function(e) {
        var $form = $(this);
        var formData = $form.serialize();
        var formAction = $form.prop('action');
        var formMethod = $form.prop('method');
        var encType = $form.prop('enctype');

        $.ajax({
            beforeSend: function(jqXHR,settings) {
                if ($form.prop('isSending')) {
                    return false;
                }
                $form.prop('isSending',true);
            },
            complete: function(jqXHR,textStatus) {
                $form.prop('isSending',false);
            },
            contentType: encType,
            data: formData,
            error: function(jqXHR, textStatus, errorThrown) {
                window.location = window.location;
            },
            success: function(data, textStatus, jqXHR) {
                var $holder = $form.parent();
                $holder.fadeOut('normal',function() {
                    $holder.html(data).fadeIn();
                });
            },
            type: formMethod,
            url: formAction
        });
        e.preventDefault();
    });
})(jQuery);
```

This javascript will submit the form using AJAX and on completion it will fade the form out and replace it with the response and fade it back in.

# For advanced users:

With this example all forms on your site will be "ajaxified", this may be acceptable, but sometimes you need some control over this (for example, search forms wouldn't work well like this). Instead, you can modify the code slightly to only look for forms with a certain class.

Amend the `Form` method on `Page_Controller` like so:

```php
public function Form() {
    ...
    $form->addExtraClass('js-ajax-form');
    return $form;
}
```

Amend the javascript like so:

```js
$(window).on('submit', '.js-ajax-form', function(e) {
    ...
})(jQuery);
```

Only forms with the class `js-ajax-form` will now act in this way.

