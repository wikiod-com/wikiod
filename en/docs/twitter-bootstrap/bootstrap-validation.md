---
title: "Bootstrap Validation"
slug: "bootstrap-validation"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

* This validation technique can only be used on inputs that are within a form.

* Properties must have at least one validation requirement to show highlighting on a failed `onSubmit()` validation. Data types (other than string) have a hidden data type requirement, so do not require an explicit data annotation. Strings do not have this, so to force a validation check along with the other fields, add the data annotation `[MinLengthAttribute(0)]`.

## Using ASP.NET MVC and Data Annotations
Add the following to Web.config (in Views folder), within `<appSettings>`:

    <add key="ClientValidationEnabled" value="true"/>
    <add key="UnobtrusiveJavaScriptEnabled" value="true"/>

Add the jqueryval bundle to BundleConfig.cs:

    bundles.Add(new ScriptBundle("~/bundles/jqueryval").Include(
            "~/Scripts/jqueryval/jquery.validate*"));

Add the following to all pages that need validation (or _Layout.cshml):

    <!-- Reference to the jqueryval bundle -->
    @Scripts.Render("~/bundles/jqueryval")

    <!-- jQuery to apply bootstrap validation classes and glyphicons to inputs -->
    <script type="text/javascript">
        $.validator.setDefaults({
            highlight: function (element) {
                $(element).closest('.form-group').removeClass('has-success has-feedback').addClass('has-error has-feedback'); // red highlighting
                $(element).closest('.form-group').find('.form-control-feedback').removeClass('glyphicon-ok').addClass('glyphicon-remove'); // red cross glyphicon
            },
            unhighlight: function (element) {
                $(element).closest('.form-group').removeClass('has-error has-feedback').addClass('has-success has-feedback'); // green highlighting
                $(element).closest('.form-group').find('.form-control-feedback').removeClass('glyphicon-remove').addClass('glyphicon-ok'); // green tick glyphicon
            }
        });
    </script>

Add data annotations to the relevant fields in the model:

    using System.ComponentModel.DataAnnotations;

    [Required(ErrorMessage = "This field is required.")

In the view, add the following to each input that needs validating:

    <!-- Validation messages -->
    <div class="text-danger">@Html.ValidationMessageFor(m => m.SomeField)</div>

    <!-- Bootstrap feedback span: -->
    <span class="glyphicon form-control-feedback"></span>

Add the following to the relevant controller action to add server-side validation:

    if (!ModelState.IsValid)
    {
        return View(model);
    }
    else
    {
        // continue with action
    }

---
## Example input that requires validation

Model:

    [Required(ErrorMessage = "This field is required.")
    [StringLength(maximumLength: 10, ErrorMessage = "This field must be 10 characters or less.")]
    public string SomeRequiredField { get; set; }


View:

    <div class="form-group has-feedback">
        <div class="col-md-4">
            @Html.LabelFor(m => m.SomeRequiredField, new { @class = "control-label" })
        </div>
        <div class="col-md-8">
            @Html.TextBoxFor(m => m.SomeRequiredField, new { @class = "form-control" })
            <div class="text-danger">@Html.ValidationMessageFor(m => m.SomeRequiredField)</div>
            <span class="glyphicon form-control-feedback"></span>
        </div>
    </div>

---
## Optional

Add the following jQuery to validate inputs on blur, as well as on submit:

    $('input').on('blur', function () {
        $(this).valid();
    });

