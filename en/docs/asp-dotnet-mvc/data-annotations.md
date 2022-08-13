---
title: "Data annotations"
slug: "data-annotations"
draft: false
images: []
weight: 9889
type: docs
toc: true
---

We can add validations to our application by adding Data Annotations to our model classes. Data Annotations allow us to describe the rules we want applied to our model properties, and ASP.NET MVC will take care of enforcing them and displaying appropriate messages to  users.

## Remote validation
Remote Validation used to check whether the content enter in the input control is valid or not by sending an ajax request to server side to check it.
----------
**Working**

The `RemoteAttribute` works by making an AJAX call from the client to a controller action with the value of the field being validated. The controller action then returns a `JsonResult` response indicating validation success or failure. Returning `true` from your action indicates that validation passed. Any other value indicates failure. If you return `false`, the error message specified in the attribute is used. If you return anything else such as a string or even an integer, it will be displayed as the error message. Unless you need your error message to be dynamic, it makes sense to return true or false and let the validator use the error message specified on the attribute.

**ViewModel**

    public class ViewModel
    {
        [Remote("IsEmailAvailable", "Group", HttpMethod = "POST", ErrorMessage = "Email already exists. Please enter a different email address.")]
        public string Email{ get; set; }
    }


**Controller**
   
    [HttpPost]
    public JsonResult IsEmailAvailable(string Email)
    {
        // Logic to check whether email is already registered or Not.
        var emailExists = IsEmailRegistered();
        return Json(!emailExists);         
    } 


[Live Demo Fiddle][1]

You can pass additional properties of the model to the controller method using the `AdditionalFields` property of `RemoteAttribute`. A typical scenario would be to pass the ID property of the model in an 'Edit' form, so that the controller logic can ignore values for the existing record.

**Model**

      public int? ID { get; set; }
      [Display(Name = "Email address")]
      [DataType(DataType.EmailAddress)]
      [Required(ErrorMessage = "Please enter you email address")]
      [Remote("IsEmailAvailable", HttpMethod="Post", AdditionalFields="ID", ErrorMessage = "Email already exists. Please enter a different email address.")]  
      public string Email { get; set; }


**Controller**

    [HttpPost]
    public ActionResult Validate(string email, int? id)
    {
        if (id.HasValue)
        {
            return Json(!db.Users.Any(x => x.Email == email && x.ID != id);
        }
        else
        {
            return Json(!db.Users.Any(x => x.Email == email);
        }
    }

[Working Demo - Additional Fields][2]


**Additional Note**

The default error message is understandably vague, so always remember to override the default error message when using the `RemoteAttribute`.


  [1]: https://dotnetfiddle.net/HDoyF3
  [2]: https://dotnetfiddle.net/59L3Ca

## Basic validation attributes used in ViewModel

**Model**
----

    using System.ComponentModel.DataAnnotations;

    public class ViewModel
    {
        [Required(ErrorMessage="Name is required")] 
        public string Name { get; set; }
    
        [StringLength(14, MinimumLength = 14, ErrorMessage = "Invalid Phone Number")]
        [Required(ErrorMessage="Phone Number is required")] 
        public string PhoneNo { get; set; }
    
        [Range(typeof(decimal), "0", "150")]
        public decimal? Age { get; set; }
    
        [RegularExpression(@"^\d{5}(-\d{4})?$", ErrorMessage = "Invalid Zip Code.")]
        public string ZipCode {get;set;}
    
        [EmailAddress(ErrorMessage = "Invalid Email Address")] 
        public string Email { get; set; }

        [Editable(false)] 
        public string Address{ get; set; }
    }

**View**
----
    
    // Include Jquery and Unobstructive Js here for client side validation
    
    @using (Html.BeginForm("Index","Home") { 

        @Html.TextBoxFor(model => model.Name) 
        @Html.ValidationMessageFor(model => model.Name)
    
        @Html.TextBoxFor(model => model.PhoneNo)
        @Html.ValidationMessageFor(model => model.PhoneNo)
    
        @Html.TextBoxFor(model => model.Age)
        @Html.ValidationMessageFor(model => model.Age)
    
        @Html.TextBoxFor(model => model.ZipCode)
        @Html.ValidationMessageFor(model => model.ZipCode)
    
        @Html.TextBoxFor(model => model.Email)
        @Html.ValidationMessageFor(model => model.Email)

        @Html.TextBoxFor(model => model.Address)
        @Html.ValidationMessageFor(model => model.Address)

        <input type="submit" value="submit" />
    }

**Controller**
----

    public ActionResult Index(ViewModel _Model) 
    { 
        // Checking whether the Form posted is valid one. 
        if(ModelState.IsValid) 
        { 
            // your model is valid here.
            // perform any actions you need to, like database actions,
            // and/or redirecting to other controllers and actions.
        }
        else 
        {
            // redirect to same action
            return View(_Model);
        } 
    }

## Custom Validation Attribute
When it comes to validate some rules which are not generic data validation e.g ensuring a field is required or some range of values but they are specific to your business logic then you can create your own ***Custom Validator***. To create a custom validation attribute, you just need to `inherit` **`ValidationAttribute`** class and `override` its **`IsValid`** method. The **`IsValid`** method takes two parameters, the first is an `object` named as `value` and the second is a `ValidationContext object` named as `validationContext`. **`Value`** refers to the actual value from the field that your custom validator is going to validate.

Suppose you want to validate `Email` through `Custom Validator`

    public class MyCustomValidator : ValidationAttribute
    {
        private static string myEmail= "admin@dotnetfiddle.net";

        protected override ValidationResult IsValid(object value, ValidationContext validationContext)
        {
            string Email = value.ToString();
            if(myEmail.Equals(Email))
                return new ValidationResult("Email Already Exist");
            return ValidationResult.Success;
        }
    }

    public class SampleViewModel
    {
        [MyCustomValidator]
        [Required]
        public string Email { get; set; }

        public string Name { get; set; }
    }

>! <h2><kbd>[Here is its DotNetFiddle Demo](https://dotnetfiddle.net/s5Mi1x)</kbd></h2>

## StringLengthAttribute


## Compare Attribute


## RequiredAttribute
The [`Required`](https://msdn.microsoft.com/library/system.componentmodel.dataannotations.requiredattribute(v=vs.110).aspx) attribute specifies that a property is required. An error message can be specified on using the [`ErrorMessage`](https://msdn.microsoft.com/library/system.componentmodel.dataannotations.validationattribute.errormessage(v=vs.110).aspx) property on the attribute.

First add the namespace:

    using System.ComponentModel.DataAnnotations;

And apply the attribute on a property.

    public class Product
    {
       [Required(ErrorMessage = "The product name is required.")]
       public string Name { get; set; }
    
       [Required(ErrorMessage = "The product description is required.")]
       public string Description { get; set; }
    }

It is also possible to use resources in the error message for globalized applications. In this case, the [`ErrorMessageResourceName`](https://msdn.microsoft.com/library/system.componentmodel.dataannotations.validationattribute.errormessageresourcename(v=vs.110).aspx) must be specified with the resource key of the resource class (`resx` file) that must be setted on the [`ErrorMessageResourceType`](https://msdn.microsoft.com/library/system.componentmodel.dataannotations.validationattribute.errormessageresourcetype(v=vs.110).aspx):

    public class Product
    {
       [Required(ErrorMessageResourceName = "ProductNameRequired", 
                 ErrorMessageResourceType = typeof(ResourceClass))]
       public string Name { get; set; }

       [Required(ErrorMessageResourceName = "ProductDescriptionRequired", 
                 ErrorMessageResourceType = typeof(ResourceClass))]
       public string Description { get; set; }
    }


## Range Attribute


## RegularExpression Attribute
The [`[RegularExpression]`](https://msdn.microsoft.com/en-us/library/system.componentmodel.dataannotations.regularexpressionattribute(v=vs.110).aspx) attribute can decorate any properties or public fields and specifies a regular expression that must be matched for the property be considered valid. 

    [RegularExpression(validationExpression)]
    public string Property { get; set; }

Additionally, it accepts an optional `ErrorMessage` property that can be used to set the message received by the user when invalid data is entered :

    [RegularExpression(validationExpression, ErrorMessage = "{your-error-message}")]
    public string Property { get; set; }

**Example(s)**

    [RegularExpression(@"^[a-z]{8,16}?$", ErrorMessage = "A User Name must consist of 8-16 lowercase letters")]
    public string UserName{ get; set; }
    [RegularExpression(@"^\d{5}(-\d{4})?$", ErrorMessage = "Please enter a valid ZIP Code (e.g. 12345, 12345-1234)")]
    public string ZipCode { get; set; }

## EDMx model - Data Annotation
Edmx model internel

    public partial class ItemRequest
    {
        public int RequestId { get; set; }
        //...
    }

Adding data annotation to this - if we modify this model directly, when a update to the model is made, the changes are lost . so

To add a attribute in this case 'Required'

Create a new class - any name
Then

    using System.ComponentModel;
    using System.ComponentModel.DataAnnotations;
    
    //make sure the namespace is equal to the other partial class ItemRequest
    namespace MvcApplication1.Models 
    {
        [MetadataType(typeof(ItemRequestMetaData))]
        public partial class ItemRequest
        {
        }
    
        public class ItemRequestMetaData
        {
            [Required]
            public int RequestId {get;set;}
    
            //...
        }
    }

or

    using System.Collections.Generic;
    using System.ComponentModel.DataAnnotations;
    
    namespace YourApplication.Models
    {
        public interface IEntityMetadata
        {
            [Required]
            Int32 Id { get; set; }
        }
    
        [MetadataType(typeof(IEntityMetadata))]
        public partial class Entity : IEntityMetadata
        {
            /* Id property has already existed in the mapped class */
        }
    }

## Data annotations for Database first implementation (model code auto-generated)
    [MetadataType(typeof(RoleMetaData))]
    public partial class ROLE
    {
    }

    public class RoleMetaData
    {
        [Display(Name = "Role")]
        public string ROLE_DESCRIPTION { get; set; }

        [Display(Name = "Username")]
        public string ROLE_USERNAME { get; set; }
    }

If you used database-first and your model code was auto-generated, this message will appear above your model code:

> This code was generated from a template.
> Manual changes to this file may cause unexpected behavior in
> your application.
Manual changes to this file will be
> overwritten if the code is regenerated

If you want to use data-annotations and you don't want them to be over-written if you refresh the edmx just add another a partial class to your model folder that looks like the example above.

