---
title: "Model validation"
slug: "model-validation"
draft: false
images: []
weight: 9923
type: docs
toc: true
---

## Remove an object from validation
Say you have the following model:

    public class foo
    {
        [Required]
        public string Email { get; set; }

        [Required]
        public string Password { get; set; }

        [Required]
        public string FullName { get; set; }
    }

But you want to exclude FullName from the modelvalidation because you are using the model also in a place where FullName is not filled in, you can do so in the following way:

    ModelState.Remove("FullName");

## Custom Error Messages
If you want to provide Custom Error Messages you would do it like this:
    
    public class LoginViewModel
    {
        [Required(ErrorMessage = "Please specify an Email Address")]
        [EmailAddress(ErrorMessage = "Please specify a valid Email Address")]
        public string Email { get; set; }
        
        [Required(ErrorMessage = "Type in your password")]
        public string Password { get; set; }
    }

When your Error Messages are in a ResourceFile (.resx) you have to specify the ResourceType and the ResourceName:

    public class LoginViewModel
    {
        [Required(ErrorMessageResourceType = typeof(ErrorResources), ErrorMessageResourceName = "LoginViewModel_RequiredEmail")]
        [EmailAddress(ErrorMessageResourceType = typeof(ErrorResources), ErrorMessageResourceName = "LoginViewModel_ValidEmail")]
        public string Email { get; set; }
        
        [Required(ErrorMessageResourceType = typeof(ErrorResources), ErrorMessageResourceName = "LoginViewModel_RequiredPassword")]
        public string Password { get; set; }
    }







## Validate Model in ActionResult
    [HttpPost]
    public ActionResult ContactUs(ContactUsModel contactObject)
    {
        // This line checks to see if the Model is Valid by verifying each Property in the Model meets the data validation rules
        if(ModelState.IsValid)
        {
        }
        return View(contactObject);
    }


The model class

    public class ContactUsModel
    {
        [Required]
        public string Name { get; set; }
        [Required]
        [EmailAddress] // The value must be a valid email address
        public string Email { get; set; }
        [Required]
        [StringLength(500)] // Maximum length of message is 500 characters
        public string Message { get; set; }
    }



## Creating Custom Error Messages in Model and in Controller
Let's say that you have the following class:

    public class PersonInfo
    {
        public int ID { get; set; }

        [Display(Name = "First Name")]
        [Required(ErrorMessage = "Please enter your first name!")]
        public string FirstName{ get; set; }

        [Display(Name = "Last Name")]
        [Required(ErrorMessage = "Please enter your last name!")]
        public string LastName{ get; set; }

        [Display(Name = "Age")]
        [Required(ErrorMessage = "Please enter your Email Address!")]
        [EmailAddress(ErrorMessage = "Invalid Email Address")]
        public string EmailAddress { get; set; }
    }

These custom error messages will appear if your `ModelState.IsValid` returns false.

But, you as well as I know that there can only be 1 email address per person, or else you will be sending emails to potentially wrong people and/or multiple people.  This is where checking in the controller comes into play.  So let's assume people are creating accounts for you to save via the Create Action.

    [HttpPost]
    [ValidateAntiForgeryToken]
    public ActionResult Create([Bind(Include = "ID, FirstName, LastName, EmailAddress")] PersonInfo newPerson)
    {
        if(ModelState.IsValid) // this is where the custom error messages on your model will display if return false
        {
            if(database.People.Any(x => x.EmailAddress == newPerson.EmailAddress))  // checking if the email address that the new person is entering already exists.. if so show this error message
            {
                ModelState.AddModelError("EmailAddress", "This email address already exists! Please enter a new email address!");
                return View(newPerson);
            }
            
            db.Person.Add(newPerson);
            db.SaveChanges():
            return RedirectToAction("Index");
        }
    
        return View(newPerson);
    }


I hope this is able to help somebody!

## Model Validation in JQuery.
In cases where you need to ensure model validation using Jquery, .valid() function can be used.

The model class fields

    [Required]
    [Display(Name = "Number of Hospitals")]
    public int Hospitals{ get; set; }
    [Required]
    [Display(Name = "Number of Beds")]
    public int Beds { get; set; }

The View code

    @using (Html.BeginForm(new {id = "form1", @class = "form-horizontal" }))
    {

    <div class="divPanel">
      <div class="row">
        <div class="col-md-3">                
                @Html.LabelFor(m => m.Hospitals)
                @Html.TextBoxFor(m => m.Hospitals, new { @class = "form-control", @type = "number"})
                @Html.ValidationMessageFor(m => m.Hospitals)

        </div>
        <div class="col-md-3">

                @Html.LabelFor(m => m.Beds)
                @Html.TextBoxFor(m => m.Beds, new { @class = "form-control", @type = "number"})
                @Html.ValidationMessageFor(m => m.Beds)
        </div>
    <div class="col-md-3">             
            <button type=button  class="btn btn-primary" id="btnCalculateBeds"> Calculate Score</button>
        </div>
     </div>

      </div>
     }

The script for Validation check.

    $('#btnCalculateBeds').on('click', function (evt) {
    evt.preventDefault();

    if ($('#form1').valid()) {
    //Do Something.
    }
    }

Ensure that the `jquery.validate` and `jquery.validate.unobtrusive` files are present in the solution.

