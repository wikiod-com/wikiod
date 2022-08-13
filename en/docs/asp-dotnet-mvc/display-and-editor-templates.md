---
title: "Display and Editor templates"
slug: "display-and-editor-templates"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

When dealing with objects in an MVC app, if any object should be shown in multiple places with the same format, we'd need some kind of standardized layout. ASP.NET MVC has made this kind of standardization easy to do with the inclusion of display and editor templates. In short, display and editor templates are used to standardize the layout shown to the user when editing or displaying certain types or classes.

## Display Template
Model:
   
     public class User  
     {
        public int ID { get; set; }
        public string FirstName { get; set; }
        public DateTime DateOfBirth { get; set; }
     }

If we want to display the users in different Views, it would be better to create a standardized layout for these users wherever they need to be displayed. We can accomplish this using display templates.

A display template is simply a partial view that is model-bound to the object it wants to display, and exists in the `Views/Shared/DisplayTemplates` folder (though you can also put it in `Views/ControllerName/DisplayTemplates`). Further, **the name of the view (by default) should be the name of the object you want to use it as the template for**.

> Views/Shared/DisplayTemplates/User.cshtml

  

     @model TemplatesDemo.Models.User
        
    <div style="padding-bottom: 10px">
        <p><strong>ID:</strong> @Html.DisplayFor(m => m.ID)</p>
        <p><strong>Name:</strong> @Html.DisplayFor(m => m.FirstName)</p>
        <p><strong>Date of Birth:</strong> @Html.DisplayFor(m => m.DateOfBirth)</p>
    </div>
    <hr/>

Now, if we want to display all the users from database and show them in different Views we can simply send the list of users to the View and and use the Display Template to show them. We can use one of two methods to do that:

    Html.DisplayFor()
    Html.DisplayForModel()

`DisplayFor` call the display template for the type of the property selected (e.g. `Html.DisplayFor(x => x.PropertyName)`. `DisplayForModel` calls the display template for the `@model` of the view

**View**

  

    @model IEnumerable<TemplatesDemo.Models.User>
    @{
        ViewBag.Title = "Users";
    }
    
    <h2>Users</h2>
    
    @Html.DisplayForModel()



## Editor Template
Display Templates can be used to standardize the layout of an object, so let's now see how we can do the same thing for these objects when editing them. Just like display templates, there's two ways to call editor templates for a given type:

    Html.EditorFor()
    Html.EditorForModel()

Editor templates, similarly to display templates, need to exist in either **Views/Shared/EditorTemplates** or **Views/ControllerName/EditorTemplates**. For this demo, we'll be creating them in the Shared folder. Again, **the name of the view (by default) should be the name of the object you want to use it as the template for.**

**Model**

    public class User
        {
            public int Id { get; set; }
            public string Name { get; set; }
            public DateTime DateOfBirth { get; set; }
            public Roles Roles { get; set; }
            public int RoleId { get; set; }
        }

     public class Roles
        {
            public int Id { get; set; }
            public string Role { get; set; }
        }



Say we want to be able edit any user from the database in multiple views. We will use a **ViewModel** for this purpose.

**ViewModel**

    public class UserEditorViewModel
        {
            public User User { get; set; }
            public IEnumerable<Roles> Roles { get; set; }
        }

Using this **ViewModel**, we will create an **Editor Template** 

> Views/Shared/EditorTemplates/UserEditorViewModel.cshtml

    @model TemplatesDemo.Models.UserEditorViewModel
    
    <div class="form-group">
        @Html.DisplayNameFor(m => m.User.Id)
        @Html.EditorFor(m => m.User.Id)
    </div>
    <div class="form-group">
        @Html.DisplayNameFor(m => m.User.Name)
        @Html.EditorFor(m => m.User.Name)
    </div>
    <div class="form-group">
        @Html.DisplayNameFor(m => m.User.DateOfBirth)
        @Html.EditorFor(m => m.User.DateOfBirth)
    </div>
    <div class="form-group">
        @Html.DisplayNameFor(m => m.User.Roles.Role)
        @Html.DropDownListFor(m => m.User.RoleId, new SelectList(Model.Roles,"Id","Role"))
    </div>

We will get the desired user and the list of available roles and bind them in the viewModel **UserEditorViewModel** in the Controller Action and send the viewModel to the view. For simplicity, I am initiating the viewModel from the Action

****Action****

     public ActionResult Editor()
            {
                var viewModel = new UserEditorViewModel
                {
                    User = new User
                    {
                        Id = 1,
                        Name = "Robert",
                        DateOfBirth = DateTime.Now,
                        RoleId = 1
                    },
                    Roles = new List<Roles>()
                    {
                        new Roles
                        {
                            Id = 1,
                            Role = "Admin"
                        },
                        new Roles
                        {
                            Id = 2,
                            Role = "Manager"
                        },
                        new Roles
                        {
                            Id = 3,
                            Role = "User"
                        }
                    }
                };
    
                return View(viewModel);
            }


We can use the created **Editor Template** in any view we wish 

**View**

    @model TemplatesDemo.Models.UserEditorViewModel
        
    @using (Html.BeginForm("--Your Action--", "--Your Controller--"))
    {
        @Html.EditorForModel()
        <input type="submit" value="Save" />
    }

