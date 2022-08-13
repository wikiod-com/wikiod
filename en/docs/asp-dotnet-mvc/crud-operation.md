---
title: "CRUD operation"
slug: "crud-operation"
draft: false
images: []
weight: 9935
type: docs
toc: true
---

CRUD Operation refers to classic (create, read, update, delete) operations as it pertains to data.

In ASP MVC context there are several ways to CRUD your data using Models and subsequently views, Controllers.

One simple way is to make use of the scaffolding feature provided by the Visual studio templates and customize to your needs.

Please keep in mind that CRUD is very broadly defined and it has many variations to suit your requirements. For e.g. Database first, Entity first etc.

For simplicity sake, this CRUD operation uses a entity framework context in the controller. It is not a good practice, but it is beyond this topic's scope.
Click in [entity framework][1] if you want to learn more about it.


  [1]: https://www.wikiod.com/entity-framework

## Create - Controller Part
To implement the create functionality we need two actions: *GET* and *POST*. 

 1. The *GET* action used to return view which will show a form allowing user to input data using HTML elements. If there are some default values to be inserted before user adding any data, it should be assigned to the view model properties on this action.
     
 2. When the user fills the form and clicks the "Save" button we will be dealing with data from the form. Because of that now we need the *POST* action. This method will be responsible for managing data and saving it to database. In case of any errors, the same view returned with stored form data & error message explains what problem occurs after submit action.

 
We'll implement these two steps within two Create() methods within our controller class.

        // GET: Student/Create 
        // When the user access this the link ~/Student/Create a get request is made to controller Student and action Create, as the page just need to build a blank form, any information is needed to be passed to view builder
        public ActionResult Create()
        {
            // Creates a ViewResult object that renders a view to the response.
            // no parameters means: view = default in this case Create and model = null
            return View();
        }

        // POST: Student/Create        
        [HttpPost]
        // Used to protect from overposting attacks, see https://www.wikiod.com/asp-dotnet-mvc/htmlantiforgerytoken for details
        [ValidateAntiForgeryToken]
        // This is the post request with forms data that will be bind the action, if in the data post request have enough information to build a Student instance that will be bind
        public ActionResult Create(Student student)
        {
            try
            {
            //Gets a value that indicates whether this instance received from the view is valid.
                if (ModelState.IsValid)
                {
                    // Adds to the context
                    db.Students.Add(student);
                    // Persist the data 
                    db.SaveChanges();
                    // Returns an HTTP 302 response to the browser, which causes the browser to make a GET request to the specified action, in this case the index action.
                    return RedirectToAction("Index");
                }
            }
            catch 
            {
                // Log the error (uncomment dex variable name and add a line here to write a log).
                ModelState.AddModelError("", "Unable to save changes. Try again, and if the problem persists see your system administrator.");
            }
            // view = default in this case Create and model = student
            return View(student);
        }


  [1]: https://www.wikiod.com/asp-dotnet-mvc/htmlantiforgerytoken

## Create - View Part
    
    @model ContosoUniversity.Models.Student
    
    //The Html.BeginForm helper Writes an opening <form> tag to the response. When the user submits the form, the request will be processed by an action method.                 
    @using (Html.BeginForm()) 
    {
        //Generates a hidden form field (anti-forgery token) that is validated when the form is submitted.
        @Html.AntiForgeryToken()
        
    <div class="form-horizontal">
        <h4>Student</h4>
        <hr />

        //Returns an unordered list (ul element) of validation messages that are in the ModelStateDictionary object.
        @Html.ValidationSummary(true, "", new { @class = "text-danger" })

        <div class="form-group">
            //Returns an HTML label element and the property name of the property that is represented by the specified expression.            
            @Html.LabelFor(model => model.LastName, htmlAttributes: new { @class = "control-label col-md-2" })

            <div class="col-md-10">
                //Returns an HTML input element for each property in the object that is represented by the Expression expression.
                @Html.EditorFor(model => model.LastName, new { htmlAttributes = new { @class = "form-control" } })

                //Returns the HTML markup for a validation-error message for each data field that is represented by the specified expression.
                @Html.ValidationMessageFor(model => model.LastName, "", new { @class = "text-danger" })
            </div>
        </div>

        <div class="form-group">
            @Html.LabelFor(model => model.FirstMidName, htmlAttributes: new { @class = "control-label col-md-2" })
            <div class="col-md-10">
                @Html.EditorFor(model => model.FirstMidName, new { htmlAttributes = new { @class = "form-control" } })
                @Html.ValidationMessageFor(model => model.FirstMidName, "", new { @class = "text-danger" })
            </div>
        </div>

        <div class="form-group">
            @Html.LabelFor(model => model.EnrollmentDate, htmlAttributes: new { @class = "control-label col-md-2" })
            <div class="col-md-10">
                @Html.EditorFor(model => model.EnrollmentDate, new { htmlAttributes = new { @class = "form-control" } })
                @Html.ValidationMessageFor(model => model.EnrollmentDate, "", new { @class = "text-danger" })
            </div>
        </div>

        <div class="form-group">
            <div class="col-md-offset-2 col-md-10">
                <input type="submit" value="Create" class="btn btn-default" />
            </div>
        </div>
    </div>
    }

    <div>
        //Returns an anchor element (a element) the text is Back to List and action is Index
        @Html.ActionLink("Back to List", "Index")
    </div>



## Details - Controller part
  By the url `~/Student/Details/5` being: (~: site root, Student: Controller, Details: Action, 5: student id), it is possible to retrieve the student by its id. 
    

    // GET: Student/Details/5
        public ActionResult Details(int? id)
        {
            // it good practice to consider that things could go wrong so,it is wise to have a validation in the controller
            if (id == null)
            {
                // return a bad request
                return new HttpStatusCodeResult(HttpStatusCode.BadRequest);
            }
            Student student = db.Students.Find(id);
            if (student == null)
            {
                // if doesn't found return 404
                return HttpNotFound();
            }
            return View(student);
        }

## Edit - Controller part
     // GET: Student/Edit/5
     // It is receives a get http request for the controller Student and Action Edit with the id of 5
        public ActionResult Edit(int? id)
        {
             // it good practice to consider that things could go wrong so,it is wise to have a validation in the controller
            if (id == null)
            {
                // returns a bad request
                return new HttpStatusCodeResult(HttpStatusCode.BadRequest);
            }
            
            // It finds the Student to be edited.
            Student student = db.Students.Find(id);
            if (student == null)
            {
                // if doesn't found returns 404
                return HttpNotFound();
            }
            // Returns the Student data to fill out the edit form values.
            return View(student);
        }

This method is very similar to the details action method, which is a good candidate to a refactoring, but it out of scope of this topic. 

        // POST: Student/Edit/5
        [HttpPost]

        //used to To protect from overposting attacks more details see https://www.wikiod.com/asp-dotnet-mvc/htmlantiforgerytoken
        [ValidateAntiForgeryToken]

        //Represents an attribute that is used for the name of an action.
        [ActionName("Edit")]
        public ActionResult Edit(Student student)
        {
            try
            {
                //Gets a value that indicates whether this instance received from the view is valid.
                if (ModelState.IsValid)
                {
                    // Two thing happens here:
                    // 1) db.Entry(student) -> Gets a DbEntityEntry object for the student entity providing access to information about it and the ability to perform actions on the entity.
                    // 2) Set the student state to modified, that means that the student entity is being tracked by the context and exists in the database, and some or all of its property values have been modified.
                    db.Entry(student).State = EntityState.Modified;

                    // Now just save the changes that all the changes made in the form will be persisted.
                    db.SaveChanges();

                    // Returns an HTTP 302 response to the browser, which causes the browser to make a GET request to the specified action, in this case the index action.
                    return RedirectToAction("Index");
                }
            }
            catch
            {
                //Log the error add a line here to write a log.
                ModelState.AddModelError("", "Unable to save changes. Try again, and if the problem persists, see your system administrator.");
            }

            // return the invalid student instance to be corrected.
            return View(student);
        }


## Details - View part
    // Model is the class that contains the student data send by the controller and will be rendered in the view
    @model ContosoUniversity.Models.Student   
   
    <h2>Details</h2>
    
    <div>
       <h4>Student</h4>
    <hr />
    <dl class="dl-horizontal">
        <dt>
            //Gets the display name for the model.
            @Html.DisplayNameFor(model => model.LastName)
        </dt>

        <dd>
            //Returns HTML markup for each property in the object that is represented by the Expression expression.
            @Html.DisplayFor(model => model.LastName)
        </dd>

        <dt>
            @Html.DisplayNameFor(model => model.FirstMidName)
        </dt>

        <dd>
            @Html.DisplayFor(model => model.FirstMidName)
        </dd>

        <dt>
            @Html.DisplayNameFor(model => model.EnrollmentDate)
        </dt>

        <dd>
            @Html.DisplayFor(model => model.EnrollmentDate)
        </dd>
        <dt>
            @Html.DisplayNameFor(model => model.Enrollments)
        </dt>
        <dd>
            <table class="table">
                <tr>
                    <th>Course Title</th>
                    <th>Grade</th>
                </tr>
                @foreach (var item in Model.Enrollments)
                {
                    <tr>
                        <td>
                            @Html.DisplayFor(modelItem => item.Course.Title)
                        </td>
                        <td>
                            @Html.DisplayFor(modelItem => item.Grade)
                        </td>
                    </tr>
                }
            </table>
        </dd>
    </dl>
    </div>
    <p>
        //Returns an anchor element (a element) the text is Edit, action is Edit and the route value is the model ID property.
        @Html.ActionLink("Edit", "Edit", new { id = Model.ID }) |
        @Html.ActionLink("Back to List", "Index")
    </p>


## Delete  - Controller part
Is good practice to resist the temptation of doing the delete action in the get request. It would be a huge security error,  it has to be done always in the post method.
        
        // GET: Student/Delete/5
        public ActionResult Delete(int? id)
        {
            // it good practice to consider that things could go wrong so,it is wise to have a validation in the controller
            if (id == null)
            {
                // returns a bad request
                return new HttpStatusCodeResult(HttpStatusCode.BadRequest);
            }

            // It finds the Student to be deleted.
            Student student = db.Students.Find(id);
            if (student == null)
            {
                // if doesn't found returns 404
                return HttpNotFound();
            }
            // Returns the Student data to show the details of what will be deleted.
            return View(student);
        }

        // POST: Student/Delete/5
        [HttpPost]

        //Represents an attribute that is used for the name of an action.
        [ActionName("Delete")]

        //used to To protect from overposting attacks more details see https://www.wikiod.com/asp-dotnet-mvc/htmlantiforgerytoken
        [ValidateAntiForgeryToken]
        public ActionResult Delete(int id)
        {
            try
            {
                // Finds the student
                Student student = db.Students.Find(id);

                // Try to remove it
                db.Students.Remove(student);

                // Save the changes
                db.SaveChanges();
            }
            catch
            {
                //Log the error add a line here to write a log.
                ModelState.AddModelError("", "Unable to save changes. Try again, and if the problem persists, see your system administrator.");
            }

            // Returns an HTTP 302 response to the browser, which causes the browser to make a GET request to the specified action, in this case the index action.
            return RedirectToAction("Index");
        }

