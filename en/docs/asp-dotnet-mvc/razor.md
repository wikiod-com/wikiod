---
title: "Razor"
slug: "razor"
draft: false
images: []
weight: 9855
type: docs
toc: true
---

**What is Razor?**

Razor is a markup syntax that lets you embed server-based code (Visual Basic and C#) into web pages.

Server-based code can create dynamic web content on the fly, while a web page is written to the browser. When a web page is called, the server executes the server-based code inside the page before it returns the page to the browser. By running on the server, the code can perform complex tasks, like accessing databases.


## Syntax
 - @{ ... }
 - @variableName
 - @(variableName)
 - @for(...){ }
 - @(Explicit Expression)
 - @* comments *@


ASP.NET Razor includes view engines for both C# and VB.

The C# view engine processes files with a `.cshtml` extension, while the VB view engine works with `.vbhtml` files.

## Basic Syntax
Razor code can be inserted anywhere within HTML code. Razor code blocks are enclosed in `@{ ... }`. Inline variable and functions start with `@`. Code inside the Razor brackets follow the normal C# or VB rules.

**Single line statement:**

    @{ var firstNumber = 1; }

**Multi-line code block:**

    @{
        var secondNumber = 2;
        var total = firstNumber + secondNumber;
    }

**Using a variable inline:**

    <h1>The total count is @total</h1>

**Using a variable inline explicitly**:

    <h2>Item@(item.Id)</h2>

For this particular example we will not be able to use the implicit syntax because `Item@item.Id` looks like an email and will be rendered as such by Razor.

**Enclose code inside control flow statements:**  

    <h1>Start with some HTML code</h1>

    @for (int i = 0; i < total; i++){
        Console.Write(i);
    }

    <p>Mix in some HTML code for fun!</p>
    <p>Add a second paragraph.</p>

    @if (total > 3)
    {
        Console.Write("The total is greater than 3");
    }
    else
    {
        Console.Write("The total is less than 3");
    }

This same syntax would be used for all statements such as `for`, `foreach`, `while`, `if`, `switch`, etc.

**Adding code inside of code:**

    @if (total > 3)
    {
        if(total == 10)
        {
            Console.Write("The total is 10")
        }
    }

Note that you don't need to type the `@` at the second `if`. After code you can just type other code behind the existing code. 

If you want to add code after a HTML element you **do** need to type a `@`.


## Add Comments
Razor has its own comment syntax which begins with `@*` and ends with `*@`.

**Inline Comment:**

    <h1>Comments can be @*hi!*@ inline</h1>

**Multi-line Comment:**

    @* Comments can spread
       over multiple
       lines *@

**HTML Comment**

You can also use the normal HTML comment syntax starting with `<!--` and ending with `-->` in Razor views. But unlike other comments, the Razor code inside a HTML comment is still executed normally.

    @{
        var hello = "Hello World!"; 
    } 
    <!-- @hello -->

The above example produces the following HTML output:

    <!-- Hello World! -->


**Comments within a code block:**

    @{
        // This is a comment
        var Input = "test";
    }

## Adding a custom attribute with - (hyphen) in name
If you need to add an attribute through razor that has a - (hyphen) in the name you cannot simply do 

    @Html.DropDownListFor(m => m.Id, Model.Values, new { @data-placeholder = "whatever" })

it will not compile.  data-* attributes are valid and common in html5 for adding extra values to elements.

However the following will work

    @Html.DropDownListFor(m => m.Id, Model.Values, new { @data_placeholder = "whatever" })

since "_" is replaced with "-" when rendered.  

This works fine as underscores are not acceptable in attribute names in html.



## Create inline classes and methods using @functions
Using Razor `@functions` keyword gives the capability of introducing classes and methods for inline use within a Razor file:

    @functions
    {
        string GetCssClass(Status status)
        {
            switch (status)
            {
                case Status.Success:
                    return "alert-success";
                case Status.Info:
                    return "alert-info";
                case Status.Warning:
                    return "alert-warning";
                case Status.Danger:
                default:
                    return "alert-danger";
             }
         }
    }


    <label class="alert @GetCssClass(status)"></label>

The same can be done for classes:
       
    @functions
    {
        class Helpers
        {
            //implementation
        }
    }

## Display HTML within Razor code block
While inside a Razor code block, the browser will only recognize HTML code if the code is escaped.

**Use `@:` for a Single line:**

    @foreach(int number in Model.Numbers)
    {
        @:<h1>Hello, I am a header!</h1>
    }

**Use `<text> ... </text>` for Multi-line:**

    @{
        var number = 1;

        <text>
            Hello, I am text
            <br / >
            Hello, I am more text!
        </text>
    }

Note that Razor, when inside a code block, will understand HTML tags. Therefore, adding the `text` tag around HTML tags is unnecessary (although still correct), such as:

    @{
        var number = 1;
        <text>
            <div>
                Hello, I am text
                <br / >
                Hello, I am more text!
            </div>
        </text>
    }

## Escaping @ character
In many cases, the Razor parser is smart enough to figure out when the `@` sign is meant to be used as part of code, as opposed to being part of something like an email address. In the example below, escaping the `@` sign is not necessary:

```
<p>Reach out to us at contact@mail.com</p>
```

However, in some cases, usage of the `@` sign is more ambiguous, and it must be explicitly escaped with `@@`, as in the example below:

```
<p>Join us @@ Stack Overflow!</p>
```

Alternatively, we can use a HTML encoded `@` character

```
<p>Join us &#64; Stack Overflow!</p>
```

## Share @helpers across views
@Helpers could be shared between views.

They should be created in the folder App_Code

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/EMu2v.png


    @helper CreatePrimaryBootstrapButton(string label)
    {
        <button type="button" class="btn btn-primary">@label</button>
    }
    
    //call
    
    @MenuHelpers.CreatePrimaryBootstrapButton("my button")



The globals `@Url` and `@Html` aren't available by default in the @Helper defined in App_code. You could add them as follows (for every .cshtml in your App_code folder)

    @*  Make @Html and @Url available *@
    @functions
    {
        private new static HtmlHelper<object> Html
        {
            get { return ((WebViewPage)CurrentPage).Html; }
        }
    
        private static UrlHelper Url
        {
            get { return ((WebViewPage)CurrentPage).Url; }
        }
     }

 

## Editor Templates
Editor templates are a good way to reuse Razor code. You can define editor templates as Razor partial views and then use them in other views.

Editor templates usually exist in the `Views/Shared/EditorTemplates/` folder, although they can also be saved to the `Views/ControllerName/EditorTemplates/` folder. The name of the view is typically the name of the object you want to use the template for, like `<type>.cshtml`.

Here is a simple editor template for DateTime:

    @model DateTime
    <div>
       <span>
          @Html.TextBox("", Model.ToShortDateString(), new { data_date_picker="true" })
       </span>
    </div>

Save the file as **Views/Shared/EditorTemplate/DateTime.cshtml**.

Then, use [`EditorFor`][1] to call this template code in a another view:

    @Html.EditorFor(m => m.CreatedDate)

---

There is also a UIHint attribute to specify the file name:

    public class UiHintExampleClass
    {
        [UIHint("PhoneNumber")]
        public string Phone { get; set; }
    }

Define this phone number template in **Views/Shared/EditorTemplates/PhoneNumber.cshtml**.

---

Editor templates can be defined for Custom Types as well.

Here is a custom type called `SubModel`:

    public class SubModel
    {
        public Guid Id { get; set;} 
        public string FirstName { get; set; }
        public string LastName { get; set; }
    }
    
    public class Model
    {
        public Guid Id { get; set; }
        public DateTime Created {get; set; }
        
        public SubModel SubModel{get; set; }
    }

This is the EditorTemplate for SubModel:

    @model SubModel
    <div class="form-group">
        @Html.LabelFor(m => m.FirstName)
        @Html.TextBoxFor(m => m.FirstName)
    </div>
    <div class="form-group">
        @Html.LabelFor(m => m.LastName)
        @Html.TextBoxFor(m => m.LastName)
    </div>

Now, the View for Model simply becomes:

    @model Model
    @Html.EditorFor(m => m.CreatedDate)
    @Html.EditorFor(m => m.SubModel, new { @Prefix = "New"}) 
    @* the second argument is how you can pass viewdata to your editor template*@

 [1]:https://msdn.microsoft.com/en-us/library/system.web.mvc.html.editorextensions.editorfor(v=vs.118).aspx

## Pass Razor content to a @helper
Send a Razor part to a @helper, for example a HTML div.

    @helper WrapInBox(Func<Object, HelperResult> content)
    {
        <div class="box">@content(null) </div>
    }

    //call 
    @WrapInBox(@<div>
                    I'm a inner div
                </div>)

