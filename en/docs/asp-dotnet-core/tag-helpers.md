---
title: "Tag Helpers"
slug: "tag-helpers"
draft: false
images: []
weight: 9937
type: docs
toc: true
---

## Parameters
| Name     |      Info      |
|----------|:--------------|
| asp-action | The name of the action method to which the form should be posted to |
|asp-controller| The name of the controller where the action method specified in asp-action exists|
|asp-route-*| Custom route values you want to add as querystring to the form action attribute value. Replace 8 with the querystring name you want|



## Form Tag Helper - With custom route attributes
<!-- language-all: lang-razor -->
    <form asp-action="create" 
          asp-controller="Home" 
          asp-route-returnurl="dashboard" 
          asp-route-from="google">
         <!--Your form elements goes here--> 
    </form>

This will generate the below markup 

    <form action="/Home/create?returnurl=dashboard&amp;from=google" method="post">
        <!--Your form elements goes here--> 
    </form>

## Form Tag Helper - Basic example
<!-- language: lang-razor -->
    <form asp-action="create" asp-controller="Home">
         <!--Your form elements goes here--> 
    </form>



## Input Tag Helper
Assuming your view is strongly typed to a view model like

<!-- language: lang-cs -->
    public class CreateProduct
    {
       public string Name { set; get; }
    }
And you are passing an object of this to the view from your action method.

<!-- language: lang-razor -->
    @model CreateProduct
    <form asp-action="create" asp-controller="Home" >

        <input type="text" asp-for="Name"/>   
        <input type="submit"/>

    </form>

This will generate the below markup.

<!-- language: lang-html -->
    <form action="/Home/create" method="post"> 
    
        <input type="text" id="Name" name="Name" value="" />
        <input type="submit"/>
        <input name="__RequestVerificationToken" type="hidden" value="ThisWillBeAUniqueToken" />

    </form>

If you want the input field to be rendered with a default value, you can set the Name property value of your view model in the action method.

<!-- language: lang-cs -->
    public IActionResult Create()
    {
      var vm = new CreateProduct { Name="IPhone"};
      return View(vm);
    } 

**Form submission & Model binding**

Model binding will work fine if you use `CreateProduct` as your HttpPost action method parameter/a parameter named `name`

## Select Tag Helper
Assuming your view is strongly typed to a view model like this

<!-- language: c# -->
    public class CreateProduct
    {       
       public IEnumerable<SelectListItem> Categories { set; get; }
       public int SelectedCategory { set; get; }
    }
And in your GET action method, you are creating an object of this view model, setting the Categories property and sending to the view

<!-- language: c# -->
    public IActionResult Create()
    {
        var vm = new CreateProduct();
        vm.Categories = new List<SelectListItem>
        {
            new SelectListItem {Text = "Books", Value = "1"},
            new SelectListItem {Text = "Furniture", Value = "2"}
        };
        return View(vm);
    }
and in your view

<!-- language: lang-razor -->
    @model CreateProduct
<!-- language: lang-razor -->
    <form asp-action="create" asp-controller="Home">
        <select asp-for="SelectedCategory" asp-items="@Model.Categories">
            <option>Select one</option>
        </select>
        <input type="submit"/>
    </form>

This will render the below markup(*included only relevant parts of form/fields*)

<!-- language: lang-razor -->
    <form action="/Home/create" method="post">  
        <select data-val="true" id="SelectedCategory" name="SelectedCategory">
            <option>Select one</option>
            <option value="1">Shyju</option>
            <option value="2">Sean</option>
        </select>
        <input type="submit"/>
    </form>

**Getting the selected dropdown value in form submission**

You can use the same view model as your HttpPost action method parameter

<!-- language: c# -->
    [HttpPost]
    public ActionResult Create(CreateProduct model)
    {
      //check model.SelectedCategory value
      / /to do : return something
    }

**Set an option as the selected one**

If you want to set an option as the selected option, you may simply set the `SelectedCategory` property value.

<!-- language: c# -->
    public IActionResult Create()
    {
        var vm = new CreateProduct();
        vm.Categories = new List<SelectListItem>
        {
            new SelectListItem {Text = "Books", Value = "1"},
            new SelectListItem {Text = "Furniture", Value = "2"},
            new SelectListItem {Text = "Music", Value = "3"}
        };
        vm.SelectedCategory = 2;
        return View(vm);
    }


**Rendering a Multi select dropdown/ListBox**

If you want to render a multi select dropdown, you can simply change your view model property which you use for `asp-for` attribute in your view to an array type.

<!-- language: c# -->
    public class CreateProduct
    {       
       public IEnumerable<SelectListItem> Categories { set; get; }
       public int[] SelectedCategories { set; get; }
    }

In the view 

<!-- language: lang-razor -->
    @model CreateProduct
<!-- language: lang-razor -->
    <form asp-action="create" asp-controller="Home" >
        <select asp-for="SelectedCategories" asp-items="@Model.Categories">
            <option>Select one</option>
        </select>
        <input type="submit"/>
    </form>

This will generate the SELECT element with `multiple` attribute

<!-- language: lang-razor -->
    <form action="/Home/create" method="post">
         <select id="SelectedCategories" multiple="multiple" name="SelectedCategories">
            <option>Select one</option>
            <option value="1">Shyju</option>
           <option value="2">Sean</option>
         </select>
        <input type="submit"/>
    </form>



## Custom Tag Helper
<!-- language-all: lang-razor -->
You can create your own tag helpers by implementing `ITagHelper` or deriving from the convenience class `TagHelper`.

- The default convention is to target an html tag that matches the name of the helper without the optional TagHelper suffix. For example `WidgetTagHelper` will target a `<widget>` tag.
- The `[HtmlTargetElement]` attribute can be used to further control the tag being targetted
- Any public property of the class can be given a value as an attribute in the razor markup. For example a public property `public string Title {get; set;}` can be given a value as `<widget title="my title">`
- By default, tag helpers translates Pascal-cased C# class names and properties for tag helpers into lower kebab case. For example, if you omit using `[HtmlTargetElement]` and the class name is `WidgetBoxTagHelper`, then in Razor you'll write `<widget-box></widget-box>`.
- `Process` and `ProcessAsync` contain the rendering logic. Both receive a **context** parameter with information about the current tag being rendered and an **output** parameter used to customize the rendered result.

Any assembly containing custom tag helpers needs to be added to the **_ViewImports.cshtml** file (Note it is the assembly being registered, not the namespace):

    @addTagHelper *, MyAssembly

Sample Widget Custom Tag Helper
===============================

The following example creates a custom widget tag helper that will target razor markup like:

    <widget-box title="My Title">This is my content: @ViewData["Message"]</widget-box>

Which will be rendered as:

    <div class="widget-box">
        <div class="widget-header">My Title</div>
        <div class="widget-body">This is my content: some message</div>
    </div>

The coded needed to create such a tag helper is the following:

<!-- language: c# -->
    [HtmlTargetElement("widget-box")]
    public class WidgetTagHelper : TagHelper
    {
        public string Title { get; set; }

        public override async Task ProcessAsync(TagHelperContext context, TagHelperOutput output)
        {
            var outerTag = new TagBuilder("div");
            outerTag.Attributes.Add("class", output.TagName);
            output.MergeAttributes(outerTag);
            output.TagName = outerTag.TagName;

            //Create the header
            var header = new TagBuilder("div");
            header.Attributes.Add("class", "widget-header");
            header.InnerHtml.Append(this.Title);
            output.PreContent.SetHtmlContent(header);

            //Create the body and replace original tag helper content
            var body = new TagBuilder("div");
            body.Attributes.Add("class", "widget-body");
            var originalContents = await output.GetChildContentAsync();
            body.InnerHtml.Append(originalContents.GetContent());
            output.Content.SetHtmlContent(body);
        }
    }


## Label Tag Helper
Label Tag Helper can be used to render `label` for a model property. It replaces method `Html.LabelFor` in previous versions of MVC.

Let's say you have a model:

<!-- language: lang-cs -->
    public class FormViewModel
    {
        public string Name { get; set; }
    }

In the view you can use `label` HTML element and `asp-for` tag helper:

<!-- language: lang-razor -->
    <form>
        <label asp-for="Name"></label>
        <input asp-for="Name" type="text" />
    </form>

This is equivalent to the following code in earlier versions of MVC:

<!-- language: lang-razor -->
    <form>
        @Html.LabelFor(x => x.Name)
        @Html.TextBoxFor(x => x.Name)
    </form>

Both code snippets above render the same HTML:

<!-- language: lang-html -->
    <form>
        <label for="Name">Name</label>
        <input name="Name" id="Name" type="text" value="">
    </form>

## Anchor tag helper
<!-- language-all: lang-razor -->
Anchor tag helper is used generate href attributes to link to a particular controller action or MVC route. Basic example

    <a asp-controller="Products" asp-action="Index">Login</a>

Sometimes, we need to specify additional parameters for the controller action that you are binding to. We can specify values for these parameters by adding attributes with the asp-route- prefix.

    <a asp-controller="Products" asp-action="Details" asp-route-id="@Model.ProductId">
       View Details
    </a>



