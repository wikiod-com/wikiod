---
title: "Html Helpers"
slug: "html-helpers"
draft: false
images: []
weight: 9932
type: docs
toc: true
---

HTML helpers are methods used to render HTML elements in a view. They are part of the `System.Web.Mvc.HtmlHelper` namespace.

There are different types of HTML helpers:

**Standard HTML Helpers**: They are used to render normal HTML elements, e.g. `Html.TextBox()`.

**Strongly Typed HTML Helpers**: These helpers render HTML elements based on model properties, e.g. `Html.TextBoxFor()`.

**Custom HTML Helpers**: The user can create custom helper method which returns `MvcHtmlString`.


## Standard HTML Helpers with their HTML Outputs
[Html.TextBox()][1]

- `@Html.TextBox("Name", null, new { @class = "form-control" })  `  
*output:***`<input class="form-control" id="Name"name="Name"type="text"value=""/>`**
- `@Html.TextBox("Name", "Stack Overflow", new { @class = "form-control" })  `  
*output:***`<input class="form-control" id="Name"name="Name"type="text" value="Stack Overflow"/>`**

[Html.TextArea()][2]

- `@Html.TextArea("Notes", null, new { @class = "form-control" })   `  
*output:***`<textarea class="form-control" id="Notes" name="Notes" rows="2" cols="20"></textarea>`**
- `@Html.TextArea("Notes", "Please enter Notes", new { @class = "form-control" })    `  
*output:***`<textarea class="form-control" id="Notes" name="Notes" rows="2" cols="20" >Please enter Notes</textarea>`**

[Html.Label()][3]

- `@Html.Label("Name","FirstName")`  
*output:***`<label for="Name"> FirstName </label>`**
- `@Html.Label("Name", "FirstName", new { @class = "NameClass" })`  
*output:***`<label for="Name" class="NameClass">FirstName</label>`**

[Html.Hidden()][4]

- `@Html.Hidden("Name", "Value")`  
*output:***`<input id="Name" name="Name" type="hidden" value="Value" />`**

[Html.CheckBox()][5]

- `@Html.CheckBox("isStudent", true)`  
*output:***`<input checked="checked" id="isStudent" name="isStudent" type="checkbox" 
        value="true" />`**

[Html.Password()][6]

- `@Html.Password("StudentPassword")`  
*output:***`<input id="StudentPassword" name="StudentPassword" type="password"        value="" />`**


  [1]: https://msdn.microsoft.com/en-us/library/system.web.webpages.html.htmlhelper.textbox(v=vs.111).aspx
  [2]: https://msdn.microsoft.com/en-us/library/system.web.webpages.html.htmlhelper.textarea(v=vs.111).aspx
  [3]: https://msdn.microsoft.com/en-us/library/system.web.webpages.html.htmlhelper.label(v=vs.111).aspx
  [4]: https://msdn.microsoft.com/en-us/library/system.web.webpages.html.htmlhelper.hidden(v=vs.111).aspx
  [5]: https://msdn.microsoft.com/en-us/library/system.web.webpages.html.htmlhelper.checkbox(v=vs.111).aspx
  [6]: https://msdn.microsoft.com/en-us/library/system.web.mvc.html.inputextensions.password(v=vs.118).aspx

## Custom Helper - Render Radio Button with Label
           public static MvcHtmlString RadioButtonLabelFor<TModel, TProperty>(this HtmlHelper<TModel> self, Expression<Func<TModel, TProperty>> expression, bool value, string labelText)
        {
            // Retrieve the qualified model identifier
            string name = ExpressionHelper.GetExpressionText(expression);
            string fullName = self.ViewContext.ViewData.TemplateInfo.GetFullHtmlFieldName(name);

            // Generate the base ID
            TagBuilder tagBuilder = new TagBuilder("input");
            tagBuilder.GenerateId(fullName);
            string idAttr = tagBuilder.Attributes["id"];

            // Create an ID specific to the boolean direction
            idAttr = string.Format("{0}_{1}", idAttr, value);

            // Create the individual HTML elements, using the generated ID
            MvcHtmlString radioButton = self.RadioButtonFor(expression, value, new { id = idAttr });
            MvcHtmlString label = self.Label(idAttr, labelText);

            return new MvcHtmlString(radioButton.ToHtmlString() + label.ToHtmlString());
        }

**Example :** `@Html.RadioButtonLabelFor(m => m.IsActive, true, "Yes")`

## Custom HTML Helper - Display Name
    /// <summary>
    /// Gets displayName from DataAnnotations attribute
    /// </summary>
    /// <typeparam name="TModel"></typeparam>
    /// <typeparam name="TProperty"></typeparam>
    /// <param name="htmlHelper"></param>
    /// <param name="expression"></param>
    /// <returns></returns>
    public static MvcHtmlString GetDisplayName<TModel, TProperty>(this HtmlHelper<TModel> htmlHelper, Expression<Func<TModel, TProperty>> expression)
    {
        var metaData = ModelMetadata.FromLambdaExpression(expression, htmlHelper.ViewData);
        var value = metaData.DisplayName ?? (metaData.PropertyName ?? ExpressionHelper.GetExpressionText(expression));
        return MvcHtmlString.Create(value);
    }

## Custom Helper - Render submit button
    /// <summary>
    /// Creates simple button
    /// </summary>
    /// <param name="poHelper"></param>
    /// <param name="psValue"></param>
    /// <returns></returns>
    public static MvcHtmlString SubmitButton(this HtmlHelper poHelper, string psValue)
    {
        return new MvcHtmlString(string.Format("<input type=\"submit\" value=\"{0}\">", psValue));
    }



## Exhaustive list of HtmlHelper samples including HTML output
### [`HtmlHelper.Action()`][1]

- `@Html.Action(actionName: "Index")`  
*output:* **The HTML rendered by an action method called `Index()`**


- `@Html.Action(actionName: "Index", routeValues: new {id = 1})`  
*output:* **The HTML rendered by an action method called `Index(int id)`**


- `@(Html.Action("Index", routeValues: new RouteValueDictionary(new Dictionary<string, object>{ {"id", 1} })))`  
*output:* **The HTML rendered by an action method called `Index(int id)`**


- `@Html.Action(actionName: "Index", controllerName: "Home")`  
*output:* **The HTML rendered by an action method called `Index()` in the `HomeController`**


- `@Html.Action(actionName: "Index", controllerName: "Home", routeValues: new {id = 1})`  
*output:* **The HTML rendered by an action method called `Index(int id)` in the `HomeController`**


- `@Html.Action(actionName: "Index", controllerName: "Home", routeValues: new RouteValueDictionary(new Dictionary<string, object>{ {"id", 1} }))`  
*output:* **The HTML rendered by an action method called `Index(int id)` in the `HomeController`**

### [`HtmlHelper.ActionLink()`][2]

- `@Html.ActionLink(linkText: "Click me", actionName: "Index")`  
*output:* **`<a href="Home/Index">Click me</a>`**


- `@Html.ActionLink(linkText: "Click me", actionName: "Index", routeValues: new {id = 1})`  
*output:* **`<a href="Home/Index/1">Click me</a>`**


- `@Html.ActionLink(linkText: "Click me", actionName: "Index", routeValues: new {id = 1}, htmlAttributes: new {@class = "btn btn-default", data_foo = "bar")`  
*output:* **`<a href="Home/Index/1" class="btn btn-default" data-foo="bar">Click me</a>`**


- `@Html.ActionLink()`  
*output:* **`<a href=""></a>`**

### [`@HtmlHelper.BeginForm()`][3]
- `@using (Html.BeginForm("MyAction", "MyController", FormMethod.Post, new {id="form1",@class = "form-horizontal"}))`  
*output:* **`<form action="/MyController/MyAction" class="form-horizontal" id="form1" method="post">`**


  [1]: https://msdn.microsoft.com/en-us/library/system.web.mvc.html.childactionextensions.action(v=vs.118).aspx
  [2]: https://msdn.microsoft.com/en-US/library/system.web.mvc.html.linkextensions_methods(v=vs.118).aspx
  [3]: https://msdn.microsoft.com/en-US/library/system.web.mvc.html.formextensions_methods(v=vs.118).aspx

## Custom Helper - Date Time Picker
    public static MvcHtmlString DatePickerFor<TModel, TProperty>(this HtmlHelper<TModel> htmlHelper, Expression<Func<TModel, TProperty>> expression, object htmlAttributes)
    {
        var sb = new StringBuilder();
        var metaData = ModelMetadata.FromLambdaExpression(expression, htmlHelper.ViewData);
        var dtpId = "dtp" + metaData.PropertyName;
        var dtp = htmlHelper.TextBoxFor(expression, htmlAttributes).ToHtmlString();
        sb.AppendFormat("<div class='input-group date' id='{0}'> {1} <span class='input-group-addon'><span class='glyphicon glyphicon-calendar'></span></span></div>", dtpId, dtp);
        return MvcHtmlString.Create(sb.ToString());
    }

**Example:**

    @Html.DatePickerFor(model => model.PublishedDate,  new { @class = "form-control" })

If you use **Bootstrap.v3.Datetimepicker** The your JavaScript is like below -- 

    $('#dtpPublishedDate').datetimepicker({ format: 'MMM DD, YYYY' });



