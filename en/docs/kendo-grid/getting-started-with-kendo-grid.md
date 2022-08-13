---
title: "Getting started with kendo-grid"
slug: "getting-started-with-kendo-grid"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
We can add Kendo-UI grid in HTML5/Javascript, ASP.NET MVC, JSP and PHP project/application.


----------
Please follow below steps to add kendo-UI grid in HTML5 page.
-------------------------------------------------------------

 1. Create empty html5 page.
 2. Include **kendo.common.min.css** and **kendo.default.min.css**. Add a link tag within the head tag.
 3. Kendo-UI library is depended on Jquery. So, include **kendo.all.min.js** and kendo.aspnetmvc.min.js after jQuery.
 4. There are two possible ways to instantiate a Kendo UI grid.
    -  From an empty div element. In this case all the Grid settings are provided in the initialization script statement.
    -  From an existing HTML table element. In this case some of the Grid settings can be inferred from the table structure and elements HTML attributes.

    In both cases the grid is registered as a jQuery plugin.

    You can find cdn path [here][1] for above mentioned files.


----------
Example: Kendo-UI Grid in HTML5 page - Empty div element
----------

    <!DOCTYPE html>
    <html>
    <head>
        <title></title>
        <link rel="stylesheet" href="http://kendo.cdn.telerik.com/2016.2.714/styles/kendo.common.min.css">
        <link rel="stylesheet" href="http://kendo.cdn.telerik.com/2016.2.714/styles/kendo.default.min.css">
        <script src="http://code.jquery.com/jquery-1.9.1.min.js"></script>
        <script src="http://kendo.cdn.telerik.com/2016.2.714/js/kendo.all.min.js"></script>
    </head>
    <body>
        <div id="grid">
        </div>
        <script>
            var products = [{
                ProductID: 11,
                ProductName: "Chai",
            }, {
                ProductID: 22,
                ProductName: "Chang",
            }, {
                ProductID: 33,
                ProductName: "Aniseed Syrup",
            }, {
                ProductID: 44,
                ProductName: "Chef Anton's Cajun Seasoning",
            }, {
                ProductID: 55,
                ProductName: "Chef Anton's Gumbo Mix",
            }];
            $(document).ready(function () {
                $("#grid").kendoGrid({
                    dataSource: {
                        data: products,
                        schema: {
                            model: {
                                id: "ProductID",
                                fields: {
                                    ProductName: {
                                        type: "string"
                                    }
                                },
                            }
                        },
                        pageSize: 10
                    },
                    sortable: true, 
                    filterable: true,
                    pageable: true,
                    columns: [
                        { field: "ProductID", title: "ProductID" },
                        { field: "ProductName", title: "ProductName" },
                        { command: ["edit", "destroy"], title: "&nbsp;" }
                    ],
                    editable: "inline"
                });
            }); 
        </script>
    </body>
    </html>

----------
Example: Kendo-UI Grid in HTML5 page - Existing HTML table element
----------

    <!DOCTYPE html>
    <html>
     <head>
        <title></title>
        <link rel="stylesheet" href="http://kendo.cdn.telerik.com/2016.2.714/styles/kendo.common.min.css">
        <link rel="stylesheet" href="http://kendo.cdn.telerik.com/2016.2.714/styles/kendo.default.min.css">
        <script src="http://code.jquery.com/jquery-1.9.1.min.js"></script>
        <script src="http://kendo.cdn.telerik.com/2016.2.714/js/kendo.all.min.js"></script>
    </head>
    <body>
    
            <div id="example">
                <table id="grid">
                    <colgroup>
                        <col />
                        <col />
                        <col style="width:110px" />
                        <col style="width:120px" />
                        <col style="width:130px" />
                    </colgroup>
                    <thead>
                        <tr>
                            <th data-field="make">Car Make</th>
                            <th data-field="model">Car Model</th>
                            <th data-field="year">Year</th>
                            <th data-field="category">Category</th>
                            <th data-field="airconditioner">Air Conditioner</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td>Volvo</td>
                            <td>S60</td>
                            <td>2010</td>
                            <td>Saloon</td>
                            <td>Yes</td>
                        </tr>
                        <tr>
                            <td>Audi</td>
                            <td>A4</td>
                            <td>2002</td>
                            <td>Saloon</td>
                            <td>Yes</td>
                        </tr> 
                        <tr>
                            <td>Toyota</td>
                            <td>Avensis</td>
                            <td>2006</td>
                            <td>Saloon</td>
                            <td>No</td>
                        </tr>
                    </tbody>
                </table>
    
                <script>
                    $(document).ready(function() {
                        $("#grid").kendoGrid({
                            height: 550,
                            sortable: true
                        });
                    });
                </script>
            </div>
    
    </body>
    </html>

----------

Please follow below steps to add kendo-UI grid in ASP.NET MVC Application.

-------------------------------------------------------------

 1. Create ASP.NET MVC Project
 2. Include Javascript and CSS files. There are two options either include a local copy of those files or use the Kendo UI CDN services.
   - Use Local JavaScript and CSS

       Navigate to the install location of Telerik UI for ASP.NET MVC. By default, it is in C:\Program Files (x86)\Telerik\. 

       Copy the **js** directory from the install location and paste it in the **Scripts** folder of the application. 

       Copy the **styles** directory from the install location and paste it in the **Content** folder of the application. 

       Rename the **Scripts/js** directory to **Scripts/kendo**. Rename **Content/styles** to **Content/kendo**.
 
       Open **App_Start/BundleConfig.cs** to add below script and style bundles for Telerik UI for ASP.NET MVC.

   

    bundles.Add(new ScriptBundle("~/bundles/kendo").Include(
                        "~/Scripts/kendo/kendo.all.min.js",
                        // "~/Scripts/kendo/kendo.timezones.min.js", // uncomment if using the Scheduler
                        "~/Scripts/kendo/kendo.aspnetmvc.min.js"));

    bundles.Add(new StyleBundle("~/Content/kendo/css").Include(
                    "~/Content/kendo/kendo.common.min.css",
                    "~/Content/kendo/kendo.default.min.css"));

    bundles.IgnoreList.Clear(); //Tell the ASP.NET bundles to allow minified files in debug mode.
 
Move the jQuery bundle to the head tag of the page. It is at the end of the page by default. Render the Telerik UI for ASP.NET MVC script bundle after jQuery.

    @Scripts.Render("~/bundles/jquery")
    @Scripts.Render("~/bundles/kendo")

   - Use CDN Services

      Include **kendo.common.min.css** and **kendo.default.min.css**. Add a link tag within the head tag of the layout.

      Include **kendo.all.min.js** and **kendo.aspnetmvc.min.js** after jQuery.

     If using the Telerik MVC Scheduler wrapper, include **kendo.timezones.min.js** after **kendo.all.min.js**.

     You can find cdn path [here][1] for above mentioned files.

     Add **Kendo.Mvc.dll** reference into your project and the DLL is available in location **wrappers/aspnetmvc/Binaries/MVC***.

     The next step is to let ASP.NET MVC know of the Kendo.Mvc.UI namespace where the server-side wrappers are. For this add `<add namespace="Kendo.Mvc.UI" />` namespace tag in root web.config and View web.config.

 3. To verify your set up, please add below Kendo UI DatePicker widget in view/aspx page.
 
   Razor

    @(Html.Kendo().DatePicker().Name("datepicker"))

   ASPX

    <%: Html.Kendo().DatePicker().Name("datepicker") %>

  [1]: http://docs.telerik.com/kendo-ui/intro/installation/cdn-service

