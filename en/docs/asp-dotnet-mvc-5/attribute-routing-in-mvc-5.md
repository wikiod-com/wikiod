---
title: "Attribute routing in mvc-5"
slug: "attribute-routing-in-mvc-5"
draft: false
images: []
weight: 9942
type: docs
toc: true
---

## Syntax

 1. {productId:int}/{productTitle} 
Mapped to ProductsController.Show(int id)

 2. {username} Mapped to ProfilesController.Show(string username)

 3. {username}/catalogs/{catalogId:int}/{catalogTitle} Mapped to CatalogsController.Show(string username, int catalogId)





Routing is how ASP.NET MVC matches a URI to an action. MVC 5 supports a new type of routing, called attribute routing. As the name implies, attribute routing uses attributes to define routes. Attribute routing gives you more control over the URIs in your web application.

The earlier style of routing, called convention-based routing, is still fully supported. In fact, you can combine both techniques in the same project.

## Route Constraints
Route constraints let you restrict how the parameters in the route template are matched. The general syntax is {parameter:constraint}. For example:

    // eg: /users/5
    [Route(“users/{id:int}”]
    public ActionResult GetUserById(int id) { … }
     
    // eg: users/ken
    [Route(“users/{name}”]
    public ActionResult GetUserByName(string name) { … }

Here, the first route will only be selected if the “id” segment of the URI is an integer. Otherwise, the second route will be chosen.

|Const|Description (Matches:)|Example|
|----------|-----------|-------|
alpha|Uppercase or lowercase Latin alphabet characters (a-z, A-Z)|{x:alpha}|
bool|Boolean value.|{x:bool}|
datetime|DateTime value.|{x:datetime}|
decimal|Decimal value.|{x:decimal}|
double|64-bit floating-point value.|{x:double}|
float|32-bit floating-point value.|{x:float}|
guid|GUID value.|{x:guid}|
int|32-bit integer value.|{x:int}|
length|String with the specified length or within a specified range of lengths.|{x:length(6)} {x:length(1,20)}|
long|64-bit integer value.|{x:long}|
max|Integer with a maximum value.|{x:max(10)}|
maxlength|String with a maximum length.|{x:maxlength(10)}|
min|Integer with a minimum value.|{x:min(10)}|
minlength|String with a minimum length.|{x:minlength(10)}|
range|Integer within a range of values.|{x:range(10,50)}|
regex|Regular expression.|{x:regex(^\d{3}-\d{3}-\d{4}$)}|

## How to implement attribute route
**Enabling Attribute Routing**
To enable attribute routing, call MapMvcAttributeRoutes during configuration.

    public static void RegisterRoutes(RouteCollection routes)
    {
    routes.IgnoreRoute(“{resource}.axd/{*pathInfo}”);
 
    routes.MapMvcAttributeRoutes();
 
    routes.MapRoute(
        name: “Default”,
        url: “{controller}/{action}/{id}”,
        defaults: new { controller = “Home”, action = “Index”, id = UrlParameter.Optional }
    );
    }



## Optional URI Parameters and Default Values
You can make a URI parameter optional by adding a question mark to the route parameter. You can also specify a default value by using the form parameter=value.

    public class BooksController : Controller
    {
        // eg: /books
        // eg: /books/1430210079
        [Route(“books/{isbn?}”)]
        public ActionResult View(string isbn)
        {
            if (!String.IsNullOrEmpty(isbn))
            {
                return View(“OneBook”, GetBook(isbn));
            }
            return View(“AllBooks”, GetBooks());
        }
 
    // eg: /books/lang
    // eg: /books/lang/en
    // eg: /books/lang/he
    [Route(“books/lang/{lang=en}”)]
    public ActionResult ViewByLanguage(string lang)
    {
        return View(“OneBook”, GetBooksByLanguage(lang));
    }
In this example, both /books and /books/1430210079 will route to the “View” action, the former will result with listing all books, and the latter will list the specific book. Both /books/lang and /books/lang/en will be treated the same.

## Route Prefixes
Often, the routes in a controller all start with the same prefix. For example:

    public class ReviewsController : Controller
    {
        // eg: /reviews
        [Route(“reviews”)]
        public ActionResult Index() { … }
        // eg: /reviews/5
        [Route(“reviews/{reviewId}”)]
        public ActionResult Show(int reviewId) { … }
        // eg: /reviews/5/edit
        [Route(“reviews/{reviewId}/edit”)]
        public ActionResult Edit(int reviewId) { … }
    }

You can set a common prefix for an entire controller by using the [RoutePrefix] attribute:

    [RoutePrefix(“reviews”)]
    public class ReviewsController : Controller
    {
        // eg.: /reviews
        [Route]
        public ActionResult Index() { … }
        // eg.: /reviews/5
        [Route(“{reviewId}”)]
        public ActionResult Show(int reviewId) { … }
        // eg.: /reviews/5/edit
        [Route(“{reviewId}/edit”)]
        public ActionResult Edit(int reviewId) { … }
    }

Use a tilde (~) on the method attribute to override the route prefix if needed:

    [RoutePrefix(“reviews”)]
    public class ReviewsController : Controller
    {
        // eg.: /spotlight-review
        [Route(“~/spotlight-review”)]
        public ActionResult ShowSpotlight() { … }
     
        …
    }

## Default Route
You can also apply the [Route] attribute on the controller level, capturing the action as a parameter. That route would then be applied on all actions in the controller, unless a specific [Route] has been defined on a specific action, overriding the default set on the controller.

    [RoutePrefix(“promotions”)]
    [Route(“{action=index}”)]
    public class ReviewsController : Controller
    {
        // eg.: /promotions
        public ActionResult Index() { … }
     
        // eg.: /promotions/archive
        public ActionResult Archive() { … }
     
        // eg.: /promotions/new
        public ActionResult New() { … }
     
        // eg.: /promotions/edit/5
        [Route(“edit/{promoId:int}”)]
        public ActionResult Edit(int promoId) { … }
    }

