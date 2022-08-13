---
title: "Create Html Helpers"
slug: "create-html-helpers"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

Html helpers are a very useful way of creating html elements in views using MVC framework. With a bit of time your team can really benefit from using them. It helps with keeping the code clean and error prone.

To use the helpers you need to first add a `@using` directive inside the view, or add the namespace inside the `Web.config` file located in the `Views` folder.

## Create a simple helper - a div with a text in it
    public static class MyHelpers 
    {
       public static MvcHtmlString MyCustomDiv(this HtmlHelper htmlHelper, string text,
              object htmlAttributes = null)
       {
            var mainTag = new TagBuilder("div");
            mainTag.MergeAttributtes(htmlAttributes);
            mainTag.AddCssClass("some custom class");
            mainTag.SetInnerHtml(text);
            return MvcHtmlString.Create(mainTag.ToString());
       }
    }
To use it in the views: 

    @Html.MyCustomDiv("Test inside custom div");
    @Html.MyCustomDiv("Test inside custom div", new {@class="some class for the div element"});

## Disposable Helper (like Html.BeginForm)
 1. First create a disposable class:


    public class MyDisposableHelper: IDisposable
    {
       private bool _disposed;        
       private readonly ViewContext _viewContext;

        public MyDisposableHelper(ViewContext viewContext)
        {
            if (viewContext == null)
            {
                throw new ArgumentNullException(nameof(viewContext));
            }            
            _viewContext = viewContext;
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing)
        {
            if (_disposed)
                return;
            _disposed = true;            
            _viewContext.Writer.Write("</div>");
        }

        public void EndForm()
        {
            Dispose(true);
        }
     }

This class inherits `IDisposable` because we want to use the helper like `Html.BeginForm(...)`. When disposed it closes the `div` created when we called the helper in the view. 

 2. Create the `HtmlHelper` extension method:


    public static MyDisposableHelper BeginContainingHelper(this HtmlHelper htmlHelper) 
    {
         var containingTag = new TagBuilder("div");
         //add default css classes, attributes as needed
        htmlHelper.ViewContext.Writer.Write(containingTag .ToString(TagRenderMode.StartTag)); 
        return new MyDisposableHelper (htmlHelper.ViewContext);
    }

What to notice here is the call to `Writer.Write` to write to the response page out custom element. The `TagRenderMode.StartTag` is used to inform the writer not to close the div just yet, because we are gonna close it when disposing the `MyDisposableHelper` class. 

 3. To use it in the view:


    @using (Html.BeginContainingHelper()) {
        <div>element inside our custom element</div>
    }

