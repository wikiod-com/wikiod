---
title: "Bundling and Minification"
slug: "bundling-and-minification"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

## Minification
The minification is used to reduce the size of CSS and Javascript files to speed up download times. This process is done by removing all of the unnecessary white-space, comments, and any other non-essential content from the files.

This process is done automatically when using a `ScriptBundle` or a `StyleBundle` object. If you need to disable it, you have to use a basic `Bundle` object.

**Example using Minification**
-------

The following code uses preprocessor directives to apply bundling only during releases in order to allow for easier debugging during non-releases (as non-bundled files are typically easier to navigate through) :

    public static void RegisterBundles(BundleCollection bundles)  
    {  
        #if DEBUG
            bundles.Add(new Bundle("~/bundles/jquery").Include("~/Scripts/jquery-{version}.js"));
            bundles.Add(new Bundle("~/Content/css").Include("~/Content/site.css"));  
        #else
            bundles.Add(new ScriptBundle("~/bundles/jquery").Include("~/Scripts/jquery-{version}.js"));
            bundles.Add(new StyleBundle("~/Content/css").Include("~/Content/site.css"));  
        #endif
    }

## Script and Style Bundles
The following is the default code snippet for the BundleConfig.cs file.

    using System.Web.Optimization;
    
    public class BundleConfig  
    {  
        // For more information on Bundling, visit http://go.microsoft.com/fwlink/?LinkId=254725  
        public static void RegisterBundles(BundleCollection bundles)  
        {  
            bundles.Add(new ScriptBundle("~/bundles/jquery").Include(  
                        "~/Scripts/jquery-{version}.js"));  
                        
        // Use the development version of Modernizr to develop with and learn from. Then, when you're  
        // ready for production, use the build tool at http://modernizr.com to pick only the tests you need.  
        bundles.Add(new ScriptBundle("~/bundles/modernizr").Include(  
                    "~/Scripts/modernizr-*"));  
  
        bundles.Add(new StyleBundle("~/Content/css").Include("~/Content/site.css"));  
  
        bundles.Add(new StyleBundle("~/Content/themes/base/css").Include(  
                    "~/Content/themes/base/jquery.ui.core.css",  
                    "~/Content/themes/base/jquery.ui.resizable.css",  
        }  
     } 

Bundles are registered in the Global.asax file inside the Application_Start() method:

    using System.Web.Optimization;
    
    protected void Application_Start() 
    {
        BundleConfig.RegisterBundles(BundleTable.Bundles);
    }


Bundles should be rendered in your Views as so:

    @using System.Web.Optimization
    
    @Scripts.Render("~/bundles/jquery")
    @Scripts.Render("~/bundles/modernizr")
    @Styles.Render("~/Content/css") 
    @Styles.Render("~/Content/themes/base/css") 

Note that bundling does not occur when you are in development mode (where the compilation Element in the Web.config file is set to debug="true" ). Instead, the Render statements in your Views will include each individual file in a non-bundled, non-minified format, for ease of debugging.

Once the application is in production mode (where the compilation Element in the Web.config file is set to debug="false"), bundling will take place.

This can lead to complications for scripts that reference relative paths of other files, such as references to Twitter Bootstrap's icon files. This can be addressed by using System.Web.Optimization's CssRewriteUrlTransform class:

     bundles.Add(new StyleBundle("~/bundles/css").Include(
                    "~/Content/css/*.css", new CssRewriteUrlTransform()));

The CssRewriteUrlTransform class will rewrite relative Urls within the bundled files to absolute paths, so that the references will remain intact after the calling reference is moved to the location of the bundle (e.g. using the above code, moving from "~/Content/css/bootstrap.css" to "~/bundles/css/bootstrap.css").

