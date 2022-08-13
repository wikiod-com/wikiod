---
title: "Globalization in ASP.NET MVC using Smart internationalization for ASP.NET"
slug: "globalization-in-aspnet-mvc-using-smart-internationalization-for-aspnet"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

[Smart internationalization for ASP.NET page](https://github.com/turquoiseowl/i18n)

The benefit of this approach is that you don't have to clutter controllers and other classes with code to look up values from .resx files. You simply surround text in [[[triple brackets.]]] (The delimiter is configurable.) An `HttpModule` looks for a translation in your .po file to replace the delimited text. If a translation is found, the `HttpModule` substitutes the translation. If no translation is found, it removes the triple brackets and renders the page with the original untranslated text.

.po files are a standard format for supplying translations for applications, so there are a number of applications available for editing them. It's easy to send a .po file to a non-technical user so that they can add translations.

## Basic configuration and setup
 1. Add the [I18N nuget package][1] to your MVC project.
 2. In web.config, add the `i18n.LocalizingModule` to your `<httpModules>` or `<modules>` section.  


    <!-- IIS 6 -->
    <httpModules>
      <add name="i18n.LocalizingModule" type="i18n.LocalizingModule, i18n" />
    </httpModules>

    <!-- IIS 7 -->
    <system.webServer> 
      <modules>
        <add name="i18n.LocalizingModule" type="i18n.LocalizingModule, i18n" />
      </modules>
    </system.webServer>

 3. Add a folder named "locale" to the root of your site. Create a subfolder for each culture you wish to support. For example, `/locale/fr/`.
 4. In each culture-specific folder, create a text file named `messages.po`.
 5. For testing purposes, enter the following lines of text in your `messages.po` file:


    #: Translation test
    msgid "Hello, world!"
    msgstr "Bonjour le monde!"

 6. Add a controller to your project which returns some text to translate.


    using System.Web.Mvc;

    namespace I18nDemo.Controllers
    {
        public class DefaultController : Controller
        {
            public ActionResult Index()
            {
                // Text inside [[[triple brackets]]] must precisely match
                // the msgid in your .po file.
                return Content("[[[Hello, world!]]]");
            }
        }
    }

 7. Run your MVC application and browse to the route corresponding to your controller action, such as http://localhost:[yourportnumber]/default.  
Observe that the URL is changed to reflect your default culture, such as  
http://localhost:[yourportnumber]/en/default.
 8. Replace `/en/` in the URL with `/fr/` (or whatever culture you've selected.) The page should now display the translated version of your text.
 9. Change your browser's language setting to prefer your alternate culture and browse to `/default` again. Observe that the URL is changed to reflect your alternate culture and the translated text appears.
 10. In web.config, add handlers so that users cannot browse to your `locale` folder.  


    <!-- IIS 6 -->
    <system.web>
      <httpHandlers>
        <add path="*" verb="*" type="System.Web.HttpNotFoundHandler"/>
      </httpHandlers>
    </system.web>
   
    <!-- IIS 7 -->
    <system.webServer>
      <handlers>
        <remove name="BlockViewHandler"/>
       <add name="BlockViewHandler" path="*" verb="*" preCondition="integratedMode" type="System.Web.HttpNotFoundHandler"/>
      </handlers>
    </system.webServer>

[1]: https://www.nuget.org/packages/I18N/

