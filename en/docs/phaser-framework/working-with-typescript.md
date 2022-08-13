---
title: "Working with TypeScript"
slug: "working-with-typescript"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Phaser environment setup (Asp.Net MVC5 - Typescript - Visual Studio 2015)
Create a new ASP.Net Project:

[![enter image description here][1]][1]

Select the empty template:

[![enter image description here][2]][2]

Add two new folders: `App` and `Scripts` in the root folder:

[![enter image description here][3]][3]

[Add `npm` configuration][4] file in the root folder:

[![enter image description here][5]][5]
 
    {
        "version": "1.0.0",
        "name": "phaser.js.environment.setup",
        "private": true,
          "devDependencies": {
            "gulp": "3.9.1",
            "phaser": "2.6.2"
          }
    }

    
[Add `gulp` configuration][6] file in the root folder:

[![enter image description here][7]][7]

[Add `typings` folder][8] in `Scripts` folder:

[![enter image description here][9]][9]

Gulp task:

    /// <binding ProjectOpened='install' />
    
    var gulp = require('gulp');
    
    gulp.task('phaser-setup-typings', function () {
        gulp.src([
            './node_modules/phaser/typescript/pixi.d.ts',
            './node_modules/phaser/typescript/p2.d.ts',
            './node_modules/phaser/typescript/phaser.d.ts',
        ])
       .pipe(gulp.dest('./Scripts/typings'));
    });
    
    gulp.task('phaser-setup', function () {
        gulp.src([
            './node_modules/phaser/build/phaser.min.js',
        ])
       .pipe(gulp.dest('./Scripts/'));
    });
    
    gulp.task('install', ['phaser-setup-typings', 'phaser-setup']);

Run the install task:

[![enter image description here][10]][10]

[Add a typescript file][11] in the `App` folder:

[![enter image description here][12]][12]

[Add an MVC controller][13]:

[![enter image description here][14]][14]

    using System.Web.Mvc;
    
    namespace PhaserSetUp.Controllers
    {
        public class HomeController : Controller
        {
            // GET: Home
            public ActionResult Index()
            {
                return View();
            }
        }
    }

[Add `web optimization` `nuget` package][15]:

    Install-Package Microsoft.AspNet.Web.Optimization

[![enter image description here][16]][16]

Add `BundleConfig.cs` class into the `App_Start` folder:

    using System.Web.Optimization;
    
    namespace PhaserSetUp.App_Start
    {
        public class BundleConfig
        {
            public static void RegisterBundles(BundleCollection bundles)
            {
                bundles.Add(new ScriptBundle("~/bundles/app").Include(
                            "~/App/app.js"));
            }
        }
    }

Edit the Global.asax

    using System;
    using System.Web;
    using System.Web.Mvc;
    using System.Web.Routing;
    using System.Web.Http;
    
    namespace PhaserSetUp
    {
        public class Global : HttpApplication
        {
            void Application_Start(object sender, EventArgs e)
            {
                // Code that runs on application startup
                AreaRegistration.RegisterAllAreas();
                GlobalConfiguration.Configure(WebApiConfig.Register);
                RouteConfig.RegisterRoutes(RouteTable.Routes);  
                BundleConfig.RegisterBundles(BundleTable.Bundles);         
            }
        }
    }


Add a View:

[![enter image description here][17]][17]

    @using System.Web.Optimization
    <!DOCTYPE html>
    
    <html>
    <head>
        <meta name="viewport" content="width=device-width" />
        <title>@ViewBag.Title</title>
    </head>
    <body>
        <div>
            @RenderBody()
        </div>
        <script src="../../Scripts/phaser.min.js"></script>
        @Scripts.Render("~/bundles/app")
    </body>
    </html>


  [1]: https://i.stack.imgur.com/hltmK.png
  [2]: https://i.stack.imgur.com/5fuyY.png
  [3]: https://i.stack.imgur.com/ZhmUG.png
  [4]: https://www.wikiod.com/npm/getting-started-with-npm#Install packages
  [5]: https://i.stack.imgur.com/FsKyk.png
  [6]: https://www.wikiod.com/gulp/getting-started-with-gulp
  [7]: https://i.stack.imgur.com/nqXZ4.png
  [8]: https://www.wikiod.com/typescript/importing-external-libraries
  [9]: https://i.stack.imgur.com/PDfZK.png
  [10]: https://i.stack.imgur.com/CgPuP.png
  [11]: https://www.wikiod.com/typescript/getting-started-with-typescript
  [12]: https://i.stack.imgur.com/tR8hm.png
  [13]: https://www.wikiod.com/asp-dotnet-mvc/getting-started-with-aspnet-mvc
  [14]: https://i.stack.imgur.com/rAt6D.png
  [15]: https://www.wikiod.com/asp-dotnet-mvc/bundling-and-minification#Script and Style Bundles
  [16]: https://i.stack.imgur.com/S3iqK.png
  [17]: https://i.stack.imgur.com/9cdVq.png

