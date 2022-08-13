---
title: "Culture-specific URLs"
slug: "culture-specific-urls"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Configuring culture-specific URLs
Having culture-specific URLs can be beneficial in terms of SEO.

 E.g. English version of the following page:

    http://www.mydomain.com/insurance

Would translate into:

    http://www.mydomain.nl/verzekering

Instead of:

    http://www.mydomain.nl/nl-nl/insurance

There are more approaches of achieving this:

1. Usually, you'll want the URLs to be derived from documents' names. To do that, make sure you set [**Use name path for URL path**][1] to true. By default, it's false.
[![Use name path for URL path][2]][2]

2. If the default URL creation pattern doesn't work for you, you can set the URLs manually as described in the [official documentation][3]. However, this option is viable only if you need to adjust small amounts of URLs.

3. If you want to automate creation of URLs based on a custom pattern you can implement a [custom module][4].
    
<!-- language: lang-c# -->
    using System;
    using System.Text;
    
    using CMS;
    using CMS.DataEngine;
    using CMS.DocumentEngine;
    using CMS.Helpers;
    
    [assembly: RegisterModule(typeof(CultureSpecificUrlsModule))]    
    public class CultureSpecificUrlsModule : Module
    {
        public CultureSpecificUrlsModule() : base("CultureSpecificUrlsModule")
        {
        }
    
        protected override void OnInit()
        {
            base.OnInit();
            /***
             * Before the node gets saved, we'll update it's DocumentUrlPath.
             * The system will ensure it'll be saved in a valid URL format.
             */
            DocumentEvents.Update.Before += Update_Before;
        }
    
        private void Update_Before(object sender, DocumentEventArgs e)
        {
            /*** 
             * Here you can apply conditions before you actually update the DocumentUrlPath.
             * E.g. you can check for the document's culture.
             */
            UpdateUrlPath(e.Node);
        }
    
        public static void UpdateUrlPath(TreeNode node)
        {
            /***
             * You can set the DocumentUrlPath to whatever you want.
             * In this example we're using a method extracted from CMS.DocumentEngine.TreePathUtils.
             * The same method is used to generate a URL for the default culture.
             */
            node.DocumentUrlPath = GetUrlPathFromNamePathInternal(node.DocumentNamePath);
        }
    
        internal static string GetUrlPathFromNamePathInternal(string namePath, int level = -1)
        {
            // Check valid path
            if (String.IsNullOrEmpty(namePath) || (namePath == "/"))
            {
                return null;
            }
    
            // For top level the path is always /
            if (level == 0)
            {
                return "/";
            }
    
            // Ensure maximal level if not set
            if (level < 0)
            {
                level = Int32.MaxValue;
            }
    
            // Get the path parts
            string[] pathParts = namePath.Split(new[] { '/' }, StringSplitOptions.RemoveEmptyEntries);
            int currentLevel = 1;
            var path = new StringBuilder();
    
            foreach (string part in pathParts)
            {
                string shortPart = part;
    
                // Shorten the part to the allowed maximum
                if (shortPart.Length > TreePathUtils.MaxAliasLength)
                {
                    shortPart = shortPart.Substring(0, TreePathUtils.MaxAliasLength);
                }
    
                path.Append("/", shortPart);
    
                if (++currentLevel > level)
                {
                    break;
                }
            }
    
            return path.ToString();
        }
    }


4. If you need to update existing pages (e.g. if you've forgotten to check **Use name path for URL path** before you started developing your project) you can use a simple console application that will update the URLs for you:


<!-- language: lang-c# -->

    using System;
    
    using CMS.DataEngine;
    using CMS.DocumentEngine;
    
    namespace CultureUrlsUtil
    {
        class Program
        {
            static void Main(string[] args)
            {
                CMSApplication.Init();
    
                /*** Here you can narrow down the scope of documents that should be updated using DocumentQuery ***/
                var pages = DocumentHelper.GetDocuments().Culture("es-es");
                
                foreach (var page in pages)
                {
                    /*** Here we are calling code from the example above. ***/
                    CultureSpecificUrlsModule.UpdateUrlPath(page);
                    page.Update();
                }
    
                Console.Write("URLs created!");
                Console.ReadLine();
            }
        }
    }


  [1]: https://docs.kentico.com/display/K9/Settings+-+URLs+and+SEO
  [2]: https://i.stack.imgur.com/kQzDd.png
  [3]: https://docs.kentico.com/display/K9/Configuring+URLs+for+multilingual+websites#ConfiguringURLsformultilingualwebsites-UsingacustompageURLpathfordifferentcultureversions
  [4]: https://docs.kentico.com/display/K9/Creating+custom+modules

