---
title: "WebView navigation"
slug: "webview-navigation"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

All examples that fetch data from a remote URL, has to have "Internet (client)" capability checked in the Package.appxmanifest. For examples that only manipulate local data it's not necessary.

## Open HTML file from app package
You can easily open a file from your app package, but Uri scheme must be "ms-appx-web" instead of "ms-appx":

<!-- language: c# -->

    var uri = new Uri("ms-appx-web:///Assets/Html/html-sample.html");
    this.webView.Navigate(uri);

## Navigate to Uri
This code simply navigates WebView to some Uri:

<!-- language: c# -->
    this.webView.Navigate(new Uri("http://stackoverflow.com/"));

or

<!-- language: c# -->
    this.webView.Source = new Uri("http://stackoverflow.com/");


## Navigate with HttpRequestMessage
Set custom user agent and navigate to Uri:
    
<!-- language: c# -->

    var userAgent = "my custom user agent";
    var uri = new Uri("http://useragentstring.com/");
    var requestMessage = new HttpRequestMessage(HttpMethod.Get, uri);
    requestMessage.Headers.Add("User-Agent", userAgent);

    this.webView.NavigateWithHttpRequestMessage(requestMessage);

## Navigate to string
Show specified html string in WebView:

<!-- language: c# -->
    var htmlString = 
        @"<!DOCTYPE html>
          <html>
              <head><title>HTML document</title></head>
              <body>
                  <p>This is simple HTML content.</p>
              </body>
          </html>";

    this.webView.NavigateToString(htmlString);

## Open HTML file from app local folder or temp folder
To open a file from local folder or temp folder, target file **must not** be located in those folders' root. For security reasons, to prevent other content from being exposed by WebView, the file meant for displaying must be located in a subfolder:

<!-- language: c# -->

    var uri = new Uri("ms-appdata:///local/html/html-sample.html");
    this.webView.Navigate(uri);

## NavigateToLocalStreamUri
In case when NavigateToString can't handle some content, use NavigateToLocalStreamUri method. It will force every locally-referenced URI inside the HTML page to call to the special resolver class, which can provide right content on the fly.

Assets/Html/html-sample.html file:

    <!DOCTYPE html>
    <html>
        <head>
            <title>HTML document</title>
        </head>
        <body>
            <p>This is simple HTML content.</p>
            <img src="cat.jpg"/>
        </body>
    </html>

Code:

<!-- language: c# -->
    protected override void OnNavigatedTo(NavigationEventArgs args)
    {
        // The Uri resolver takes is in the form of "ms-local-stream://appname_KEY/folder/file"
        // For simplicity, there is method BuildLocalStreamUri which returns correct Uri.
        var uri = this.webView.BuildLocalStreamUri("SomeTag", "/html-sample.html");
        var resolver = new StreamUriResolver();
        this.webView.NavigateToLocalStreamUri(uri, resolver);

        base.OnNavigatedTo(args);
    }


    public sealed class StreamUriResolver : IUriToStreamResolver
    {
        public IAsyncOperation<IInputStream> UriToStreamAsync(Uri uri)
        {
            if (uri == null)
            {
                throw new ArgumentNullException(nameof(uri));
            }

            var path = uri.AbsolutePath;

            return GetContent(path).AsAsyncOperation();
        }


        private async Task<IInputStream> GetContent(string uriPath)
        {
            Uri localUri;

            if (Path.GetExtension(uriPath).Equals(".html"))
            {
                localUri = new Uri("ms-appx:///Assets/Html" + uriPath);
            }
            else
            {
                localUri = new Uri("ms-appdata:///local/content" + uriPath);
            }

            var file = await StorageFile.GetFileFromApplicationUriAsync(localUri);
            var stream = await file.OpenAsync(FileAccessMode.Read);

            return stream.GetInputStreamAt(0);
        }
    }

This code will take HTML page from app package and embed content from local folder into it. Provided that you have image "cat.jpg" in /local/content folder, it will show HTML page with cat image.

