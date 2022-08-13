---
title: "UIWebView"
slug: "uiwebview"
draft: false
images: []
weight: 9777
type: docs
toc: true
---

**UIWebView Delegate functions:-**

**Objective-C Declerations**

    - (BOOL)webView:(UIWebView *)webView 
    shouldStartLoadWithRequest:(NSURLRequest *)request 
     navigationType:(UIWebViewNavigationType)navigationType;
    
    - (void)webView:(UIWebView *)webView 
    didFailLoadWithError:(NSError *)error;
    
    - (void)webViewDidFinishLoad:(UIWebView *)webView;
    
    - (void)webViewDidStartLoad:(UIWebView *)webView;

## Create a UIWebView instance
**Swift**

    let webview = UIWebView(frame: CGRect(x: 0, y: 0, width: 320, height: 480))
**Objective-C**

    UIWebView *webview = [[UIWebView alloc] initWithFrame:CGRectMake(0, 0, 320, 480)];

    //Alternative way of defining frame for UIWebView
    UIWebView *webview = [[UIWebView alloc] init];
    CGRect webviewFrame = webview.frame;
    webviewFrame.size.width = 320;
    webviewFrame.size.height = 480;
    webviewFrame.origin.x = 0;
    webviewFrame.origin.y = 0;
    webview.frame = webviewFrame;


## Determining content size
In many cases, for instance when using web views in table view cells, it's important to determine the content size of the rendered HTML page. After loading the page, this can be calculated in the `UIWebViewDelegate` delegate method:

    - (void) webViewDidFinishLoad:(UIWebView *) aWebView {
        CGRect frame = aWebView.frame;
        frame.size.height = 1;
        aWebView.frame = frame;
        CGSize fittingSize = [aWebView sizeThatFits:CGSizeZero];
        frame.size = fittingSize;
        aWebView.frame = frame;
    
        NSLog(@"size: %f, %f", fittingSize.width, fittingSize.height);
    }

The code employs an additional trick of shortly setting the height of the web view to 1 prior to measuring the fitting size. Otherwise it would simply report the current frame size. After measuring we immediately set the height to the actual content height.

[Source][1]


  [1]: http://stackoverflow.com/a/3937599/235297

## Making a URL request
Load content in webview from the `url`

**Swift**

    webview.loadRequest(NSURLRequest(URL: NSURL(string: "http://www.google.com")!))

**Objective-C**

    [webview loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:@"http://www.google.com"]]];

## Load HTML string
Web views are useful to load locally generated HTML strings.

    NSString *html = @"<!DOCTYPE html><html><body>Hello World</body></html>";
    [webView loadHTMLString:html baseURL:nil];

**Swift**
    
      let htmlString = "<h1>My First Heading</h1><p>My first paragraph.</p>"
      webView.loadHTMLString(htmlString, baseURL: nil)

A local base URL may be specified. This is useful to reference images, stylesheets or scripts from the app bundle:

    NSString *html = @"<!DOCTYPE html><html><head><link href='style.css' rel='stylesheet' type='text/css'></head><body>Hello World</body></html>";
    [self loadHTMLString:html baseURL:[NSURL fileURLWithPath:[[NSBundle mainBundle] resourcePath]]];

In this case, `style.css` is loaded locally from the app's resource directory. Of course it's also possible to specify a remote URL.

## Load JavaScript
We can run custom JavaScript on a `UIWebView` using the method `stringByEvaluatingJavaScriptFromString()`.This method returns the result of running the JavaScript script passed in the script parameter, or nil if the script fails.

**Swift**

**Load script from String**

    webview.stringByEvaluatingJavaScriptFromString("alert('This is JavaScript!');")

**Load script from Local file**

    //Suppose you have javascript file named "JavaScript.js" in project.
    let filePath = NSBundle.mainBundle().pathForResource("JavaScript", ofType: "js")
            do {
                let jsContent = try String.init(contentsOfFile: filePath!, encoding: NSUTF8StringEncoding)
                webview.stringByEvaluatingJavaScriptFromString(jsContent)
            }
            catch let error as NSError{
                print(error.debugDescription)
            }

**Objective-C**

**Load script from String**

    [webview stringByEvaluatingJavaScriptFromString:@"alert('This is JavaScript!');"];

**Load script from Local file**

    //Suppose you have javascript file named "JavaScript.js" in project.
    NSString *filePath = [[NSBundle mainBundle] pathForResource:@"JavaScript" ofType:@"js"];
    NSString *jsContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
    [webview stringByEvaluatingJavaScriptFromString:jsContent];
<br>

**Note**
The `stringByEvaluatingJavaScriptFromString:` method waits synchronously for JavaScript evaluation to complete. If you load web content whose JavaScript code you have not vetted, invoking this method could hang your app. Best practice is to adopt the `WKWebView` class and use its `evaluateJavaScript:completionHandler:` method instead. But `WKWebView` is available from iOS 8.0 and later.



## Stop Loading Web Content
Method `stopLoading()` stops the current loading process of the webview.

**Swift**

    webview.stopLoading()

**Objective-C**

    [webview stopLoading];

## Reload Current Web Content
**Swift**

    webview.reload()

**Objective-C**

    [webview reload];

## Load Document files like .pdf, .txt, .doc etc.
Instead of web pages, we can also load the document files into iOS WebView  like .pdf, .txt, .doc etc.. `loadData` method is used to load `NSData` into webview.

**Swift**

    //Assuming there is a text file in the project named "home.txt".
    let localFilePath = NSBundle.mainBundle().pathForResource("home", ofType:"txt");
    let data = NSFileManager.defaultManager().contentsAtPath(localFilePath!);
    webview.loadData(data!, MIMEType: "application/txt", textEncodingName:"UTF-8" , baseURL: NSURL())

**Objective-C**

    //Assuming there is a text file in the project named "home.txt".
    NSString *localFilePath = [[NSBundle mainBundle] pathForResource:@"home" ofType:@"txt"];
    NSData *data = [[NSFileManager defaultManager] contentsAtPath:localFilePath];
    [webview loadData:data MIMEType:@"application/txt" textEncodingName:@"UTF-8" baseURL:[NSURL new]];



## Make links That inside UIWebview clickable 
[![enter image description here][1]][1]

In vc.h

    @interface vc : UIViewController<UIWebViewDelegate>

in vc.m


    - (BOOL)webView:(UIWebView *)webView shouldStartLoadWithRequest:(NSURLRequest *)request navigationType:(UIWebViewNavigationType)navigationType{
        
        
        if (navigationType == UIWebViewNavigationTypeLinkClicked){
            //open it on browser if you want to open it in same web view remove return NO;
            NSURL *url = request.URL;
            if ([[UIApplication sharedApplication] canOpenURL:url]) {
                [[UIApplication sharedApplication] openURL:url];
            }
            return NO;
            
        }
        
        return YES;
        
    }


  [1]: http://i.stack.imgur.com/GMgcn.png

## Load local HTML file in webView
First, add the HTML File to your Project (If you are asked to choose options for adding the file, select *Copy items if needed*)


**The following line of code loads the content of the HTML file into the webView**

    webView.loadRequest(NSURLRequest(URL: NSURL(fileURLWithPath: NSBundle.mainBundle().pathForResource("YOUR HTML FILE", ofType: "html")!})

 - If your HTML file is called index.html replace **YOUR HTML FILE** with **index**
 - You can use this code either in *viewDidLoad()* or *viewDidAppear()* or any other function

