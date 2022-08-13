---
title: "Getting started with Jsoup"
slug: "getting-started-with-jsoup"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Extract the URLs and titles of links
Jsoup can be be used to easily extract all links from a webpage. In this case, we can use Jsoup to extract only specific links we want, here, ones in a `h3` header on a page. We can also get the text of the links.

    Document doc = Jsoup.connect("http://stackoverflow.com").userAgent("Mozilla").get();
    for (Element e: doc.select("a.question-hyperlink")) {
        System.out.println(e.attr("abs:href"));
        System.out.println(e.text());
        System.out.println();
    }
    
This gives the following output:

    http://stackoverflow.com/questions/12920296/past-5-week-calculation-in-webi-bo-4-0
    Past 5 week calculation in WEBI (BO 4.0)?
    
    http://stackoverflow.com/questions/36303701/how-to-get-information-about-the-visualized-elements-in-listview
    How to get information about the visualized elements in listview?

    [...]

What's happening here:

* First, we get the HTML document from the specified URL. This code also sets the User Agent header of the request to "Mozilla", so that the website serves the page it would usually serve to browsers.

* Then, use `select(...)` and a for loop to get all the links to Stack Overflow questions, in this case links which have the class `question-hyperlink`.

* Print out the text of each link with `.text()` and the href of the link with `attr("abs:href")`. In this case, we use `abs:` to get the _absolute_ URL, ie. with the domain and protocol included.
    

## Extract full URL from partial HTML
Selecting only the attribute value of a link:href will return the relative URL.

       String bodyFragment = 
              "<div><a href=\"/documentation\">Stack Overflow Documentation</a></div>";

 
        Document doc = Jsoup.parseBodyFragment(bodyFragment);
        String link = doc
                .select("div > a")
                .first()
                .attr("href");
        
        System.out.println(link);

**Output**

    /documentation

----

By passing the base URI into the <code><a href="https://jsoup.org/apidocs/org/jsoup/Jsoup.html#parse-java.lang.String-java.lang.String-">parse</a></code> method and using the <code><a href="https://jsoup.org/apidocs/org/jsoup/nodes/Node.html#absUrl-java.lang.String-">absUrl</a></code> method instead of <code><a href="https://jsoup.org/apidocs/org/jsoup/nodes/Node.html#attr-java.lang.String-">attr</a></code>, we can extract the full URL.


        Document doc = Jsoup.parseBodyFragment(bodyFragment, "http://stackoverflow.com");
        
        String link = doc
                    .select("div > a")
                    .first()
                    .absUrl("href");
        
        System.out.println(link);


**Output**

    https://www.wikiod.com/docs/

## Extract the data from HTML document file
Jsoup can be used to manipulate or extract data from a **file** on local that contains HTML. `filePath` is path of a file on disk. `ENCODING` is desired Charset Name e.g. "Windows-31J". It is optional.
   

     // load file
        File inputFile = new File(filePath);
        // parse file as HTML document
        Document doc = Jsoup.parse(filePath, ENCODING);
        // select element by <a> 
        Elements elements = doc.select("a");

