---
title: "Selectors"
slug: "selectors"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

A selector is a chain of simple selectors, separated by combinators. Selectors are case insensitive (including against elements, attributes, and attribute values).

The universal selector (*) is implicit when no element selector is supplied (i.e. *.header and .header is equivalent).

| Pattern | Matches | Example |
| ------- | ------- | ------- |
| `*` | any element | `*` |
| `tag` | elements with the given tag name | `div` |
| <code>ns&#124;E</code> | elements of type E in the namespace ns | <code>fb&#124;name finds <fb:name> elements</code> |
| `#id` | elements with attribute ID of "id" | `div#wrap, #logo` |
| `.class` | elements with a class name of "class" | `div.left, .result` |
| `[attr]` | elements with an attribute named "attr" (with any value) | `a[href], [title]` |
| `[^attrPrefix]` | elements with an attribute name starting with "attrPrefix". Use to find elements with HTML5 datasets | `[^data-], div[^data-]` |
| `[attr=val]` | elements with an attribute named "attr", and value equal to "val" | `img[width=500], a[rel=nofollow]` |
| `[attr="val"]` | elements with an attribute named "attr", and value equal to "val" | `span[hello="Cleveland"][goodbye="Columbus"], a[rel="nofollow"]` |
| `[attr^=valPrefix]` | elements with an attribute named "attr", and value starting with "valPrefix" | `a[href^=http:]` |
| `[attr$=valSuffix]` | elements with an attribute named "attr", and value ending with "valSuffix" | `img[src$=.png]` |
| `[attr*=valContaining]` | elements with an attribute named "attr", and value containing "valContaining" | `a[href*=/search/]` |
| `[attr~=regex]` | elements with an attribute named "attr", and value matching the regular expression | <code>img[src~=(?i)\\.(png&#124;jpe?g)]</code> |
| | The above may be combined in any order | `div.header[title]` |

[Selector full reference][1]


  [1]: https://jsoup.org/apidocs/org/jsoup/select/Selector.html

## Selecting elements using CSS selectors
    String html = "<!DOCTYPE html>" +
                  "<html>" +
                    "<head>" +
                      "<title>Hello world!</title>" +
                    "</head>" +
                    "<body>" +
                      "<h1>Hello there!</h1>" +
                      "<p>First paragraph</p>" +
                      "<p class=\"not-first\">Second paragraph</p>" +
                      "<p class=\"not-first third\">Third <a href=\"page.html\">paragraph</a></p>" +
                    "</body>" +
                  "</html>";
    
    // Parse the document
    Document doc = Jsoup.parse(html);
    
    // Get document title
    String title = doc.select("head > title").first().text();
    System.out.println(title); // Hello world!
    
    Element firstParagraph = doc.select("p").first();
    
    // Get all paragraphs except from the first
    Elements otherParagraphs = doc.select("p.not-first");
    // Same as
    otherParagraphs = doc.select("p");
    otherParagraphs.remove(0);

    // Get the third paragraph (second in the list otherParagraphs which
    // excludes the first paragraph)
    Element thirdParagraph = otherParagraphs.get(1);
    // Alternative:
    thirdParagraph = doc.select("p.third");

    // You can also select within elements, e.g. anchors with a href attribute
    // within the third paragraph.
    Element link = thirdParagraph.select("a[href]");
    // or the first <h1> element in the document body
    Element headline = doc.select("body").first().select("h1").first();

You can find a detailed overview of supported selectors [here][1].


  [1]: http://jsoup.org/cookbook/extracting-data/selector-syntax

## Extract Twitter Markup
        // Twitter markup documentation: 
        // https://dev.twitter.com/cards/markup
        String[] twitterTags = {
                "twitter:site", 
                "twitter:site:id", 
                "twitter:creator", 
                "twitter:creator:id", 
                "twitter:description", 
                "twitter:title", 
                "twitter:image", 
                "twitter:image:alt", 
                "twitter:player", 
                "twitter:player:width", 
                "twitter:player:height", 
                "twitter:player:stream", 
                "twitter:app:name:iphone", 
                "twitter:app:id:iphone", 
                "twitter:app:url:iphone", 
                "twitter:app:name:ipad", 
                "twitter:app:id:ipad", 
                "twitter:app:url:ipadt",
                "twitter:app:name:googleplay", 
                "twitter:app:id:googleplay", 
                "twitter:app:url:googleplay"        
        };
        
        // Connect to URL and extract source code
        Document doc = Jsoup.connect("http://stackoverflow.com/").get();
        
        for (String twitterTag : twitterTags) {
            
            // find a matching meta tag
            Element meta = doc.select("meta[name=" + twitterTag + "]").first();
            
            // if found, get the value of the content attribute
            String content = meta != null ? meta.attr("content") : "";
            
            // display results
            System.out.printf("%s = %s%n", twitterTag, content);
        }


**Output**

    twitter:site = 
    twitter:site:id = 
    twitter:creator = 
    twitter:creator:id = 
    twitter:description = Q&A for professional and enthusiast programmers
    twitter:title = Stack Overflow
    twitter:image = 
    twitter:image:alt = 
    twitter:player = 
    twitter:player:width = 
    twitter:player:height = 
    twitter:player:stream = 
    twitter:app:name:iphone = 
    twitter:app:id:iphone = 
    twitter:app:url:iphone = 
    twitter:app:name:ipad = 
    twitter:app:id:ipad = 
    twitter:app:url:ipadt = 
    twitter:app:name:googleplay = 
    twitter:app:id:googleplay = 
    twitter:app:url:googleplay = 



