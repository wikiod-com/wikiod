---
title: "Web crawling with Jsoup"
slug: "web-crawling-with-jsoup"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Extracting email adresses & links to other pages
Jsoup can be used to extract links and email address from a webpage, thus "Web email address collector bot" First, this code uses a Regular expression to extract the email addresses, and then uses methods provided by Jsoup to extract the URLs of links on the page. 

    public class JSoupTest {
    
        public static void main(String[] args) throws IOException {
            Document doc = Jsoup.connect("http://stackoverflow.com/questions/15893655/").userAgent("Mozilla").get();
    
            Pattern p = Pattern.compile("[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+");
            Matcher matcher = p.matcher(doc.text());
            Set<String> emails = new HashSet<String>();
            while (matcher.find()) {
                emails.add(matcher.group());
            }
    
            Set<String> links = new HashSet<String>();
    
            Elements elements = doc.select("a[href]");
            for (Element e : elements) {
                links.add(e.attr("href"));
            }
            
            System.out.println(emails);
            System.out.println(links);
    
        }
    
    }

This code could also be easily extended to also recursively visit those URLs and extract data from linked pages. It could also easily be used with a different regex to extract other data.

(Please don't become a spammer!)

## Extracting JavaScript data with Jsoup
In this example, we will try to find JavaScript data which containing `backgroundColor:'#FFF'`. Then, we will change value of backgroundColor `'#FFF'` ⇨ `'#ddd'`. This code uses `getWholeData()` and `setWholeData()` methods to manipulate JavaScript data. Alternatively, `html()` method can be used to get data of JavaScript.

    // create HTML with JavaScript data
        StringBuilder html = new StringBuilder();
        html.append("<!DOCTYPE html> <html> <head> <title>Hello Jsoup!</title>");
        html.append("<script>");
        html.append("StackExchange.docs.comments.init({");
        html.append("highlightColor: '#F4A83D',");
        html.append("backgroundColor:'#FFF',");
        html.append("});");
        html.append("</script>");
        html.append("<script>");
        html.append("document.write(<style type='text/css'>div,iframe { top: 0; position:absolute; }</style>');");
        html.append("</script>\n");
        html.append("</head><body></body> </html>");
    
        // parse as HTML document
        Document doc = Jsoup.parse(html.toString());
    
        String defaultBackground = "backgroundColor:'#FFF'";
        // get <script>
        for (Element scripts : doc.getElementsByTag("script")) {
            // get data from <script>
            for (DataNode dataNode : scripts.dataNodes()) {
                // find data which contains backgroundColor:'#FFF'
                if (dataNode.getWholeData().contains(defaultBackground)) {
                    // replace '#FFF' -> '#ddd'
                    String newData = dataNode.getWholeData().replaceAll(defaultBackground, "backgroundColor:'#ddd'");
                    // set new data contents
                    dataNode.setWholeData(newData);
                }
            }
        }
        System.out.println(doc.toString());

**Output**

    <script>StackExchange.docs.comments.init({highlightColor: '#F4A83D',backgroundColor:'#ddd',});</script>

## Extracting all the URLs from a website using JSoup (recursion)
In this example we will extract all the web links from a website. I am using `http://stackoverflow.com/` for illustration. Here recursion is used, where each obtained link's page is parsed for presence of an `anchor tag` and that link is again submitted to the same function.

The condition `if(add && this_url.contains(my_site))` will limit results **to your domain only**. 

        import java.io.IOException;
        import java.util.HashSet;
        import java.util.Set;
        import org.jsoup.Jsoup;
        import org.jsoup.nodes.Document;
        import org.jsoup.select.Elements;
        
        public class readAllLinks {
        
            public static Set<String> uniqueURL = new HashSet<String>();
            public static String my_site;
        
            public static void main(String[] args) {
        
                readAllLinks obj = new readAllLinks();
                my_site = "stackoverflow.com";
                obj.get_links("http://stackoverflow.com/");
            }
        
            private void get_links(String url) {
                try {
                    Document doc = Jsoup.connect(url).userAgent("Mozilla").get();
                    Elements links = doc.select("a");

                    if (links.isEmpty()) {
                       return;
                    }

                    links.stream().map((link) -> link.attr("abs:href")).forEachOrdered((this_url) -> {
                        boolean add = uniqueURL.add(this_url);
                        if (add && this_url.contains(my_site)) {
                            System.out.println(this_url);
                            get_links(this_url);
                        }
                    });
        
                } catch (IOException ex) {
        
                }
        
            }
        }

The program will take much time to execute depending on your website. The above code can be extended to extract data (like titles of pages or text or images) from particular website. I would recommend you to go through company's [terms of use][1] before scarping it's website.

The example uses JSoup library to get the links, you can also get the links using `your_url/sitemap.xml`.


  [1]: https://en.wikipedia.org/wiki/Terms_of_service

