---
title: "Parsing Javascript Generated Pages"
slug: "parsing-javascript-generated-pages"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Parsing JavaScript Generated Page with Jsoup and HtmUnit
**page.html - source code**

    <html>
    <head>
        <script src="loadData.js"></script>
    </head>
    <body onLoad="loadData()">
        <div class="container">
            <table id="data" border="1">
                <tr>
                    <th>col1</th>
                    <th>col2</th>
                </tr>
            </table>
        </div>
    </body>
    </html>



**loadData.js**

        // append rows and cols to table.data in page.html
        function loadData() {
            data = document.getElementById("data");
            for (var row = 0; row < 2; row++) {
                var tr = document.createElement("tr");
                for (var col = 0; col < 2; col++) {
                    td = document.createElement("td");
                    td.appendChild(document.createTextNode(row + "." + col));
                    tr.appendChild(td);
                }
                data.appendChild(tr);
            }
        }


**page.html when loaded to browser**

| Col1 | Col2 |
| ------ | ------ |
| 0.0   | 0.1   |
| 1.0   | 1.1   |

**Using jsoup to parse page.html for col data**

        // load source from file
        Document doc = Jsoup.parse(new File("page.html"), "UTF-8");

        // iterate over row and col
        for (Element row : doc.select("table#data > tbody > tr"))

            for (Element col : row.select("td"))
                
                // print results
                System.out.println(col.ownText());


**Output**

(empty)


**What happened?**

Jsoup parses the source code as delivered from the server (or in this case loaded from file).  It does not invoke client-side actions such as JavaScript or CSS DOM manipulation.  In this example, the rows and cols are never appended to the data table.

**How to parse my page as rendered in the browser?**

        // load page using HTML Unit and fire scripts
        WebClient webClient = new WebClient();
        HtmlPage myPage = webClient.getPage(new File("page.html").toURI().toURL());

        // convert page to generated HTML and convert to document
        doc = Jsoup.parse(myPage.asXml());
     
        // iterate row and col
        for (Element row : doc.select("table#data > tbody > tr"))

            for (Element col : row.select("td"))

                // print results
                System.out.println(col.ownText());

        // clean up resources        
        webClient.close();

**Output**

    0.0
    0.1
    1.0
    1.1


