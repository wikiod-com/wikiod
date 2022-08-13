---
title: "Very simple CRUD examples for XML documents"
slug: "very-simple-crud-examples-for-xml-documents"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Create a simple document
This very simple snippet of XQuery can be executed in QueryConsole using the built-in "Documents" database as a sandbox. Each piece of the snippet has a comment to explain what the following line of code means.

    xquery version "1.0-ml";
    (: Let's first insert a simple document to get started :)
    (: You need a URI- the location where the document is found in the database :)
    let $uri := "/stuff/mysimpledocument.xml"
    (: Documents need content. This is a simple XML node :)
    let $doc := 
      <my-document>
        <body>Very simple example</body>
      </my-document>
    (: Permissions are a big topic. For now, we'll use the default permissions. :)
    let $permissions := xdmp:default-permissions()
    (: Document collections are optional. One or more can be specified :)
    (: Adding a collection for further examples that will use it. :)
    let $collections := "simple-example"
    (: Now we're just going to insert this document in the database :)
    let $insert := xdmp:document-insert($uri,$doc,$permissions,$collections)
    return <message>Document saved to {$uri}</message>

When this is executed, the console returns

    <message>Document saved to /stuff/mysimpledocument.xml</message>

Now that the document exists, we can test additional operations...





## Read/access our sample document
If we know the desired URI of the document we are looking for:

    fn:doc("/stuff/mysimpledocument.xml")

*Returns the full document from the database, using the URI to locate it.*

Since this is XQuery, we can use XPath to find the document when we know about the structure, but not the URI:

    /my-document

*Returns the element "my-document" and its contents...*


## Update the simple document
We will now add some additional XML nodes to the "my-document" element and update the document. The snippet again contains comments to explain what is happening.

    xquery version "1.0-ml";
    (: We are preserving the same URI as we used originally :)
    let $uri := "/stuff/mysimpledocument.xml"
    (: Need to get the existing contents so we can append to those :)
    let $orig-content := fn:doc($uri)/my-document
    (: Documents need content. This is a simple XML node :)
    let $new-content := 
      <notes>
        <note>Anything can be changed</note>
        <note>New content is added in this example, but we could replace it all too</note>
      </notes>
    (: Now to build the new xml. There's lots of ways to do this :)
    (: line 17 inserts the original contents into this new node construct :)
    (: line 18 inserts the $new-content after the original contents :)
    let $new-doc-content :=
      <my-document>
        {$orig-content/node()}
        {$new-content}
      </my-document>
    (: Leave permissions untouched... :)
    let $permissions := xdmp:document-get-permissions($uri)
    (: Leave collections untouched... :)
    let $collections := xdmp:document-get-collections($uri)
    (: Now we're just going to insert this document in the database :)
    let $insert := xdmp:document-insert($uri,$new-doc-content,$permissions,$collections)
    return <message>Document {$uri} updated</message>

When executed, this returns the message

    <message>Document /stuff/mysimpledocument.xml updated</message>

Now run the different read commands above to see the updated content. It should look like this:

    <my-document>
        <body>Very simple example</body>
        <notes>
            <note>Anything can be changed</note>
            <note>New content is added in this example, but we could replace it all too</note>
        </notes>
    </my-document>


## Bonus: Simple Search example (Another way to read)
MarkLogic is first and foremost a search engine, so let's use two different methods to search for this document.

**Using search:search()**

This gives a peek into using search:search() to develop search applications. This library provides Google-like search results and will likely speed up your development of simple search tools. More information and a deeper dive [can be found here][1].

    xquery version "1.0-ml";
    import module namespace search = "http://marklogic.com/appservices/search"
         at "/MarkLogic/appservices/search/search.xqy";
    (: What is search without a keyword? :)
    let $term := "very simple"
    return search:search($term)

The result looks a bit confusing, but you can see that it returns one result, our example document.

**Using cts:search()**

More advanced search situations might call for more granular search capabilities. This is just to whet your appetite for what is available in search. More detailed information [is found here][2].

    xquery version "1.0-ml";
    (: What is search without a keyword? :)
    let $term := "very simple"
    (: Complex queries can be made from individual cts queries. Here, we just have one simple query :)
    let $query := cts:word-query($term,"case-insensitive")
    (: Return the documents that match the query :)
    return cts:search(fn:doc(),$query)

This is an incredibly simple example. BTW, if we want to get back the URI for the matching documents, instead of the documents themselves, we can change the last line of this snippet to- 

    return 
      for $result in cts:search(fn:doc(),$query)
        return fn:base-uri($result)


  [1]: http://docs.marklogic.com/search
  [2]: http://docs.marklogic.com/guide/search-dev

## Delete - Last, but not least
To round out simple examples of CRUD operations, we present the following examples. Always use great care in deleting documents.

    (: When we know the URI, we can delete it very easily :)
    let $uri := "/stuff/mysimpledocument.xml"
    return xdmp:document-delete($uri)

or simplified:

    xdmp:document-delete("/stuff/mysimpledocument.xml")

You can certainly use XPath to find the document, get the URI of it, and then delete it with something like this, but the danger is that any documents that are returned by the XPath expressions will be removed. Not always a good thing.

    (: Use caution when using XPath to select target docs to delete :)
    for $doc in /my-document
    return xdmp:document-delete(fn:base-uri($doc))

Want to delete all documents? This will do it, but be very careful you know what database your code will execute against.

    for $doc in fn:doc()
    return xdmp:document-delete(fn:base-uri($doc))



