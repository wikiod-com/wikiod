---
title: "Linking"
slug: "linking"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Inline Link
The form for a link in markdown is as follows.

    [Shown Text](Link)

For example, [This will take you to Example.com](http://www.example.com) is created with   
    
    [This will take you to Example.com](http://www.example.com)


## Reference Link
URLs for links can be specified later in the document.

**Markdown**

    [Text1][1] will link to the first link, and [Text2][2] to the second.
    You [can reuse][1] names, and give longer names [like this one][a link].
    You can also link text [like this] without giving the reference an explicit name.
    
    [1]: http://www.google.com
    [2]: http://stackoverflow.com/
    [a link]: http://example.org/
    [like this]: http://stackexchange.com/

**Output**

> [Text1][1] will link to the first link, and [Text2][2] to the second.
You [can reuse][1] names, and give longer names [like this one][a link].
You can also link text [like this] without giving the reference an explicit name.

[1]: http://www.google.com
[2]: http://stackoverflow.com/
[a link]: http://example.org/
[like this]: http://stackexchange.com/

## Named anchors (link to a fragment of page)
  
  create destination with  

`<a id="destinationLinkName"></a>`

link to destination

`[link text](#destinationLinkName)`  


