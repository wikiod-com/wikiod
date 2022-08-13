---
title: "Getting started with beautifulsoup"
slug: "getting-started-with-beautifulsoup"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
[pip][1] may be used to install BeautifulSoup. To install Version 4 of BeautifulSoup, run the command:

    pip install beautifulsoup4

Be aware that the package name is `beautifulsoup4` instead of `beautifulsoup`, the latter name stands for old release, see [old beautifulsoup][2]

  [1]: https://pypi.python.org/pypi/pip
  [2]: https://pypi.python.org/pypi/BeautifulSoup

## A BeautifulSoup "Hello World" scraping example
    from bs4 import BeautifulSoup
    import requests
    
    main_url = "https://fr.wikipedia.org/wiki/Hello_world"
    req = requests.get(main_url)
    soup = BeautifulSoup(req.text, "html.parser")
    
    # Finding the main title tag.
    title = soup.find("h1", class_ = "firstHeading")
    print title.get_text()
    
    # Finding the mid-titles tags and storing them in a list.
    mid_titles = [tag.get_text() for tag in soup.find_all("span", class_ = "mw-headline")]
    
    # Now using css selectors to retrieve the article shortcut links
    links_tags = soup.select("li.toclevel-1")
    for tag in links_tags:
        print tag.a.get("href")
    
    # Retrieving the side page links by "blocks" and storing them in a dictionary
    side_page_blocks = soup.find("div",
                                id = "mw-panel").find_all("div",
                                                          class_ = "portal")
    blocks_links = {}
    for num, block in enumerate(side_page_blocks):
        blocks_links[num] = [link.get("href") for link in block.find_all("a", href = True)]
    
    print blocks_links[0]

Output:

    "Hello, World!" program
    #Purpose
    #History
    #Variations
    #See_also
    #References
    #External_links
    [u'/wiki/Main_Page', u'/wiki/Portal:Contents', u'/wiki/Portal:Featured_content', u'/wiki/Portal:Current_events', u'/wiki/Special:Random', u'https://donate.wikimedia.org/wiki/Special:FundraiserRedirector?utm_source=donate&utm_medium=sidebar&utm_campaign=C13_en.wikipedia.org&uselang=en', u'//shop.wikimedia.org']


Entering your prefered parser when instanciating Beautiful Soup avoids the usual `Warning` declaring that `no parser was explicitely specified`.

Different methods can be used to find an element within the
webpage tree.

Although a handful of other methods exist, `CSS classes` and `CSS selectors` are two handy ways to find elements in the tree.

It should be noted that we can look for tags by setting their attribute value to True when searching them.

`get_text()` allows us to retrieve text contained within a tag. It returns it as a single Unicode string.
`tag.get("attribute")` allows to get a tag's attribute value.



