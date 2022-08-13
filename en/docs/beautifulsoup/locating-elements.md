---
title: "Locating elements"
slug: "locating-elements"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Locate a text after an element in BeautifulSoup
Imagine you have the following HTML:

    <div>
        <label>Name:</label>
        John Smith
    </div>

And you need to locate the text "John Smith" after the `label` element.

In this case, you can locate the `label` element by text and then use [`.next_sibling` property][1]:

    from bs4 import BeautifulSoup
    
    data = """
    <div>
        <label>Name:</label>
        John Smith
    </div>
    """
    
    soup = BeautifulSoup(data, "html.parser")
    
    label = soup.find("label", text="Name:")
    print(label.next_sibling.strip())

Prints `John Smith`.


  [1]: https://www.crummy.com/software/BeautifulSoup/bs4/doc/#next-sibling-and-previous-sibling

## Locating comments
To locate comments in `BeautifulSoup`, use the `text` (or [`string`][1] in the recent versions) argument checking the type to be [`Comment`][2]:

    from bs4 import BeautifulSoup
    from bs4 import Comment
    
    data = """
    <html>
        <body>
            <div>
            <!-- desired text -->
            </div>
        </body>
    </html>
    """
    
    soup = BeautifulSoup(data, "html.parser")
    comment = soup.find(text=lambda text: isinstance(text, Comment))
    print(comment)

Prints `desired text`.


  [1]: https://www.crummy.com/software/BeautifulSoup/bs4/doc/#the-string-argument
  [2]: https://www.crummy.com/software/BeautifulSoup/bs4/doc/#comments-and-other-special-strings

## Filter functions
BeautifulSoup allows you to filter results by providing a function to `find_all` and similar functions. This can be useful for complex filters as well as a tool for code reuse.


Basic usage
===========

Define a function that takes an element as its only argument. The function should return `True` if the argument matches.


    def has_href(tag):
        '''Returns True for tags with a href attribute'''
        return  bool(tag.get("href"))

    soup.find_all(has_href) #find all elements with a href attribute
    #equivilent using lambda:
    soup.find_all(lambda tag: bool(tag.get("href")))

Another example that finds tags with a `href` value that do not start with

Providing additional arguments to filter functions
==================

Since the function passed to `find_all` can only take one argument, it's sometimes useful to make 'function factories' that produce functions fit for use in `find_all`. This is useful for making your tag-finding functions more flexible.



    def present_in_href(check_string):
        return lambda tag: tag.get("href") and check_string in tag.get("href")
    
    soup.find_all(present_in_href("/partial/path"))








## Using CSS selectors to locate elements in BeautifulSoup
BeautifulSoup has a [limited support for CSS selectors][1], but covers most commonly used ones. Use `select()` method to find multiple elements and `select_one()` to find a single element.

Basic example:

    from bs4 import BeautifulSoup
    
    data = """
    <ul>
        <li class="item">item1</li>
        <li class="item">item2</li>
        <li class="item">item3</li>
    </ul>
    """
    
    soup = BeautifulSoup(data, "html.parser")
    
    for item in soup.select("li.item"):
        print(item.get_text())

Prints:

    item1
    item2
    item3


  [1]: https://www.crummy.com/software/BeautifulSoup/bs4/doc/#css-selectors

## Accessing internal tags and their attributes of initially selected tag
Let's assume you got an `html` after selecting with `soup.find('div', class_='base class')`:

    from bs4 import BeautifulSoup

    soup = BeautifulSoup(SomePage, 'lxml')
    html = soup.find('div', class_='base class')
    print(html)
    
    <div class="base class">
      <div>Sample text 1</div>
      <div>Sample text 2</div>
      <div>
        <a class="ordinary link" href="https://example.com">URL text</a>
      </div>
    </div>
    
    <div class="Confusing class"></div>
    '''
And if you want to access `<a>` tag's `href`, you can do it this way:

    a_tag = html.a
    link = a_tag['href']
    print(link)

    https://example.com

This is useful when you can't directly select `<a>` tag because it's `attrs` don't give you unique identification, there are other "twin" `<a>` tags in parsed page. But you can uniquely select a parent tag which contains needed `<a>`.

## Collecting optional elements and/or their attributes from series of pages
Let's consider situation when you parse number of pages and you want to collect value from element that's _optional_ (can be presented on one page and can be absent on another) for a paticular page.

Moreover the element itself, for example, is the _most ordinary element_ on page, in other words no specific attributes can uniquely locate it. But you see that you can properly select its parent element and you know wanted element's _order number_ in the respective nesting level.

    from bs4 import BeautifulSoup
    
    soup = BeautifulSoup(SomePage, 'lxml')
    html = soup.find('div', class_='base class') # Below it refers to html_1 and html_2

Wanted element is optional, so there could be 2 situations for `html` to be:

    html_1 = '''
    <div class="base class">    # №0
      <div>Sample text 1</div>  # №1
      <div>Sample text 2</div>  # №2  
      <div>!Needed text!</div>  # №3
    </div>
    
    <div>Confusing div text</div>  # №4
    '''
            
    html_2 = '''
    <div class="base class">    # №0
      <div>Sample text 1</div>  # №1
      <div>Sample text 2</div>  # №2  
    </div>
    
    <div>Confusing div text</div>  # №4
    '''

If you got `html_1` you can collect `!Needed text!` from tag №3 this way:

    wanted tag = html_1.div.find_next_sibling().find_next_sibling() # this gives you whole tag №3
It initially gets №1 `div`, then 2 times switches to next `div` on same nesting level to get to №3.

    wanted_text = wanted_tag.text # extracting !Needed text!

Usefulness of this approach comes when you get `html_2` - approach won't give you error, it will give `None`:

    print(html_2.div.find_next_sibling().find_next_sibling())
    None
Using `find_next_sibling()` here is crucial because it limits element search by respective nesting level. If you'd use `find_next()` then tag №4 will be collected and you don't want it:

    print(html_2.div.find_next().find_next())
    <div>Confusing div text</div>

You also can explore `find_previous_sibling()` and `find_previous()` which work straight opposite way. 

All described functions have their _miltiple_ variants to catch all tags, not just the first one:

    find_next_siblings()
    find_previous_siblings()
    find_all_next()
    find_all_previous()



