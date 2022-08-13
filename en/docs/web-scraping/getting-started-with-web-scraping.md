---
title: "Getting started with web-scraping"
slug: "getting-started-with-web-scraping"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Web Scraping in Python (using BeautifulSoup)
When performing data science tasks, it’s common to want to use data found on the internet. You’ll usually be able to access this data via an Application Programming Interface(API) or in other formats. However, there are times when the data you want can only be accessed as part of a web page. In cases like this, a technique called web scraping comes into picture.  
To apply this technique to get data from web-pages, we need to have basic knowledge about web-page structure and tags used in web-page development(i.e, `<html>` ,`<li>`,`<div>` etc.,). If you are new to web development you can learn it [here][1].

So to start with web scrapping, we'll use a simple [website][2]. We'll use `requests` module to get the web-page content OR source code.

    import requests
    page = requests.get("http://dataquestio.github.io/web-scraping-pages/simple.html")
    print (page.content) ## shows the source code
 Now we'll use bs4 module to scrap the content to get the useful data.

    from bs4 import BeautifulSoup
    soup = BeautifulSoup(page.content, 'html.parser')
    print(soup.prettify()) ##shows source in html format
You can find the required tags using `inspect element` tool in your browser.Now let's say you want to get all the data that is stored with `<li>` tag.Then you can find it with the script 

    soup.find_all('li')
    # you can also find all the list items with class='ABC'
    # soup.find_all('p', class_='ABC')
    # OR all elements with class='ABC'
    # soup.find_all(class_="ABC")
    # OR all the elements with class='ABC'
    # soup.find_all(id="XYZ")
Then you can get the text in the tag using 

    for i in range(len(soup.find_all('li'))):
        print (soup.find_all('li')[i].get_text())

The whole script is small and pretty simple. 

    import requests
    from bs4 import BeautifulSoup
    
    page = requests.get("http://dataquestio.github.io/web-scraping-pages/simple.html") #get the page
    soup = BeautifulSoup(page.content, 'html.parser') # parse according to html
    soup.find_all('li') #find required tags
    
    for i in range(len(soup.find_all('li'))):
        print (soup.find_all('li')[i].get_text())
    

  [1]: https://www.w3schools.com/
  [2]: https://www.york.ac.uk/teaching/cws/wws/webpage1.html

