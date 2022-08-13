---
title : beautifulsoup Tutorial
slug : beautifulsoup-tutorial
weight : 9957
draft : false
images : []
type : docs
---

In this section, we discuss what Beautiful Soup is, what it is used for and a brief outline on how to go about using it. 

Beautiful Soup is a Python library that uses your pre-installed html/xml parser and converts the web page/html/xml into a tree consisting of tags, elements, attributes and values. To be more exact, the tree consists of four types of objects, Tag, NavigableString, BeautifulSoup and Comment. This tree can then be "queried" using the methods/properties of the BeautifulSoup object that is created from the parser library. 

**Your need :** Often, you may have one of the following needs :
1. You might want to parse a web page to determine, how many of what tags are found, how many elements of each tag are found and their values. You might want to change them. 

2. You might want to determine element names and values, so that you can use them in conjunction with other libraries for web page automation, such as [Selenium][1]. 

3. You might want to transfer/extract data shown in a web page to other formats, such as a CSV file or to a relational database such as SQLite or mysql. In this case, the library helps you with the first step, of understanding the structure of the web page, although you will be using other libraries to do the act of transfer. 

4. You might want to find out how many elements are styled with a certain CSS style and which ones. 

**Sequence** for typical basic use in your Python code: 

1. Import the Beautiful Soup library 

2. Open a web page or html-text with the BeautifulSoup library, by mentioning which parser to be used. The result of this step is a BeautifulSoup object. (Note: This parser name mentioned, must be installed already as part of your Python pacakges. For instance, `html.parser`, is an in-built, 'with-batteries' package shipped with Python. You could install other parsers such as `lxml` or `html5lib`. )

3. "Query" or search the BeautifulSoup object using the syntax `'object.method'` and obtain the result into a collection, such as a Python dictionary. For some methods, the output will be a simple value. 

4. Use the result from the previous step to do whatever you want to do with it, in rest of your Python code. You can also modify the element values or attribute values in the tree object. Modifications don't affect the source of the html code, but you can call output formatting methods (such as `prettify`) to create new output from the BeautifulSoup object. 

**Commonly used methods:** Typically, the `.find` and `.find_all` methods are used to search the tree, giving the input arguments.
 
The input arguments are : the tag name that is being sought, attribute names and other related arguments. These arguments could be presented as : a string, a regular expression, a list or even a function. 

**Common uses** of the BeautifulSoup object include :

1. Search by CSS class
2. Search by Hyperlink address
3. Search by Element Id, tag
4. Search by Attribute name. Attribute value. 

If you have a need to filter the tree with a combination of the above criteria, you could also write a function that evaluates to true or false, and search by that function. 



  [1]: http://selenium-python.readthedocs.io/

