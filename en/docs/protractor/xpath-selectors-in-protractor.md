---
title: "XPath selectors in Protractor"
slug: "xpath-selectors-in-protractor"
draft: false
images: []
weight: 9402
type: docs
toc: true
---

## Selecting a DOM element using protractor
Apart from CSS, model, and binding selectors, protractor can also locate elements using xpath
View 

    <ul>
    <li><a href='http://www.google.com'>Go to google</a></li>
    </ul>

Code

    var googleLink= element(by.xpath('//ul/li/a'));
    expect(element.getText()).to.eventually.equal('Go to google','The text you mention was not found');

## Selecting elements with specific attributes
XPath selectors can be used to select elements with specific attributes, such as class, id, title etc. 

By Class
========

View:

    <div class="HakunaMatata"> Hakuna Matata </div>

Code: 

 

    var theLionKing= element(by.xpath('//div[@class="HakunaMatata"]'));
    expect(theLionKing.getText()).to.eventually.equal('Hakuna Matata', "Text not found");

However, an element can have multiple classes. In such cases, the 'contains' workaround can be used

View:
 

    <div class="Hakuna Matata"> Hakuna Matata </div>

Code:

    var theLionKing= element(by.xpath('//div[conatins(@class,"Hakuna")]'));
    expect(theLionKing.getText()).to.eventually.equal('Hakuna Matata', "Text not found");

The above piece of code will return elements containing both 'class="HakunaMatata"' and 'class="Hakuna Matata"'. If your search text is a part of a space-separated list, then the following workaround may be used:

    var theLionKing= element(by.xpath('//div[contains(concat(' ',normalize-space(@class),' '), "Hakuna")]'));
    expect(theLionKing.getText()).to.eventually.equal('Hakuna Matata', "Text not found");

By id
=====

ID remains the easiest and the most precise locator which can be used to select an element.

View:

    <div id="HakunaMatata">Hakuna Matata</div>

Code:

    var theLionKing= element(by.xpath('//div[@id="HakunaMatata"])');
    expect(theLionKing.getText()).to.eventually.equal('Hakuna Matata', "Text not found");

As with classes, the contains function can be used to find an element containing the given text.

Other attributes
================

Finding an element with a given **title** attribute

View

 

    <div title="Hakuna Matata">Hakuna Matata</div>

Code

    var theLionKing= element(by.xpath('//div[@title="Hakuna Matata"]'));
    expect(theLionKing.getText()).to.eventually.equal('Hakuna Matata', "Text not found");

 Selecting an element with a specific text

View

    <div class="Run Simba Run">Run Simba</div>

Code

    var runSimba= element(by.xpath('//div[text()="Run Simba"]'));
As with other text based searches, the contains function can be used to select elements with text() containing the required match.

View

    <div class="Run Simba Run">Run Simba,run</div>

Code
   

     var runSimba= element(by.xpath('//div[contains(text(),"Run Simba")]'));
    expect(runSimba.getText()).to.eventually.equal('Run Simba, run', "Text not found"); //true

Selecting an element with a specific name attribute

View 

    <input type="text" name="FullName"></input>

Code

    var fullNameInput= element(by.xpath('//input[@name="FullName"]'));
    fullNameInput.sendKeys("John Doe");


