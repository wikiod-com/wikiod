---
title: "Lists"
slug: "lists"
draft: false
images: []
weight: 9790
type: docs
toc: true
---

HTML offers three ways for specifying lists: ordered lists, unordered lists, and description lists. Ordered lists use ordinal sequences to indicate the order of list elements, unordered lists use a defined symbol such as a bullet to list elements in no designated order, and description lists use indents to list elements with their children. This topic explains the implementation and combination of these lists in HTML markup.

## Syntax
 - `<ol> ordered list items </ol>` 
 - `<ul> unordered list items </ul>`
 - `<li> list item (ordered and not) </li>`
 - `<dl> description list items </dl>`
 - `<dt> description list title </dt>`
 - `<dd> description list description </dd>`

**See Also**

You can add a list-style-type CSS property to an `<ul>` tag in order to change what kind of icon is used to mark each list item, for example `<ul style="list-style-type:disc">`. The following list-style-types are permitted:

 - "list-style-type:disc" is the default dot.
 - "list-style-type:circle" is an unfilled circle.
 - "list-style-type:square" is a filled square.
 - "list-style-type:none" uses no mark at all.

You can also add a type attribute to an `<ol>` tag in order to change how numbering is done, for example `<ol type="1">`. The following types are permitted:

 - type="1" is the default form.
 - type="A" uses uppercase letters in alphabetical order
 - type="a" uses lowercase letters in alphabetical order
 - type="I" uses roman numerals with uppercase lettering
 - type="i" uses roman numerals with lowercase lettering

## Ordered List
An ordered list can be created with the `<ol>` tag and each list item can be created with the `<li>` tag as in the example below:

    <ol>
      <li>Item</li>
      <li>Another Item</li>
      <li>Yet Another Item</li>
    </ol>

This will produce a numbered list (which is the default style):

><ol>
>  <li>Item</li>
>  <li>Another Item</li>
>  <li>Yet Another Item</li>
></ol>

---- 

### Manually changing the numbers ###
 
There are a couple of ways you can play with which numbers appear on the list items in an ordered list. The first way is to set a starting number, using the `start` attribute. The list will start at this defined number, and continue incrementing by one as usual.

    <ol start="3">
      <li>Item</li>
      <li>Some Other Item</li>
      <li>Yet Another Item</li>
    </ol>

This will produce a numbered list (which is the default style):

><ol start="3">
>  <li>Item</li>
>  <li>Some Other Item</li>
>  <li>Yet Another Item</li>
></ol>

You can also explicitly set a certain list item to a specific number. Further list items after one with a specified value will continue incrementing by one from that list item's value, ignoring where the parent list was at.

    <li value="7"></li>

It is also worth noting that, by using the `value` attribute directly on a list item, you can override an ordered list's existing numbering system by restarting the numbering at a lower value. So if the parent list was already up to value 7, and encountered a list item at value 4, then that list item would still display as 4 and continue counting from that point again.

    <ol start="5">
      <li>Item</li>
      <li>Some Other Item</li>
      <li value="4">A Reset Item</li>
      <li>Another Item</li>
      <li>Yet Another Item</li>
    </ol>

So the example above will produce a list that follows the numbering pattern of 5, 6, 4, 5, 6 - starting again at a number lower than the previous and duplicating the number 6 in the list.

**Note:** The `start` and `value` attributes only accept a number - even if the ordered list is set to display as Roman numerals or letters.

<!-- if version [gte 5] -->

You can reverse the numbering by adding `reversed` in your `ol` element:

    <ol reversed>
      <li>Item</li>
      <li>Some Other Item</li>
      <li value="4">A Reset Item</li>
      <li>Another Item</li>
      <li>Yet Another Item</li>
    </ol>

Reverse numbering is helpful if you're continually adding to a list, such as with new podcast episodes or presentations, and you want the most recent items to appear first.

<!-- end version if -->

---- 

### Changing the type of numeral ###

You can easily change the type of numeral shown in the list item marker by using the `type` attribute

    <ol type="1|a|A|i|I">

| Type | Description | Examples
| -----: | ------ | ------
| `1`   | Default value - Decimal numbers   |  1,2,3,4
| `a`   | Alphabetically ordered (lowercase)   | a,b,c,d
| `A`   | Alphabetically ordered (uppercase)   | A,B,C,D
| `i`   | Roman Numerals (lowercase)  | i,ii,iii,iv
| `I`   | Roman Numerals (uppercase)  | I,II,III,IV

> You should use `ol` to display a list of items, where the items have
> been intentionally ordered and order should be emphasized. If changing
> the order of the items does NOT make the list incorrect, you should
> use [`<ul>`][1].


  [1]: https://www.wikiod.com/html/lists#Unordered List

## Unordered List
An unordered list can be created with the `<ul>` tag and each list item can be created with the `<li>` tag as shown by the example below:

    <ul>
      <li>Item</li>
      <li>Another Item</li>
      <li>Yet Another Item</li>
    </ul>

This will produce a bulleted list (which is the default style):

><ul>
>  <li>Item</li>
>  <li>Another Item</li>
>  <li>Yet Another Item</li>
></ul>

> You should use `ul` to display a list of items, where the order of the items is
> not important. If changing the order of the items makes the list
> incorrect, you should use [`<ol>`][1].


  [1]: https://www.wikiod.com/html/lists#Ordered List

## Nested lists
You can nest lists to represent sub-items of a list item.

    <ul>
      <li>item 1</li>
      <li>item 2
        <ul>
          <li>sub-item 2.1</li>
          <li>sub-item 2.2</li>
        </ul>
      </li>
      <li>item 3</li>
    </ul>

><ul>
>  <li>item 1</li>
>  <li>item 2
>    <ul>
>      <li>sub-item 2.1</li>
>      <li>sub-item 2.2</li>
>    </ul>
>  </li>
>  <li>item 3</li>
></ul>

The nested list has to be a child of the `li` element.

You can nest different types of list, too:

    <ol>
        <li>Hello, list!</li>
        <li>
            <ul>
                <li>Hello, nested list!</li>
            </ul>
        </li>
    </ol>

## Description List
A description list (or *definition list*, as it was called before HTML5) can be created with the `dl` element. It consists of name-value groups, where the name is given in the `dt` element, and the value is given in the `dd` element.

<!-- language: lang-html -->

    <dl>
      <dt>name 1</dt>
      <dd>value for 1</dd>
      <dt>name 2</dt>
      <dd>value for 2</dd>
    </dl>

[Live demo][1]

A name-value group can have more than one name and/or more than one value (which represent alternatives):

    <dl>

      <dt>name 1</dt>
      <dt>name 2</dt>
      <dd>value for 1 and 2</dd>

      <dt>name 3</dt>
      <dd>value for 3</dd>
      <dd>value for 3</dd>

    </dl>
[Live demo][2]


  [1]: https://jsfiddle.net/794vj10g/
  [2]: https://jsfiddle.net/vefya4ds/

