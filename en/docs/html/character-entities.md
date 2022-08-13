---
title: "Character Entities"
slug: "character-entities"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Common Special Characters
Some character may be reserved for HTML and cannot be used directly as it may obstruct the actual HTML codes. For example, trying to display the left and right angle brackets (`<>`) in the source code may cause unexpected results in the output. Similarly, white spaces as written in the source code may not display as expected in the output HTML. Some, like &phone;, are not available in the ASCII character set.

For this purpose, character entities are created. These are of the form `&entity_name;` or `&entity_number;`. The following are some of the available HTML entities.

| Character| Description          | Entity Name | Entity Number |
| -------- | -------------------- | ----------- | ------------- |
| “&nbsp;”   | non-breaking space   | `&nbsp;`  | `&#160;`    |
| “&lt;”     | less than            | `&lt;`    | `&#60;`     |
| “&gt;”     | greater than         | `&gt;`    | `&#62;`     |
| “&amp;”    | ampersand            | `&amp;`   | `&#38;`     |
| “&mdash;”  | em dash              | `&mdash;` | `&#8212;`   |
| “&ndash;”  | en dash              | `&ndash;` | `&#8211;`   |
| “&copy;”   | copyright            | `&copy;`  | `&#169;`    |
| “&reg;”    | registered trademark | `&reg;`   | `&#174;`    |
| “&trade;”  | trademark            | `&trade;` | `&#8482;`   |
| “&phone;”  | phone                | `&phone;` | `&#9742;`   |

Thus, to write

**&copy; 2016 Stack Exchange Inc.**

the following HTML code is used:

    <b>&copy; 2016 Stack Exchange Inc.</b>

## Character Entities in HTML
Many symbols and special characters are required while developing a web page in html, but as we know that sometimes the use of characters directly may interfere with the actual html code which have certain characters reserved and also certain characters being not available on keyboard. Thus, to avoid the conflict and at same time to be able to use different symbols in our code w3 org provides us with 'Character Entities'.

Character Entities are predefined with 'Entity Name' - &entity_name; and 'Entity Number' - &entity_number;  so we need to use either of the two for the required symbol to be rendered on our page. 

The list of few Character Entities can be found at https://dev.w3.org/html5/html-author/charref

A simple example with the use of character entity for 'magnifying glass' :

    <input type="text" placeholder="  &#128269; Search"/>

which renders as 
  
[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/MUEkn.png

