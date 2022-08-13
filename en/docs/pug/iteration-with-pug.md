---
title: "Iteration with Pug"
slug: "iteration-with-pug"
draft: false
images: []
weight: 9949
type: docs
toc: true
---

How to iterate over a simple JSON object and save a lot of typing

You need to have Node.js and Pug installed

## Each iteration
Build an `app.js` with a simple *data store*:

    app.get("/bookstore", function (req, res, next) {
        // Your route data
       var bookStore = [
            {
                title: "Templating with Pug",
                author: "Winston Smith",
                pages: 143,
                year: 2017        
            },
            {
                title: "Node.js will help",
                author: "Guy Fake",
                pages: 879,
                year: 2015        
            }
        ];
        res.render("index", {
            bookStore: bookStore
        });
    });

Iterate over the data store using an `index.pug` file and an each loop:

    each book in bookStore
      ul
        li= book.title
        li= book.author
        li= book.pages
        li= book.year

Result will be:

    
    <ul>
      <li>Templating with Pug</li>
      <li>Winston Smith</li>
      <li>143</li>
      <li>2017</li>
    </ul>
    <ul>
      <li>Node.js will help</li>
      <li>Guy Fake</li>
      <li>879</li>
      <li>2015</li>
    </ul>

[Reference](https://pugjs.org/language/iteration.html)

