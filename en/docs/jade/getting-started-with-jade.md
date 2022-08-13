---
title: "Getting started with jade"
slug: "getting-started-with-jade"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Syntax
Pug (old name is Jade) is a clean, whitespace sensitive syntax for writing HTML. Here is a simple example:

    doctype html
    html(lang="en")
      head
        title= pageTitle
        script(type='text/javascript').
          if (foo) bar(1 + 5)
      body
        h1 Pug - node template engine
        #container.col
          if youAreUsingPug
            p You are amazing
          else
            p Get on it!
          p.
            Pug is a terse and simple templating language with a
            strong focus on performance and powerful features.

Produces following output as HTML

    <!DOCTYPE html>
    <html lang="en">
      <head>
        <title>Pug</title>
        <script type="text/javascript">
          if (foo) bar(1 + 5)
        </script>
      </head>
      <body>
        <h1>Pug - node template engine</h1>
        <div id="container" class="col">
          <p>You are amazing</p>
          <p>Pug is a terse and simple templating language with a strong focus on performance and powerful features.</p>
        </div>
      </body>
    </html>

Here are the rules to render Pug to HTML code:

1. By indenting the text, the HTML tree will be build. indenting could be used with spaces or tabs. This could not be mixed!
2. HTML tags are written without `<` and `>`. Attributes are places between round brackets.
3. Comment could be made with `//` or `<!-- -->`. Comments with `//-` are not visible in the rendered HTML.
4. With `#{ }` will an offered model generated: `#{header} #{user.username}`. 
5. The `#` (hashtag) without braces will a `div` element created with the text as ID. Example `#myID` will be rendered as `<div id="myID"></div>`. 
6. With a `.` _(point)_ will a `div` generated with a class attribute. Example: `.myClass` will be rendered as `<div class="myClass"></div>`
7. With <code>=&nbsp;</code> (equality sign followed by a space), a variable will be retrieved. Exaple: `h1= title`
8. A `!=` (not equal to) retrieved a variable without escaping.
9. A `-` (hyphen) allows you to write JavaScript. Example: `- console.log("foo");`
10. Linking to an external file can as follow: `script(src="/js/chat.js")`
11. Inline script could by using this `script.`.
12. A directive for adding the basic layout: `extends ../layout`.
13. At `layout.pug` happens the inserting by using `block content`
14. Use of partials could on two ways:
    1. by partial: `!= partial(template file name/options)`.
    2. By include: `include ../includes/footer`
15. The inverse of include is `extend`. This allows from a page "html `block` parts" to send to a layout page for example: `extend layout`
16. Concatenating happens with the `+` (plus) or `#` (hashtag) char. The plus is used at JavaScript code. The hashtag in HTML and renders the content: `p The name is: #{myName}





















## Using pug with Node.js
    var pug = require('pug');
    
    // compile
    var fn = pug.compile('string of pug', options);
    var html = fn(locals);
    
    // render
    var html = pug.render('string of pug', merge(options, locals));
    
    // renderFile
    var html = pug.renderFile('filename.pug', merge(options, locals));

**Options**

 - <i>filename</i> Used in exceptions, and required when using includes
 - <i>compileDebug</i> When false no debug instrumentation is compiled 
 - <i>pretty</i> Add pretty-indentation whitespace to output (false by default)

## Installation or Setup
Before to launch you to code with Pug, you need to have some prerequisits.
 
 You will need to install:
 * [NodeJS][1] with NPM
 * [ExpressJS][2] (optional)
 
 After installing NodeJS, you can check in your terminal the correct installation doing:
 
 ```
 $ node -v
 ```
 If successful, it will print the number of Node's version.
 
 To install Pug into your project, the preferred and easy way is through NPM (Node Package Manager). If you are familiar with that, simply execute this line of code in your Terminal:
 
 ```
 $ npm install pug
 ```
 If you want to install globally, you can type:
 
 ```
 $ npm install pug-cli -g
 ```
and run with

    $ pug --help

  [1]: https://nodejs.org/
  [2]: http://expressjs.com/

