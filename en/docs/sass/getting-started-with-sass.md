---
title: "Getting started with sass"
slug: "getting-started-with-sass"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Variables
If you have a value that you use often, you can store it in a variable. You could use this to define color schemes, for example. You would only have to define your scheme once and then you could use it throughout your stylesheets.

To define a variable, you must prefix its name with the $ symbol. (Like you would in PHP.)

You can store any valid CSS property value inside a variable. Such as colors, fonts or URLs.

**Example #1:**


    $foreground: #FAFAFA;
    $background: rgb(0, 0, 0);

    body {
        color: $foreground;
        background-color: $background;
    }

    p {
        color: rgb(25, 25, 20);
        background-color: $background;
    }


## Importing
Let's assume the following scenario: *You have two stylesheets: `_variables.scss` and `layout.scss`. Logically, you keep all your variables inside your variable stylesheet but want to access them from your layout stylesheet.*

> **NOTE:** You may notice that the variables stylesheet has an underscore ('_') before it's name. This is because it's a partial - meaning it's going to be imported.

> **sass-lang.com** says the following about partials: *You can create partial Sass files that contain little snippets of CSS that you can include in other Sass files. This is a great way to modularize your CSS and help keep things easier to maintain. [...] The underscore lets Sass know that the file is only a partial file and that it should not be generated into a CSS file. Sass partials are used with the @import directive.*


SCSS variables are great for this scenario. Let's assume that your `_variables.scss` looks like this:  

```scss
$primary-color: #333;
```
You can import it with `@import` and then the stylesheet's name in quotes.
Your layout stylesheet may now look like this (take note of there not being an underscore or file extension in the import):
```sass
@import 'variables';
body {
  color: $primary-color;
}
```


----------
  
This would output something like the following:

```css
body {
  color: #333;
}
```


## Comments
SASS supports two types of comments:

 - Inline comments - These only span one line and are usually used to describe a variable or block. The syntax is as follows: `// Your comment here` (you prepend it with a double slash (`//`) and the rest of the line is ignored by the parser.

 - Multiline comments - These span multiple lines and are usually used to display a copyright or license at the top of a document. You can open a multiline comment block with `/*` and you can close a multiline comment block with `*/`. Here's an example:
```
/*
   This is a comment
   It's a multiline comment
   Also a hiaku
*/
```

## Nesting
layout.scss

    nav {
        ul {
            margin: 0;
            padding: 0;
            list-style: none;
            li {
                margin: 0 5px;
               }
           }
    }

output

    nav ul {
        margin: 0;
        padding: 0;
        list-style: none;
    }
    nav ul li {
        margin: 0 5px;    
    }

## Setup
When it comes to using SASS, there are multiple ways of setting up your workspace.
Some people prefer to use command line tools (probably Linux people) and others prefer to use GUI applications. I'll cover both.

## Command Line Tools ##
The 'Install SASS' page at `sass-lang.com` covers this quite well.  You can use SASS with Ruby (which can be installed from a Linux package manager or you can [download the installer][1] on Windows).
macOS comes with Ruby pre-installed.

Once you've installed Ruby, you need to install SASS (in some cases, `sudo` may not be needed):
```
sudo gem install sass
```

Finally, you can check you've installed SASS with `sass -v`.

## GUI Applications ##
Whilst there are a number of GUI Applications that you can use, I recommend [Scout-App][2]. It auto-builds and compresses your CSS files for you, on file save and supports macOS, Windows and Linux.


  [1]: http://rubyinstaller.org/
  [2]: http://scout-app.io/

