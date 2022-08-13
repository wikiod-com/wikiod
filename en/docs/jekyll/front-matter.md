---
title: "Front Matter"
slug: "front-matter"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Front Matter tells Jekyll to parse the page. It can contain properties for the page.

## Basic Front Matter in Jekyll
Front matter tells Jekyll to parse a file. You add predefined variables, which are YAML sets, to the front matter. Then, you can use Liquid tags in your files to access the front matter.

Front matter is indicated with two triple-dashed lines. You must place the variables between the two triple-dashed lines, and you must place front matter at the top of the file.

For example, the front matter for two posts in a blog about learning different musical instruments might look like this:
````
---
layout: post
title: "Learning to Play the Violin by Self-Study"
date: 2016-07-25
tags: [violin, self-study, beginner]
---
````

and
````
---
layout: post
title: "Taking Lessons on the Violin as a Beginner"
date: 2016-07-25
tags: [violin, lessons, beginner]
---
````

For more information about front matter, please see [Jekyll Tips: Front Matter](https://jekyllrb.com/docs/frontmatter/).

## Predefined Variables
There are a number of predefined global variables that you can set in the front matter of a page or post.

| Variable | Description|
| ------   | ------     |
| `layout`     | If set, this specifies the layout file to use. Use the layout file name without the file extension. Layout files must be placed in the  `_layouts` directory.|
| `permalink`|If you need your processed blog post URLs to be something other than the site-wide style (default `/year/month/day/title.html`), then you can set this variable and it will be used as the final URL.|
|`published`|Set to false if you don’t want a specific post to show up when the site is generated.|

There are also predefined variables specifically for posts.

| Variable | Description|
| ------   | ------     |
| `date`     |A date here overrides the date from the name of the post. This can be used to ensure correct sorting of posts. A date is specified in the format `YYYY-MM-DD HH:MM:SS +/-TTTT`; hours, minutes, seconds, and timezone offset are optional. |
| `category` `categories`|Instead of placing posts inside of folders, you can specify one or more categories that the post belongs to. When the site is generated the post will act as though it had been set with these categories normally. Categories (plural key) can be specified as a YAML list or a comma-separated string.|
|`tags`|Similar to categories, one or multiple tags can be added to a post. Also like categories, tags can be specified as a YAML list or a comma-separated string.
|

## Using Custom Variables
You can also put custom variables in the front matter. These can be reused in the page layout.

For example, if your front matter looks like this:
```
---
layout: post
title: "Using Custom Variables!"
date: 2016-07-25
chicken: "I like Chicken."
---
```
You can use the `chicken` variable in the post layout like this:
```
<!DOCTYPE HTML>
<html>
  <head>
    <title>{{ post.chicken }}</title>
  </head>
  <body>
    ...
```
The `chicken` variable will be substituted in instead of `{{ post.chicken }}`


