---
title: "Colors"
slug: "colors"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## RGB Colors Using Hexadecimal Notation
    <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
      <circle r="30" cx="100" cy="100" fill="#ff0000" stroke="#00ff00" />
      <rect x="200" y="200" width="50" height="50" fill="#ffff00" stroke="#00ffff" />
    </svg>

Same as above using [shorthand hexadecimal form][1]:

    <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
      <circle r="30" cx="100" cy="100" fill="#f00" stroke="#0f0" />
      <rect x="200" y="200" width="50" height="50" fill="#ff0" stroke="#0ff" />
    </svg>

  [1]: https://en.wikipedia.org/wiki/Web_colors#Shorthand_hexadecimal_form

## RGB colors with functional notation - integer values or percentages
    <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
      <circle r="30" cx="100" cy="100" fill="rgb(255, 0, 0)" stroke="rgb(0, 255, 0)" />
      <rect x="200" y="200" width="50" height="50" fill="rgb(100%, 100%, 0%)" stroke="rgb(0%, 100%, 100%)" />
    </svg>

in functional notation, RGBA values are also supported.

    <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
        <circle r="30" cx="100" cy="100" fill="rgba(255, 0, 0, 0.5)" stroke="rgba(0, 255, 0,5 0.5)" />
        <rect x="200" y="200" width="50" height="50" fill="rgba(100%, 100%, 0%, 0.5)" stroke="rgba(0, 100%, 100%, 0.5)" />
    </svg>

## Named colors - use predefined names for fill and stroke attributes
A list of recognised color keyword names can be found in the [W3C Recommendation for SVG][1].


  [1]: https://www.w3.org/TR/SVG/types.html#ColorKeywords

    <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
      <circle r="30" cx="100" cy="100" fill="red" stroke="green" />
      <rect x="200" y="200" width="50" height="50" fill="yellow" stroke="blue" />
    </svg>
    

## The currentColor keyword
`currentColor` is most usefull in inline SVGs. With this you can inherit the parents css color and use it everywhere colors are used in SVG.

In this example the first circle uses the text color as fill color, and the second circle uses it as the stroke color.

    <html>
        <head>
            div{color:green}
        </head>
        <body>
            <div>
                some Text
                <svg width="2em" height="1em" viewBox="0 0 200 100">
                    <circle cx="50" cy="50" r="45" fill="currentColor"/>
                    <circle cx="150" cy="50" r="45" fill="none" stroke-width=5 stroke="currentColor"/>
                </svg>
            </div>
        </body>
    </html>



