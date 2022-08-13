---
title: "Getting started with font-awesome"
slug: "getting-started-with-font-awesome"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Basic example: using an icon
The simple use-case is to refer to a single icon in its normal size:
`<i class="fa fa-camera-retro"></i>` (View result in [this][1] fiddle.)

Create an empty tag (it is recommended to use `<i>` used for that) and assign class "`fa`" and the class corresponding to the desired icon (see list of icons [here][2]).


  [1]: https://jsfiddle.net/oxnwjd3z/
  [2]: http://fontawesome.io/icons/

## Setup
Different approaches can be used to integrate FontAwesome into a website:

For plain HTML/CSS:

* Download the zip available [here][1], unzip, and copy the contents to your website. Then reference the `/css/font-awesome.css` in the webpage `head` like so:<br /><br />`<link rel="stylesheet" src="/assets/font-awesome/css/font-awesome.css">`
* Reference it in the webpage `head` using a CDN address. One such address would be `https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css`. To reference this, just use a `link` tag but set the source to the CDN address instead

For Package managers:

Most package managers support Font Awesome as a package, and there are some examples below:

* NuGet: Just search for and install `Font-Awesome`, or run the below command in the package console:
`Install-Package FontAwesome`
* Ruby: Just run this command: `gem install font-awesome-rails`
* npm: just run this command: `npm install font-awesome`


  [1]: https://github.com/FortAwesome/Font-Awesome/archive/master.zip

## Using Font Awesome
Font Awesome is an extremely simple yet powerful library to use, with 634 icons available in just a few words.

How does it work?
Font Awesome uses Unicode characters stored in a `../fonts` directory to change any `i.fa` elements to the respective unicode character, as such displaying the icon as text.

How do I create an icon?
All icon classes have to be an `i` element, or an `italic element`, mainly for best practice, but also improves performance with Font Awesome. All icons also have the class `fa` on them. This denotes an icon and <b>will not work without it</b>. After that, just add the icon you want, prefixed with another `fa-`. A finished example is below:

`<i class="fa fa-pencil"></i>` becomes [![fa-pencil][1]][1]

Because font awesome works off unicode characters, it also allows any text manipulation to apply to it as well, such as `font-size`, `color`, and more.


  [1]: http://i.stack.imgur.com/08OV3.jpg

