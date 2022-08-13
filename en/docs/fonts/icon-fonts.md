---
title: "Icon fonts"
slug: "icon-fonts"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Including an icon font on a web page
To include an icon font in your CSS, you can include the following code :

    @font-face {
        font-family: 'myfont';
        src:url('fonts/myfont.eot?-td2xif');
        src:url('fonts/myfont.eot?#iefix-td2xif') format('embedded-opentype'),
            url('fonts/myfont.woff?-td2xif') format('woff'),
            url('fonts/myfont.ttf?-td2xif') format('truetype'),
            url('fonts/myfont.svg?-td2xif#myfont') format('svg');
        // Different URLs are required for optimal browser support
        // Make sure to :
        // 1) replace the URLs with your font's URLs
        // 2) replace `#myfont` with the name of your font
        font-weight: normal; // To avoid the font inherits boldness
        font-style: normal; // To avoid font inherits obliqueness or italic
    }

    .icon {
        font-family: 'myfont', Verdana, Arial, sans-serif; // Use regular fonts as fallback
        speak: none; // To avoid screen readers trying to read the content
        font-style: normal; // To avoid font inherits obliqueness or italic
        font-weight: normal; // To avoid the font inherits boldness
        font-variant: normal; // To avoid the font inherits small-caps
        text-transform: none; // To avoid the font inherits capitalization/uppercase/lowercase
        line-height: 1; // To avoid the font inherits an undesired line-height
        -webkit-font-smoothing: antialiased; // For improved readability on Webkit
        -moz-osx-font-smoothing: grayscale; // For improved readability on OSX + Mozilla
    }

## Using an icon font on a web page
To use an icon in your HTML, you can do each of the following :

    <!-- Method 1 -->
    <!--- * * * * * * * * * * * * -->
    <!-- Set a font-family for an entire HTML element -->
    <!-- Define your icon fonts in your CSS font-family after your regular fonts  -->
    <!-- This means that regular characters are default. Icons are a fallback  -->
    <!-- Use UTF-8 characters directly in your HTML for improved human readability -->
    <div class="rate"><p>I rate this movie ★★★★☆!!</p></div>
    
    <!-- Method 2 -->
    <!--- * * * * * * * * * * * * -->
    <!-- Set a font-family for an entire HTML element -->
    <!-- Define your icon fonts in your CSS font-family after your regular fonts  -->
    <!-- This means that regular characters are default. Icons are a fallback  -->
    <!-- Use entity codes in your HTML when UTF-8 support is uncertain -->
    <div class="rate"><p>I rate this movie &#9733;&#9733;&#9733;&#9733;&#9734;!!</p></div>
    
    <!-- Method 3 -->
    <!--- * * * * * * * * * * * * -->
    <!-- Set a font-family only for the icons but not the HTML elements that include them -->
    <!-- Define your icon fonts in your CSS font-family before your regular fonts  -->
    <!-- This means that icons are default. Regular characters are a fallback  -->
    <!-- Use UTF-8 characters directly in your HTML for improved human readability -->
    <p>I rate this movie <span class="icon">★★★★☆</span>!!</p>
    
    <!-- Method 4 -->
    <!--- * * * * * * * * * * * * -->
    <!-- Set a font-family only for the icons but not the HTML elements that include them -->
    <!-- Define your icon fonts in your CSS font-family before your regular fonts  -->
    <!-- This means that icons are default. Regular characters are a fallback  -->
    <!-- Use entity codes in your HTML when UTF-8 support is uncertain -->
    <p>I rate this movie <span class="icon">&#9733;&#9733;&#9733;&#9733;&#9734;</span>!!</p>
    
    <!-- Method 5 -->
    <!--- * * * * * * * * * * * * -->
    <!-- Set a font-family only for the icons and use a separate HTML tag for each icon -->
    <!-- Define your icon fonts in your CSS font-family before your regular fonts  -->
    <!-- This means that icons are default. Regular characters are a fallback  -->
    <!-- Use UTF-8 characters directly in your HTML for improved human readability -->
    <p>I rate this movie
        <span class="icon">★</span>
        <span class="icon">★</span>
        <span class="icon">★</span>
        <span class="icon">★</span>
        <span class="icon">☆</span>
        !!
    </p>
    
    <!-- Method 6 -->
    <!--- * * * * * * * * * * * * -->
    <!-- Set a font-family only for the icons and use a separate HTML tag for each icon -->
    <!-- Define your icon fonts in your CSS font-family before your regular fonts  -->
    <!-- This means that icons are default. Regular characters are a fallback  -->
    <!-- Use entity codes in your HTML when UTF-8 support is uncertain -->
    <p>I rate this movie
        <span class="icon">&#9733;</span>
        <span class="icon">&#9733;</span>
        <span class="icon">&#9733;</span>
        <span class="icon">&#9733;</span>
        <span class="icon">&#9734;</span>
        !!
    </p>
    
    <!-- Method 7-->
    <!--- * * * * * * * * * * * * -->
    <!-- Set a font-family only for the icons and use a separate HTML tag for each icon -->
    <!-- Define your icon fonts in your CSS font-family before your regular fonts  -->
    <!-- This means that icons are default. Regular characters are a fallback  -->
    <!-- Use the 'content' style rule with a ':before selector' in your CSS -->
    <p>I rate this movie
        <span class="icon icon-star"></span>
        <span class="icon icon-star"></span>
        <span class="icon icon-star"></span>
        <span class="icon icon-star"></span>
        <span class="icon icon-star-unfilled"></span>
        !!
    </p>

If you want to opt for method 7, you'll need some additional CSS code. This CSS code would look like this :

    .icon-star:before {
        content: "\2605";
    }

    .icon-star-unfilled:before {
        content: "\2606";
    }

Icon fonts like **[Iconic][3]**, **[Font Awesome][4]** or **[Glyphicons][5]** typically all use method 7. This, to avoid you having to copy-paste special characters from a cheat sheet or being forced to use HTML entities.

It is, however, a method that has several downsides. First of all, it requires support for the `:before` CSS selector and the use of an escape sequence for UNICODE characters. Neither IE6-7 nor [**certain versions of Webkit**][6] provide this support.

Another downside is that you have to use a seperate HTML tag for each icon, with each tag corresponding to one character from the icon font. Displaying several icons within HTML tag is not possible with method 7, unlike with other methods.

Other methods have their own downsides, though. Methods 1, 3 and 5 require you to copy-paste the character from a cheat sheet or use means to put the character itself within your code. Your code editor may not be capable of displaying the character or it may display a different character from the one in your icon font if the icon font uses a non-standard mapping that character.

Methods 1, 3 and 5 also require that your browser uses the proper encoding to display the correct character. For UNICODE characters, this isn't as obvious as it is for ASCII characters. This should, however, be ensured by adding the meta-tag `<meta charset="utf-8" />` somewhere in the `head` of your HTML-document.

Methods 2, 4 and 6 do not require you to copy-paste the character, however it makes your code less readable by humans and makes any changes to the code more prone to human error. Also, as you will need to look up the HTML-entity code for each of the icons you want to use or you'll need to memorize them. While the same obviously applies to the classes used in method 7 as well, those classes are much easier to memorize than an HTML entity code.

  [1]: https://icomoon.io/app/#/select
  [2]: https://github.com/jslegers/emoji-icon-font
  [3]: https://useiconic.com/
  [4]: http://fortawesome.github.io/Font-Awesome/
  [5]: http://glyphicons.com/
  [6]: http://stackoverflow.com/questions/9241519/webkit-css-content-unicode-bug
  [7]: http://i.stack.imgur.com/RkgYC.png

## Including a specific symbol on a web page
Consider a down-pointing triangle.

There are several correct ways to display this symbol on a web page.

# Method 1 : use decimal HTML entity

HTML :

    &#9660;

-----------

# Method 2 : use hexidecimal HTML entity

HTML :

    &#x25BC;

-----------

# Method 3 : use character directly

HTML :

    ▼

-----------

# Method 4 : use CSS

HTML :

    <span class='icon-down'></span>

CSS :

    .icon-down:before {
        content: "\25BC";
    }

-----------

Each of these three methods should have the same output. For other symbols, the same three options exist. Some even have a fourth option, allowing you to use a string based reference (eg. `&hearts;` to display ♥).

You can use a reference website like **[Unicode-table.com][1]** to find which icons are supported in UNICODE and which codes they correspond with. For example, you find the values for the down-pointing triangle at **http://unicode-table.com/en/25BC/**.


Note that these methods are sufficient only for icons that are available by default in every browser. For symbols like ☃,❄,★,☂,☭,⎗ or ⎘, this is far less likely to be the case. While it is possible to provide cross-browser support for other UNICODE symbols, you'll need to obtain an icon font or create one of your own. See **https://www.wikiod.com/fonts/creating-your-own-font for more info on how to do create your own font.

  [1]: http://unicode-table.com/en/
  [2]: http://i.stack.imgur.com/5wLDq.gif

## Popular icon fonts
The following is a list of popular icon fonts:

- [Font Awesome][1]
- [Fontello][2]
- [Modern Pictograms][3]
- [Typicons][4]
- [Foundation Icon Fonts][5]
- [Pictonic][6]
- [Pictos][7]


  [1]: http://fontawesome.io/
  [2]: http://fontello.com/
  [3]: http://thedesignoffice.org/project/modern-pictograms
  [4]: http://typicons.com/
  [5]: http://zurb.com/playground/foundation-icons
  [6]: https://pictonic.co/
  [7]: http://pictos.cc/classic/font

