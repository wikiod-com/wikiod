---
title: "Webfonts"
slug: "webfonts"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Including a webfont on a web page
To include a webfont in your CSS, you can include the following code:

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

    .usesthewebfont {
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

