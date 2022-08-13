---
title: "Getting started with lodash"
slug: "getting-started-with-lodash"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Setup
Lodash works equally well on both servers (like node.js) and browsers.

# node.js with npm

Download with npm from the CLI:

    npm install lodash

Then in your node scripts:

    var _ = require("lodash");
    
    // use lodash in your program...



# Download own copy for clientside in website (ie. in the browser)

1. [Download lodash][1] or use a package manager like npm, jspm or bower.
2. Include the script reference in your page with `<script src="lodash.js"></script>`. (Fix the path if it's not in same directory as the webpage.)

# Use lodash in a browser from a CDN

Go to [a CDN site][2] and select the version you want to use. Copy the link and use it for the script reference in your page.

    <script src="https://cdn.jsdelivr.net/lodash/4.13.1/lodash.min.js"></script>
*4.13.1 is current version at time of writing*


  [1]: https://lodash.com
  [2]: https://www.jsdelivr.com/projects/lodash

