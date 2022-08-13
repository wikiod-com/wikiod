---
title: "SUPER-Optimising for production"
slug: "super-optimising-for-production"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Once your project is done, you are left to wonder how will you upload those load of a 100 HTML imports on your web server and even if you do that, how much hours your site is going  to take to load for a single client. In this topic, you'll see how to convert the development mess into refined single html and js files.

## Syntax
 - npm install: saves packages using Node.js Package Manager
 - npm install -g: Saves packages from npm as global (Useful for Command Line interface packages)
 - cd: Sets the focus of Command Line to a specific library in which processes can be done
 - vulcanize: Crunches down all the HTML imports to a single file
 - Crisper: Converts Inline JS in HTML file to external JS file


## Installing all the required tools
Run the following command one by one:

    npm install -g vulcanize crisper
## Use ##

 - Vulcanize: Crunches all the HTML import files into a single file
 - Crisper: Extracts inline js to its own file
    
**Note**: Ubuntu users may need to prefix the above command with `sudo`.

## Putting it all together
Put all the html imports in your files in a single file `elements.html`. Don't worry about a file being imported more than once, it'll be crunched down to a single import. <br>




## Running vulcanize and crisper
on your `elements.html` file, run the following commands:

    cd PATH/TO/IMPORTFILE/
    vulcanize elements.html -o elements.vulc.html --strip-comments --inline-css --inline-js
    crisper --source elements.vulc.html --html build.html --js build.js
Vulcanize retrieved source code of all the imports, then replaced imports by their source code.
<br>
Crisper took all the js out of the elements.vulc.html file, put it in single build.js file, set a `script` tag referring the `build.js` file in the `build.html` file

## Minifying the files
 - Open [HTML minifier](https://kangax.github.io/html-minifier/)

 - Open `build.html`
 - Copy all its code
 - In the HTML minfier's first textarea, paste the code you copied from `build.html`
 - Click **Minify** button
 - In the second textarea, minified code will appear. Copy that
 - Create a `build.min.html` file and paste all your copied code in it
 - Open [JSCompress](https://jscompress.com/)
 - Select 2nd tab i.e. **Upload JavaScript Files**
 - Click on `Choose Files`
 - Select `build.js` file
 - Click on *Compress* button
 - Download the file as `build.js` in the same directory as of build.min.html

## importing build.min.html
Remove all previous imports from those HTmL files from which you copy-pasted the imports. Replace imports with 

    <link rel="import" href="PATH/TO/IMPORTFILE/build.min.html">

