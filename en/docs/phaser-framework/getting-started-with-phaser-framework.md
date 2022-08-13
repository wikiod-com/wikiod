---
title: "Getting started with phaser-framework"
slug: "getting-started-with-phaser-framework"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting Started Phaser
 1. Create a folder
 2. Create an *index.html* inside the new directory. Open it in the Bracket editor
 3. Download the Phaser repository from [github][1], then grab the *phaser.js* file from the build folder. Place the file inside your project directory.
 4. Open *index.html* and link the **phaser.js** inside the header tag.


    <!doctype html> 
    <html lang="en"> 
    <head> 
        <meta charset="UTF-8" />
        <title>My Gamer</title>    
        
         <script type="text/javascript" src="lib/phaser.js"></script>

        <style type="text/css">
            body {
                margin: 0;
            }
        </style>
        
    </head>
    <body>    
            <div id="gameContainer"></div>
    </body>
    </html>

 5. Create another js file inside the directory named *game.js*
 6. Open *game.js* file in editor and write the following code:


    // Phaser instance, width 800px, height 600px render as CANVAS. 
    // Method signature - preload, create and update

    var game = new Phaser.Game(800, 600, Phaser.CANVAS,'gameContainer', { preload: preload, create: create, update: update });
    
    function preload() {
    // this method used to load your game assets
    }
    
    function create() {
    // this method run only once used to create to game world
    }
    
    function update() {
    // this method loop 60 times in a seconds, used to handle gameplay.
    }


 8. Save all files and open *index.html* using Bracket liveserver (top right icon).
 9. The Phaser development environment is now created. A console screen should appear in the browser for error verification.

  [1]: https://github.com/photonstorm/phaser

## Getting Started with Phaser using Node.js

1. Create a folder where you would like to have your game live, and move into that 

<pre>
mkdir my-new-game
cd my-new-game
</pre>

2. Initialize the directory using npm.

<pre>
npm init -y
</pre>

3. Install phaser as a node package.

<pre>npm install phaser</pre>

4. Install http-server as a global module, to be used on the commandline.

<pre>npm install -g http-server</pre>

5. Create an index.html file and reference the Phaser executable and paste the following code into it. 

<pre>&lt;!doctype html&gt;
&lt;html lang="en"&gt;

&lt;head&gt;
    &lt;meta charset="UTF-8" /&gt;
    &lt;title&gt;My Gamer&lt;/title&gt;
    &lt;script type="text/javascript" src="node_modules/phaser/build/phaser.js"&gt;&lt;/script&gt;
    &lt;style type="text/css"&gt;
    body {
        margin: 0;
    }
    &lt;/style&gt;
&lt;/head&gt;

&lt;body&gt;
    &lt;div id="helloWorld"&gt;&lt;/div&gt;
&lt;/body&gt;
&lt;script&gt;
var game = new Phaser.Game(800, 600, Phaser.CANVAS, 'helloWorld', {
    create: create
});

function create() {

    var text = "Hello World!";
    var style = {
        font: "65px Arial",
        fill: "#ff0044",
        align: "center"
    };

    var t = game.add.text(game.world.centerX, 300, text, style);
    t.anchor.set(0.5);

}
&lt;/script&gt;

&lt;/html&gt;
</pre>

6. Start the server and load http://localhost:8080 in your browser!

<pre>    hs    </pre>

