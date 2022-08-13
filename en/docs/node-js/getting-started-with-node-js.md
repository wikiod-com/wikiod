---
title: "Getting started with Node.js"
slug: "getting-started-with-nodejs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World HTTP server
First, [install Node.js](https://www.wikiod.com/node-js/installing-nodejs) for your platform.

In this example we'll create an HTTP server listening on port 1337, which sends `Hello, World!` to the browser. Note that, instead of using port 1337, you can use any port number of your choice which is currently not in use by any other service.

The `http` module is a Node.js [**_core module_**](https://www.wikiod.com/node-js/getting-started-with-nodejs#Core modules) (a module included in Node.js's source, that does not require installing additional resources). The `http` module provides the functionality to create an HTTP server using the [`http.createServer()`](https://nodejs.org/api/http.html#http_http_createserver_requestlistener) method.
    To create the application, create a file containing the
following JavaScript code.

<!-- language: lang-js -->
    const http = require('http'); // Loads the http module

    http.createServer((request, response) => {

        // 1. Tell the browser everything is OK (Status code 200), and the data is in plain text
        response.writeHead(200, {
            'Content-Type': 'text/plain'
        });

        // 2. Write the announced text to the body of the page
        response.write('Hello, World!\n');

        // 3. Tell the server that all of the response headers and body have been sent
        response.end();

    }).listen(1337); // 4. Tells the server what port to be on

Save the file with any file name. In this case, if we name it `hello.js` we can run the application by going to the directory the file is in and using the following command:

    node hello.js

The created server can then be accessed with the URL http://localhost:1337
or http://127.0.0.1:1337 in the browser.

A simple web page will appear with a “Hello, World!” text at the top, as shown in the screenshot below.

[![Screenshot][1]][1]

[Editable online example.][2]


  [1]: http://i.stack.imgur.com/Oq3Y4.png
  [2]: https://glitch.com/edit/#!/node-hello-world

## Hello World command line
Node.js can also be used to create command line utilities. The example below reads the first argument from the command line and prints a Hello message.

To run this code on an Unix System:

1. Create a new file and paste the code below. The filename is irrelevant.
2. Make this file executable with `chmod 700 FILE_NAME`
3. Run the app with `./APP_NAME David`

On Windows you do step 1 and run it with `node APP_NAME David`

<!-- Javascript -->

    #!/usr/bin/env node
    
    'use strict';
    
    /*
        The command line arguments are stored in the `process.argv` array, 
        which has the following structure:
        [0] The path of the executable that started the Node.js process
        [1] The path to this application
        [2-n] the command line arguments
    
        Example: [ '/bin/node', '/path/to/yourscript', 'arg1', 'arg2', ... ]
        src: https://nodejs.org/api/process.html#process_process_argv
     */
    
    // Store the first argument as username.
    var username = process.argv[2];
    
    // Check if the username hasn't been provided.
    if (!username) {

        // Extract the filename
        var appName = process.argv[1].split(require('path').sep).pop();

        //  Give the user an example on how to use the app.
        console.error('Missing argument! Example: %s YOUR_NAME', appName);
    
        // Exit the app (success: 0, error: 1). 
        // An error will stop the execution chain. For example:
        //   ./app.js && ls       -> won't execute ls
        //   ./app.js David && ls -> will execute ls
        process.exit(1);
    }
    
    // Print the message to the console.
    console.log('Hello %s!', username);

## Hello World with Express
The following example uses Express to create an HTTP server listening on port 3000, which responds with "Hello, World!". Express is a commonly-used web framework that is useful for creating HTTP APIs.

First, create a new folder, e.g. `myApp`. Go into `myApp` and make a new JavaScript file containing the following code (let's name it `hello.js` for example). Then install the express module using `npm install --save express` from the command line. *Refer to [this documentation][1] for more information on how to install packages*.

    // Import the top-level function of express
    const express = require('express');

    // Creates an Express application using the top-level function
    const app = express();

    // Define port number as 3000
    const port = 3000;

    // Routes HTTP GET requests to the specified path "/" with the specified callback function
    app.get('/', function(request, response) {
      response.send('Hello, World!');
    });

    // Make the app listen on port 3000
    app.listen(port, function() {
      console.log('Server listening on http://localhost:' + port);
    });

---
From the command line, run the following command:

    node hello.js

Open your browser and navigate to `http://localhost:3000` or `http://127.0.0.1:3000` to see the response.

For more information about the Express framework, you can check the [Web Apps With Express][2] section


  [1]: https://www.wikiod.com/node-js/npm#Installing packages
  [2]: https://www.wikiod.com/node-js/web-apps-with-express

## Installing and Running Node.js
To begin, install Node.js on your development computer.

**Windows:** Navigate to the [download page][1] and download/run the installer.

**Mac:** Navigate to the [download page][1] and download/run the installer. Alternatively, you can install Node via Homebrew using `brew install node`. Homebrew is a command-line package mananger for Macintosh, and more information about it can be found on the [Homebrew website][2].

**Linux:** Follow the instructions for your distro on the [command line installation page][3].

-----
## Running a Node Program

To run a Node.js program, simply run `node app.js` or `nodejs app.js`, where `app.js` is the filename of your node app source code. You do not need to include the `.js` suffix for Node to find the script you'd like to run.

Alternatively under UNIX-based operating systems, a Node program may be executed as a terminal script. To do so, it needs to begin with a shebang pointing to the Node interpreter, such as `#!/usr/bin/env node`. The file also has to be set as executable, which can be done using `chmod`. Now the script can be directly run from the command line.


  [1]: https://nodejs.org/en/download/
  [2]: http://brew.sh/
  [3]: https://nodejs.org/en/download/package-manager/

## Debugging Your NodeJS Application
You can use the node-inspector. Run this command to install it via npm:

    npm install -g node-inspector

Then you can debug your application using
    
    node-debug app.js

The Github repository can be found here: https://github.com/node-inspector/node-inspector

-----

# Debugging natively

You can also debug node.js natively by starting it like this:

<!-- language: lang-js -->

    node debug your-script.js

To breakpoint your debugger exactly in a code line you want, use this:

<!-- language: lang-js -->

    debugger;

For more information see [here](https://nodejs.org/api/debugger.html).

In node.js 8 use the following command:

    node --inspect-brk your-script.js

Then open `about://inspect` in a recent version of Google Chrome and select your Node script to get the debugging experience of Chrome's DevTools.

## Hello World basic routing
Once you understand how to create an [HTTP Server][1] with node, it's important to understand how to make it "do" things based on the path that a user has navigated to. This phenomenon is called, "routing".

The most basic example of this would be to check `if (request.url === 'some/path/here')`, and then call a function that responds with a new file.

An example of this can be seen here:

    const http = require('http');

    function index (request, response) {
        response.writeHead(200);
        response.end('Hello, World!');
    }

    http.createServer(function (request, response) {
        
        if (request.url === '/') {
            return index(request, response);
        }

        response.writeHead(404);
        response.end(http.STATUS_CODES[404]);

    }).listen(1337);

If you continue to define your "routes" like this, though, you'll end up with one massive callback function, and we don't want a giant mess like that, so let's see if we can clean this up.

First, let's store all of our routes in an object:

    var routes = {
        '/': function index (request, response) {
            response.writeHead(200);
            response.end('Hello, World!');
        },
        '/foo': function foo (request, response) {
            response.writeHead(200);
            response.end('You are now viewing "foo"');
        }
    }

Now that we've stored 2 routes in an object, we can now check for them in our main callback:

    http.createServer(function (request, response) {
        
        if (request.url in routes) {
            return routes[request.url](request, response);
        }

        response.writeHead(404);
        response.end(http.STATUS_CODES[404]);

    }).listen(1337);

Now every time you try to navigate your website, it will check for the existence of that path in your routes, and it will call the respective function. If no route is found, the server will respond with a 404 (Not Found).

And there you have it--routing with the HTTP Server API is very simple.

  [1]: https://www.wikiod.com/node-js/getting-started-with-nodejs#Hello World HTTP server

## Hello World in the REPL
When called without arguments, Node.js starts a REPL (Read-Eval-Print-Loop) also known as the “*Node shell*”.

At a command prompt type `node`.

    $ node
    >

At the Node shell prompt `>` type "Hello World!"

    $ node
    > "Hello World!"
    'Hello World!'

## Deploying your application online
When you deploy your app to a (Node.js-specific) hosted environment, this environment usually offers a `PORT`-environment variable that you can use to run your server on. Changing the port number to `process.env.PORT` allows you to access the application.

For example, 

    http.createServer(function(request, response) {
       // your server code
    }).listen(process.env.PORT);

Also, if you would like to access this offline while debugging, you can use this:

    http.createServer(function(request, response) {
      // your server code
    }).listen(process.env.PORT || 3000);

where `3000` is the offline port number. 

## Core modules
Node.js is a Javascript engine (Google's V8 engine for Chrome, written in C++) that allows to run Javascript outside the browser. While numerous libraries are available for extending Node's functionalities, the engine comes with a set of _core modules_ implementing basic functionalities.

There's currently 34 core modules included in Node:

    [ 'assert',
      'buffer',
      'c/c++_addons',
      'child_process',
      'cluster',
      'console',
      'crypto',
      'deprecated_apis',
      'dns',
      'domain',
      'Events',
      'fs',
      'http',
      'https',
      'module',
      'net',
      'os',
      'path',
      'punycode',
      'querystring',
      'readline',
      'repl',
      'stream',
      'string_decoder',
      'timers',
      'tls_(ssl)',
      'tracing',
      'tty',
      'dgram',
      'url',
      'util',
      'v8',
      'vm',
      'zlib' ]

This list was obtained from the Node documentation API https://nodejs.org/api/all.html (JSON file: https://nodejs.org/api/all.json\).

# All core modules at-a-glance


**assert**   <p>The <code>assert</code> module provides a simple set of assertion tests that can be used to
test invariants.</p>

**buffer**   <p>Prior to the introduction of <a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Refer
ence/Global_Objects/TypedArray"><code>TypedArray</code></a> in ECMAScript 2015 (ES6), the
JavaScript language had no mechanism for reading or manipulating streams
of binary data. The <code>Buffer</code> class was introduced as part of the Node.js
API to make it possible to interact with octet streams in the context of things
like TCP streams and file system operations.</p>
<p>Now that <a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArra
y"><code>TypedArray</code></a> has been added in ES6, the <code>Buffer</code> class implements the
<a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8Array"><code>Uin
t8Array</code></a> API in a manner that is more optimized and suitable for Node.js&#39;
use cases.</p>

**c/c++_addons**    <p>Node.js Addons are dynamically-linked shared objects, written in C or C++, that
can be loaded into Node.js using the <a href="globals.html#globals_require"><code>require()</code></a> function
, and used
just as if they were an ordinary Node.js module. They are used primarily to
provide an interface between JavaScript running in Node.js and C/C++ libraries.</p>

**child_process**    <p>The <code>child_process</code> module provides the ability to spawn child processes in
a manner that is similar, but not identical, to popen(3). </p>


[**cluster**][1]          <p>A single instance of Node.js runs in a single thread. To take advantage of multi-core systems the user will sometimes want to launch a cluster of Node.js
processes to handle the load.
The cluster module allows you to easily create child processes that
all share server ports.</p>

**console**          <p>The <code>console</code> module provides a simple debugging console that is similar to the
JavaScript console mechanism provided by web browsers.</p>

**crypto**   <p>The <code>crypto</code> module provides cryptographic functionality that includes a set of
wrappers for OpenSSL&#39;s hash, HMAC, cipher, decipher, sign and verify functions.</p>

**deprecated_apis**          <p>Node.js may deprecate APIs when either: (a) use of the API is considered to be
unsafe, (b) an improved alternative API has been made available, or (c)
breaking changes to the API are expected in a future major release.</p>

**dns** <p>The <code>dns</code> module contains functions belonging to two different categories:</p>

 1. Functions that use the underlying operating system facilities to perform
name resolution, and that do not necessarily perform any network communication.
This category contains only one function: <a href="dns.html#dns_dns_lookup_hostname_options_callback"><code>dns.lookup()</code></a>. 
 2. Functions that connect to an actual DNS server to perform name resolution,
and that <em>always</em> use the network to perform DNS queries. This category
contains all functions in the <code>dns</code> module <em>except</em> <a href="dns.html#dns_dns_lookup_hostname_options_callback"><code>dns.lookup()</code></a>.      

**domain**   <p><strong>_This module is pending deprecation_</strong>. Once a replacement API has been
finalized, this module will be fully deprecated. Most end users should
<strong>not</strong> have cause to use this module. Users who absolutely must have
the functionality that domains provide may rely on it for the time being
but should expect to have to migrate to a different solution
in the future.</p>

[**Events**][2]   <p>Much of the Node.js core API is built around an idiomatic asynchronous
event-driven architecture in which certain kinds of objects (called &quot;emitters&quot;)
periodically emit named events that cause Function objects (&quot;listeners&quot;) to be
called.</p>

**fs**       <p>File I/O is provided by simple wrappers around standard POSIX functions.  To use this module do <code>require(&#39;fs&#39;)</code>. All the methods have asynchronous and synchronous forms.</p>

[**http**][3]     
<p>The HTTP interfaces in Node.js are designed to support many features
of the protocol which have been traditionally difficult to use.
In particular, large, possibly chunk-encoded, messages. The interface is
careful to never buffer entire requests or responses--the
user is able to stream data.</p>

**https**    <p>HTTPS is the HTTP protocol over TLS/SSL. In Node.js this is implemented as a
separate module.</p>

**module**   <p>Node.js has a simple module loading system.  In Node.js, files and modules
are in one-to-one correspondence (each file is treated as a separate module).</p>

**net**      <p>The <code>net</code> module provides you with an asynchronous network wrapper. It contains
functions for creating both servers and clients (called streams). You can include
this module with <code>require(&#39;net&#39;);</code>.</p>

**os**       <p>The <code>os</code> module provides a number of operating system-related utility methods.


**path**     <p>The <code>path</code> module provides utilities for working with file and directory paths.

**punycode**         <p><strong>_The version of the punycode module bundled in Node.js is being deprecated_</strong>.

**querystring**      <p>The <code>querystring</code> module provides utilities for parsing and formatting URL query strings. 

[**readline**][4]         <p>The <code>readline</code> module provides an interface for reading data from a <a href="#st
ream_class_stream_readable">Readable</a>
stream (such as <a href="process.html#process_process_stdin"><code>process.stdin</code></a>) one line at a time. 

**repl**     <p>The <code>repl</code> module provides a Read-Eval-Print-Loop (REPL) implementation that
is available both as a standalone program or includible in other applications.

[**stream**][5]   <p>A stream is an abstract interface for working with streaming data in Node.js.
The <code>stream</code> module provides a base API that makes it easy to build objects
that implement the stream interface.</p>
<p>There are many stream objects provided by Node.js. For instance, a
<a href="http.html#http_class_http_incomingmessage">request to an HTTP server</a> and <a href="process.html#pro
cess_process_stdout"><code>process.stdout</code></a>
are both stream instances.</p>

**string_decoder**   <p>The <code>string_decoder</code> module provides an API for decoding <code>Buffer</code> objects into
strings in a manner that preserves encoded multi-byte UTF-8 and UTF-16
characters. 

**timers**   <p>The <code>timer</code> module exposes a global API for scheduling functions to
be called at some future period of time. Because the timer functions are
globals, there is no need to call <code>require(&#39;timers&#39;)</code> to use the API.</p>
<p>The timer functions within Node.js implement a similar API as the timers API
provided by Web Browsers but use a different internal implementation that is
built around <a href="https://nodejs.org/en/docs/guides/event-loop-timers-and-nexttick">the Node.js Event Loop</a>.</p>

**tls_(ssl)**        <p>The <code>tls</code> module provides an implementation of the Transport Layer Security
(TLS) and Secure Socket Layer (SSL) protocols that is built on top of OpenSSL.


**tracing**          <p>Trace Event provides a mechanism to centralize tracing information generated by
V8, Node core, and userspace code.</p>
<p>Tracing can be enabled by passing the <code>--trace-events-enabled</code> flag when starting a
Node.js application.</p>

**tty**      <p>The <code>tty</code> module provides the <code>tty.ReadStream</code> and <code>tty.WriteStream</code> classes.
In most cases, it will not be necessary or possible to use this module directly.

**dgram**    <p>The <code>dgram</code> module provides an implementation of UDP Datagram sockets.</p>

**url**      <p>The <code>url</code> module provides utilities for URL resolution and parsing. 

**util**     <p>The <code>util</code> module is primarily designed to support the needs of Node.js&#39; own internal APIs. However, many of the utilities are useful for application and module developers as well. 

**v8**       <p>The <code>v8</code> module exposes APIs that are specific to the version of <a href="https://developers.google.com/v8/">V8</a> built into the Node.js binary. 
<p><em>Note</em>: The APIs and implementation are subject to change at any time.</p>

**vm**       <p>The <code>vm</code> module provides APIs for compiling and running code within V8 Virtual Machine contexts. 
JavaScript code can be compiled and run immediately or compiled, saved, and run
later.</p>
<p><em>Note</em>: The vm module is not a security mechanism.
<strong><em>Do not use it to run untrusted code</em></strong>.</p>

**zlib**     <p>The <code>zlib</code> module provides compression functionality implemented using Gzip and
Deflate/Inflate. 


  [1]: https://www.wikiod.com/node-js/cluster-module
  [2]: https://www.wikiod.com/node-js/event-emitters
  [3]: https://www.wikiod.com/node-js/http
  [4]: https://www.wikiod.com/node-js/readline
  [5]: https://www.wikiod.com/node-js/using-streams

## TLS Socket: server and client
The only major differences between this and a regular TCP connection are the private Key and the public certificate that you’ll have to set into an option object.

# How to Create a Key and Certificate

The first step in this security process is the creation of a private Key. And what is this private key? Basically, it's a set of random noise that's used to encrypt information. In theory, you could create one key, and use it to encrypt whatever you want. But it is best practice to have different keys for specific things. Because if someone steals your private key, it's similar to having someone steal your house keys. Imagine if you used the same key to lock your car, garage, office, etc.

`openssl genrsa -out private-key.pem 1024`

Once we have our private key, we can create a CSR (certificate signing request), which is our request to have the private key signed by a fancy authority. That is why you have to input information related to your company. This information will be seen by the signing authority, and used to verify you. In our case, it doesn’t matter what you type, since in the next step we're going to sign our certificate ourselves.

`openssl req -new -key private-key.pem -out csr.pem`

Now that we have our paper work filled out, it's time to pretend that we're a cool signing authority.

`openssl x509 -req -in csr.pem -signkey private-key.pem -out public-cert.pem`

Now that you have the private key and the public cert, you can establish a secure connection between two NodeJS apps. And, as you can see in the example code, it is a very simple process.

# Important!

Since we created the public cert ourselves, in all honesty, our certificate is worthless, because we are nobodies. The NodeJS server won't trust such a certificate by default, and that is why we need to tell it to actually trust our cert with the following option rejectUnauthorized: false. **Very important**: never set this variable to true in a production environment.

# TLS Socket Server

    'use strict';
    
    var tls = require('tls');
    var fs = require('fs');
    
    const PORT = 1337;
    const HOST = '127.0.0.1'
    
    var options = {
        key: fs.readFileSync('private-key.pem'),
        cert: fs.readFileSync('public-cert.pem')
    };
    
    var server = tls.createServer(options, function(socket) {
    
        // Send a friendly message
        socket.write("I am the server sending you a message.");
    
        // Print the data that we received
        socket.on('data', function(data) {
    
            console.log('Received: %s [it is %d bytes long]',
                data.toString().replace(/(\n)/gm,""),
                data.length);
    
        });
    
        // Let us know when the transmission is over
        socket.on('end', function() {
    
            console.log('EOT (End Of Transmission)');
    
        });
    
    });
    
    // Start listening on a specific port and address
    server.listen(PORT, HOST, function() {
    
        console.log("I'm listening at %s, on port %s", HOST, PORT);
    
    });
    
    // When an error occurs, show it.
    server.on('error', function(error) {
    
        console.error(error);
    
        // Close the connection after the error occurred.
        server.destroy();
    
    });

# TLS Socket Client 

    'use strict';
    
    var tls = require('tls');
    var fs = require('fs');
    
    const PORT = 1337;
    const HOST = '127.0.0.1'
    
    // Pass the certs to the server and let it know to process even unauthorized certs.
    var options = {
        key: fs.readFileSync('private-key.pem'),
        cert: fs.readFileSync('public-cert.pem'),
        rejectUnauthorized: false
    };
    
    var client = tls.connect(PORT, HOST, options, function() {
    
        // Check if the authorization worked
        if (client.authorized) {
            console.log("Connection authorized by a Certificate Authority.");
        } else {
            console.log("Connection not authorized: " + client.authorizationError)
        }
    
        // Send a friendly message
        client.write("I am the client sending you a message.");
    
    });
    
    client.on("data", function(data) {
    
        console.log('Received: %s [it is %d bytes long]',
            data.toString().replace(/(\n)/gm,""),
            data.length);
    
        // Close the connection after receiving the message
        client.end();
    
    });
    
    client.on('close', function() {
    
        console.log("Connection closed");
    
    });
    
    // When an error ocoures, show it.
    client.on('error', function(error) {
    
        console.error(error);
    
        // Close the connection after the error occurred.
        client.destroy();
    
    });

## How to get a basic HTTPS web server up and running!
Once you have node.js installed on your system, you can just follow the procedure below to get a basic web server running with support for both HTTP and HTTPS!

________
________


## Step 1 : Build a Certificate Authority

1. create the folder where you want to store your key & certificate :

    `mkdir conf`

________

2. go to that directory :

    `cd conf`

________

3. grab this `ca.cnf` file to use as a configuration shortcut :

    `wget https://raw.githubusercontent.com/anders94/https-authorized-clients/master/keys/ca.cnf`

________

4. create a new certificate authority using this configuration :

    `openssl req -new -x509 -days 9999 -config ca.cnf -keyout ca-key.pem -out ca-cert.pem`

________

5. now that we have our certificate authority in `ca-key.pem` and `ca-cert.pem`, let's generate a private key for the server :

    `openssl genrsa -out key.pem 4096`

________

6. grab this `server.cnf` file to use as a configuration shortcut :

    `wget https://raw.githubusercontent.com/anders94/https-authorized-clients/master/keys/server.cnf`

________

7. generate the certificate signing request using this configuration :

    `openssl req -new -config server.cnf -key key.pem -out csr.pem`

________

8. sign the request :

    `openssl x509 -req -extfile server.cnf -days 999 -passin "pass:password" -in csr.pem -CA ca-cert.pem -CAkey ca-key.pem -CAcreateserial -out cert.pem`

________
________

## Step 2 : Install your certificate as a root certificate

1. copy your certificate to your root certificates' folder :

    `sudo cp ca-crt.pem /usr/local/share/ca-certificates/ca-crt.pem`

________

2. update CA store :

    `sudo update-ca-certificates`

________
________

## Step 3 : Starting your node server

First, you want to create a `server.js` file that contains your actual server code.

The minimal setup for an HTTPS server in Node.js would be something like this :

    var https = require('https');
    var fs = require('fs');
    
    var httpsOptions = {
        key: fs.readFileSync('path/to/server-key.pem'),
        cert: fs.readFileSync('path/to/server-crt.pem')
    };

    var app = function (req, res) {
      res.writeHead(200);
      res.end("hello world\n");
    }
    
    https.createServer(httpsOptions, app).listen(4433);

If you also want to support http requests, you need to make just this small modification :

    var http = require('http');
    var https = require('https');
    var fs = require('fs');
    
    var httpsOptions = {
        key: fs.readFileSync('path/to/server-key.pem'),
        cert: fs.readFileSync('path/to/server-crt.pem')
    };

    var app = function (req, res) {
      res.writeHead(200);
      res.end("hello world\n");
    }
    
    http.createServer(app).listen(8888);
    https.createServer(httpsOptions, app).listen(4433);


1. go to the directory where your `server.js` is located :

    `cd /path/to`

________

2. run `server.js` :

    `node server.js`

