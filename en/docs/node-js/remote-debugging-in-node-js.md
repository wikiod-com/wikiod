---
title: "Remote Debugging in Node.JS"
slug: "remote-debugging-in-nodejs"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## NodeJS run configuration
To set up Node remote debugging, simply run the node process with the `--debug` flag. You can add a port on which the debugger should run using `--debug=<port>`.

When your node process starts up you should see the message

    Debugger listening on port <port>

Which will tell you that everything is good to go.

Then you set up the remote debugging target in your specific IDE.

## IntelliJ/Webstorm Configuration

1. Make sure that the NodeJS plugin is enabled
1. Select your run configurations (screen)

[![Run configurations][1]][1]
3. Select **+** > **Node.js Remote Debug**

[![Add new configuration][2]][2]
4. Make sure you enter the port selected above as well as the correct host

[![Configure port and host][3]][3]

Once those are configured simply run the debug target as you normally would and it will stop on your breakpoints.

  [1]: http://i.stack.imgur.com/74hst.png
  [2]: http://i.stack.imgur.com/MVlrq.png
  [3]: http://i.stack.imgur.com/x7Hbu.png

## Use the proxy for debugging via port on Linux
If you start your application on Linux, use the proxy for debugging via port, for example:

    socat TCP-LISTEN:9958,fork TCP:127.0.0.1:5858 &

Use port 9958 for remote debugging then.

