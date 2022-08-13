---
title: "Python TCP sockets; simple server and client examples with annotation"
slug: "python-tcp-sockets-simple-server-and-client-examples-with-annotation"
draft: false
images: []
weight: 9590
type: docs
toc: true
---

These are two sample programs that work together. One is a simple server, the other a simple client. Start the server in one window:

    python tserver.py

Edit the server address in the client source file if desired. Then run

    python tclient.py

The client connects to the server, then asks for input from the console, then sends it to the server. For each received buffer, the server prepends some canned info and sends it back to the client.

I've worked around certain pitfalls that arise in porting code between python2 and python3 -- in particular the bytes vs strings differences. A full explanation of that would require a lot of space and distract from the `socket` focus.

Caveats:

The server example, in particular, is focused on the `socket` operations a server will perform, but serialized for clarity. Hence, it only accepts a single connection at a time. A "real" program would either fork a new process to handle each connection, or use `select` to handle multiple connections at once.

Real programs would handle exceptions in the various socket calls, and recover or exit gracefully.

Real programs would need to worry about message boundaries (since TCP doesn't respect those). Since these programs send single buffers at a time triggered by user input, that has been ignored.


## Sample server program (annotated)
    #!/usr/bin/env python
    """
    An annotated simple socket server example in python.

    WARNING: This example doesn't show a very important aspect of
    TCP - TCP doesn't preserve message boundaries. Please refer
    to http://blog.stephencleary.com/2009/04/message-framing.html
    before adapting this code to your application.
    
    Runs in both python2 and python3.
    """
    import socket
    
    # Optionally set a specific address. This (the empty string) will listen on all
    # the local machine's IPv4 addresses. It's a common way to code a general
    # purpose server. If you specify an address here, the client will need to use
    # the same address to connect.
    SERVER_ADDRESS = ''
    
    # Can change this to any port 1-65535 (on many machines, ports <= 1024 are
    # restricted to privileged users)
    SERVER_PORT = 22222
    
    # Create the socket
    s = socket.socket()
    
    # Optional: this allows the program to be immediately restarted after exit.
    # Otherwise, you may need to wait 2-4 minutes (depending on OS) to bind to the
    # listening port again.
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    
    # Bind to the desired address(es) and port. Note the argument is a tuple: hence
    # the extra set of parentheses.
    s.bind((SERVER_ADDRESS, SERVER_PORT))
    
    # How many "pending connections" may be queued. Exact interpretation of this
    # value is complicated and operating system dependent. This value is usually
    # fine for an experimental server.
    s.listen(5)
    
    print("Listening on address %s. Kill server with Ctrl-C" %
          str((SERVER_ADDRESS, SERVER_PORT)))
    
    # Now we have a listening endpoint from which we can accept incoming
    # connections. This loop will accept one connection at a time, then service
    # that connection until the client disconnects. Lather, rinse, repeat.
    while True:
        c, addr = s.accept()
        print("\nConnection received from %s" % str(addr))
    
        while True:
            data = c.recv(2048)
            if not data:
                print("End of file from client. Resetting")
                break
    
            # Decode the received bytes into a unicode string using the default
            # codec. (This isn't strictly necessary for python2, but, since we will
            # be encoding the data again before sending, it works fine there too.)
            data = data.decode()
    
            print("Received '%s' from client" % data)
    
            data = "Hello, " + str(addr) + ". I got this from you: '" + data + "'"
    
            # See above
            data = data.encode()
    
            # Send the modified data back to the client.
            c.send(data)
    
        c.close()

## Sample client program (annotated)
    #!/usr/bin/env python
    """
    An annotated simple socket client example in python.

    WARNING: This example doesn't show a very important aspect of
    TCP - TCP doesn't preserve message boundaries. Please refer
    to http://blog.stephencleary.com/2009/04/message-framing.html
    before adapting this code to your application.
    
    Runs in both python2 and python3.
    """
    import socket
    
    # Note that the server may listen on a specific address or any address
    # (signified by the empty string), but the client must specify an address to
    # connect to. Here, we're connecting to the server on the same machine
    # (127.0.0.1 is the "loopback" address).
    SERVER_ADDRESS = '127.0.0.1'
    SERVER_PORT = 22222
    
    # Create the socket
    c = socket.socket()
    
    # Connect to the server. A port for the client is automatically allocated
    # and bound by the operating system
    c.connect((SERVER_ADDRESS, SERVER_PORT))
    
    # Compatibility hack. In python3, input receives data from standard input. In
    # python2, raw_input does exactly that, whereas input receives data, then
    # "evaluates" the result; we don't want to do that. So on python2, overwrite
    # the input symbol with a reference to raw_input. On python3, trap the
    # exception and do nothing.
    try:
        input = raw_input
    except NameError:
        pass
    
    print("Connected to " + str((SERVER_ADDRESS, SERVER_PORT)))
    while True:
        try:
            data = input("Enter some data: ")
        except EOFError:
            print("\nOkay. Leaving. Bye")
            break
    
        if not data:
            print("Can't send empty string!")
            print("Ctrl-D [or Ctrl-Z on Windows] to exit")
            continue
    
        # Convert string to bytes. (No-op for python2)
        data = data.encode()
    
        # Send data to server
        c.send(data)
    
        # Receive response from server
        data = c.recv(2048)
        if not data:
            print("Server abended. Exiting")
            break
    
        # Convert back to string for python3
        data = data.decode()
    
        print("Got this string from server:")
        print(data + '\n')
    
    c.close()

