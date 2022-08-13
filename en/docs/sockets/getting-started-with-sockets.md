---
title: "Getting started with sockets"
slug: "getting-started-with-sockets"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## How to instantiate a socket class object
Instantiating a socket can be done in various ways. 

1. by 2 line declaration & instantiation:

    First we need to define a variable which will hold a Socket class object:

       Socket socket;

    then we can create a Socket class object:

       socket = new Socket();

2. We can also make a one line definition & instantiation: 

       Socket  socket = new Socket();

both ways will create an unconnected socket. 


We can use other parameterized constructors to instantiate connected or unconnected socket class object: 

For details see class doc specs: 

  https://docs.oracle.com/javase/7/docs/api/java/net/Socket.html





## Create unconnected socket, try connect to it and check if connection is established
     public class ConnectSocketExample {

        private int HTTP_PORT = 80;

        /**
         * example method to create unconnected socket
         * then connect to it
         * at end return connected socket
         *
         * @param httpHostName - endpoint host name fot socket connection
         * @throws IOException - if the socket is already connected or an error occurs while connecting.
         */
        protected Socket connectSocket(String httpHostName) throws IOException {
            // define local variable for socket and create unconnected socket
            Socket socket = new Socket();
            //  create iNet address for socket
            InetSocketAddress inetSocketAddress = new InetSocketAddress(httpHostName, HTTP_PORT);
            // connect socket to inet address (end point )
            socket.connect(inetSocketAddress);
            // return connected socket for later use 
            return socket;
        }
        
        /** 
         * public method for try to create connected to goole.com http port socket 
         * and with check and system out print if this try was successful
         **/
        public void createAndCheckIfConnected() {
            try {
                Socket connectedSocket = connectSocket("google.com");
                boolean connected = connectedSocket.isConnected();
                System.out.print("Socket is:" + (!connected ? " not" : "" +  " connected"));
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

    }

## Write to socket a simple  http get request, and dump response
   
    /** 
     * we reuse a class written in example:
     * https://www.wikiod.com/sockets/getting-started-with-sockets
     * pleas to familiar with it first to continue with this one 
     **/
    public class WriteToSocketExample extends ConnectSocketExample {

        private String CRLF = "\r\n"; // line termination (separator)

        /**
         * write a simple http get request to socket
         * @param host - host to establish a connection
         *               (http server - see ConnectSocketExample HTTP_PORT )
         * @param path - path to file ( in this case a url location - part used in browser after host)
         * @return a  connected socket with filled in raw get request
         * @throws IOException - see ConnectSocketExample.connectSocket(host);
         */
        protected Socket writeGetToSocket(String host, String path) throws IOException {
            // create simple http raw get request for host/path
            String rawHttpGetRequest = "GET "+ path +" HTTP/1.1 " + CRLF  // request line
                    + "Host: "+ host + CRLF
                    + CRLF;
            // get bytes of this request using proper encodings
            byte[] bytesOfRequest = rawHttpGetRequest.getBytes(Charset.forName("UTF-8)"));
            // create & connect to socket
            Socket socket = connectSocket(host);
            // get socket output stream
            OutputStream outputStream = socket.getOutputStream();
            // write to the stream a get request we created
            outputStream.write(bytesOfRequest);
            // return socket with written get request
            return  socket;
        }

        /**
         * create, connect and write to socket simple http get request
         * then dump response of this request
         * @throws IOException
         */
        public void writeToSocketAndDumpResponse() throws IOException {
            // send request to http server for / page content
            Socket socket = writeGetToSocket("google.com", "/");
            // now we will read response from server
            InputStream inputStream = socket.getInputStream();
            // create a byte array buffer to read respons in chunks
            byte[] buffer = new byte[1024];
            // define a var to hold count of read bytes from stream
            int weRead;
            // read bytes from sockets till exhausted or read time out will occurred ( as we didn't add in raw get header Connection: close (default keep-alive)
            while ((weRead = inputStream.read(buffer)) != -1) {
                // print what we have read
                System.out.print(new String(buffer));
            }
        }
    }

