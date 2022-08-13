---
title: "The WebSockets API"
slug: "the-websockets-api"
draft: false
images: []
weight: 9973
type: docs
toc: true
---



WebSocket is a protocol which allows for communication between the client and the server/endpoint using a single TCP connection.

WebSocket is designed to be implemented in web browsers and web servers, but it can be used by any client or server application.

This topic about the Java APIs for websockets that were developed by [JSR 356][1] and incorporated into the Java EE 7 specifications.


  [1]: https://jcp.org/en/jsr/detail?id=356

## Creating a WebSocket communication


## Encoders and Decoder: Object-Oriented WebSockets
Thanks to encoders and decoders, the JSR 356 offers a object oriented communication models. 

**Messages definition**

Let's assume all received messages have to be transformed by the server before being sent back to all connected sessions:

    public abstract class AbstractMsg {
        public abstract void transform();
    }

Let's now assume that the server manage two message types: a text-based message and an integer-based message.

Integer messages multiply the content by itself.

    public class IntegerMsg extends AbstractMsg {
    
        private Integer content;
    
        public IntegerMsg(int content) {
            this.content = content;
        }
    
        public Integer getContent() {
            return content;
        }
    
        public void setContent(Integer content) {
            this.content = content;
        }
    
        @Override
        public void transform() {
            this.content = this.content * this.content;
        }
    }

String message prepend some text:

    public class StringMsg extends AbstractMsg {
    
        private String content;
    
        public StringMsg(String content) {
            this.content = content;
        }
    
        public String getContent() {
            return content;
        }
    
        public void setContent(String content) {
            this.content = content;
        }
    
        @Override
        public void transform() {
            this.content = "Someone said: " + this.content;
        }
    }

**Encoders and Decoder**

There is one encoder per message type and a single decoder for all messages. Encoders must implements `Encoder.XXX<Type>` interface when Decoder must implements `Decoder.XXX<Type>`.

Encoding is fairly straightforward: from a message, the `encode` method must output a JSON formatted String. Here is the example for `IntegerMsg`.

    public class IntegerMsgEncoder implements Encoder.Text<IntegerMsg> {
    
        @Override
        public String encode(IntegerMsg object) throws EncodeException {
            JsonObjectBuilder builder = Json.createObjectBuilder();
            
            builder.add("content", object.getContent());
            
            JsonObject jsonObject = builder.build();
            return jsonObject.toString();
        }
    
        @Override
        public void init(EndpointConfig config) {
            System.out.println("IntegerMsgEncoder initializing");
        }
    
        @Override
        public void destroy() {
            System.out.println("IntegerMsgEncoder closing");
        }
    }

Similar encoding for `StringMsg` class. Obviously, encoders can be factorized via abstract classes.

    public class StringMsgEncoder implements Encoder.Text<StringMsg> {
    
        @Override
        public String encode(StringMsg object) throws EncodeException {
            JsonObjectBuilder builder = Json.createObjectBuilder();
    
            builder.add("content", object.getContent());
    
            JsonObject jsonObject = builder.build();
            return jsonObject.toString();
        }
    
        @Override
        public void init(EndpointConfig config) {
            System.out.println("StringMsgEncoder initializing");
        }
    
        @Override
        public void destroy() {
            System.out.println("StringMsgEncoder closing");
        }
    
    }

Decoder proceeds in two steps: checking if the received message fits the excepted format with `willDecode` and then transform the received raw message into a object with `decode`:

public class MsgDecoder implements Decoder.Text<AbstractMsg> {

    @Override
    public AbstractMsg decode(String s) throws DecodeException {
        // Thanks to willDecode(s), one knows that
        // s is a valid JSON and has the attribute
        // "content"
        JsonObject json = Json.createReader(new StringReader(s)).readObject();
        JsonValue contentValue = json.get("content");

        // to know if it is a IntegerMsg or a StringMsg, 
        // contentValue type has to be checked:
        switch (contentValue.getValueType()) {
            case STRING:
                String stringContent = json.getString("content");
                return new StringMsg(stringContent);
            case NUMBER:
                Integer intContent = json.getInt("content");
                return new IntegerMsg(intContent);
            default:
                return null;
        }

    }

    @Override
    public boolean willDecode(String s) {

        // 1) Incoming message is a valid JSON object
        JsonObject json;
        try {
            json = Json.createReader(new StringReader(s)).readObject();
        }
        catch (JsonParsingException e) {
            // ...manage exception...
            return false;
        }
        catch (JsonException e) {
            // ...manage exception...
            return false;
        }

        // 2) Incoming message has required attributes
        boolean hasContent = json.containsKey("content");

        // ... proceed to additional test ...
        return hasContent;
    }

    @Override
    public void init(EndpointConfig config) {
        System.out.println("Decoding incoming message...");
    }

    @Override
    public void destroy() {
        System.out.println("Incoming message decoding finished");
    }

}

**ServerEndPoint**

The Server EndPoint pretty looks like the *WebSocket communication* with three main differences:
 1. ServerEndPoint annotation has the `encoders` and `decoders` attributes
 2. Messages are not sent with `sendText` but with `sendObject`
 3. OnError annotation is used. If there was an error thrown during `willDecode`, it will be processed here and error information is sent back to the client

    @ServerEndpoint(value = "/webSocketObjectEndPoint",
            decoders = {MsgDecoder.class},
            encoders = {StringMsgEncoder.class, IntegerMsgEncoder.class})
    public class ServerEndPoint {
    
        @OnOpen
        public void onOpen(Session session) {
            System.out.println("A session has joined");
        }
    
        @OnClose
        public void onClose(Session session) {
            System.out.println("A session has left");
        }
    
        @OnMessage
        public void onMessage(Session session, AbstractMsg message) {
            if (message instanceof IntegerMsg) {
                System.out.println("IntegerMsg received!");
            } else if (message instanceof StringMsg) {
                System.out.println("StringMsg received!");
            }
    
            message.transform();
            sendMessageToAllParties(session, message);
        }
    
        @OnError
        public void onError(Session session, Throwable throwable) {
            session.getAsyncRemote().sendText(throwable.getLocalizedMessage());
        }
    
        private void sendMessageToAllParties(Session session, AbstractMsg message) {
            session.getOpenSessions().forEach(s -> {
                s.getAsyncRemote().sendObject(message);
            });
        }
    }

As I was quite verbose, here is a basic JavaScript client for those who want to have a visual example. Please note that this is a chat-like example: all the connected parties will received the answer.

    <!DOCTYPE html>
    <html>
        <head>
            <title>Websocket-object</title>
            <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">        
            <!-- start of BAD PRACTICE! all style and script must go into a
                 dedicated CSS / JavaScript file-->
            <style>
                body{
                    background: dimgray;
                }
    
                .container{
                    width: 100%;
                    display: flex;
                }
    
                .left-side{
                    width: 30%;
                    padding: 2%;
                    box-sizing:  border-box;
                    margin: auto;
                    margin-top: 0;
                    background: antiquewhite;
                }
                .left-side table{
                    width: 100%;
                    border: 1px solid black;
                    margin: 5px;
                }
                .left-side table td{
                    padding: 2px;
                    width: 50%;
                }
                .left-side table input{
                    width: 100%;
                    box-sizing: border-box;
                }
    
                .right-side{
                    width: 70%;
                    background: floralwhite;
                }
            </style>
    
            <script>
                var ws = null;
                window.onload = function () {
                    // replace the 'websocket-object' with the
                    // context root of your web application.
                    ws = new WebSocket("ws://localhost:8080/websocket-object/webSocketObjectEndPoint");
                    ws.onopen = onOpen;
                    ws.onclose = onClose;
                    ws.onmessage = onMessage;
                };
    
                function onOpen() {
                    printText("", "connected to server");
                }
    
                function onClose() {
                    printText("", "disconnected from server");
                }
    
                function onMessage(event) {
                    var msg = JSON.parse(event.data);
                    printText("server", JSON.stringify(msg.content));
                }
    
                function sendNumberMessage() {
                    var content = new Number(document.getElementById("inputNumber").value);
                    var json = {content: content};
                    ws.send(JSON.stringify(json));
                    printText("client", JSON.stringify(json));
                }
    
                function sendTextMessage() {
                    var content = document.getElementById("inputText").value;
                    var json = {content: content};
                    ws.send(JSON.stringify(json));
                    printText("client", JSON.stringify(json));
                }
    
                function printText(sender, text) {
                    var table = document.getElementById("outputTable");
                    var row = table.insertRow(1);
                    var cell1 = row.insertCell(0);
                    var cell2 = row.insertCell(1);
                    var cell3 = row.insertCell(2);
    
                    switch (sender) {
                        case "client":
                            row.style.color = "orange";
                            break;
                        case "server":
                            row.style.color = "green";
                            break;
                        default:
                            row.style.color = "powderblue";
                    }
                    cell1.innerHTML = new Date().toISOString();
                    cell2.innerHTML = sender;
                    cell3.innerHTML = text;
                }
            </script>
    
            <!-- end of bad practice -->
        </head>
        <body>
    
            <div class="container">
                <div class="left-side">
                    <table>
                        <tr>
                            <td>Enter a text</td>
                            <td><input id="inputText" type="text" /></td>
                        </tr>
                        <tr>
                            <td>Send as text</td>
                            <td><input type="submit" value="Send" onclick="sendTextMessage();"/></td>
                        </tr>
                    </table>
    
                    <table>
                        <tr>
                            <td>Enter a number</td>
                            <td><input id="inputNumber" type="number" /></td>
                        </tr>
                        <tr>
                            <td>Send as number</td>
                            <td><input type="submit" value="Send" onclick="sendNumberMessage();"/></td>
                        </tr>
                    </table>
                </div>
                <div class="right-side">
                    <table id="outputTable">
                        <tr>
                            <th>Date</th>
                            <th>Sender</th>
                            <th>Message</th>
                        </tr>
                    </table>
                </div>
            </div>
        </body>
    </html>

Code is complete and was tested under Payara 4.1. Example is pure standard (no external library/framework)


