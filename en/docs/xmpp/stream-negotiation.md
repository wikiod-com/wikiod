---
title: "Stream Negotiation"
slug: "stream-negotiation"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

XMPP connections comprise two XML streams: one for ingress and one for egress. These streams are generally sent over the same TCP connection (although sometimes multiple connections may be used, especially for server-to-server connections) and share certain features for which negotiation is required (eg. authentication with SASL).

## Closing a stream
A stream is closed by sending a closing `</stream>` tag. After the closing stream tag is sent, no more data should be sent on the stream (even in response to data received from the other party). Before closing the connection, the sending entity should wait for a response `</stream>` tag to give the other party time to send any outstanding data and should time out (and terminate the underlying TCP connection[s]) if a closing stream tag is not received within a chosen amount of time.

<!-- language: lang-xml -->

    </stream:stream>

If the stream is encrypted with TLS, the parties must cleanly terminate TLS by sending a TLS `close_notify` alert and receiving one in response. Your TLS library probably does this for you.

## Starting a stream
Once a TCP connection is established, the initial stream header is sent by the initiating entity. Similarly, whenever a stream restart is required (eg. after negotiating a security layer such as TLS) a stream header must also be sent:

<!-- language: lang-xml -->

    <?xml version='1.0'?>
    <stream:stream
        from='juliet@im.example.com'
        to='im.example.com'
        version='1.0'
        xml:lang='en'
        xmlns='jabber:client'
        xmlns:stream='http://etherx.jabber.org/streams'>

The XML header is optional, but if it exists it must not specify anything other than XML version 1.0 with UTF-8 encoding.

In response, the receiving entity will send its own opening stream tag containing a unique session ID:

<!-- language: lang-xml -->

    <?xml version='1.0'?>
    <stream:stream
        from='im.example.com'
        id='++TR84Sm6A3hnt3Q065SnAbbk3Y='
        to='juliet@im.example.com'
        version='1.0'
        xml:lang='en'
        xmlns='jabber:client'
        xmlns:stream='http://etherx.jabber.org/streams'>

## Close XMPP connection using agsxmpp library
      public class ConnectionManager
        { 
             private XmppClientConnection _xmppClientConnection = null;
    
             public ConnectionManager()
                    {
                        if (_xmppClientConnection == null)
                        {
                            _xmppClientConnection = new XmppClientConnection();
                        }
                    }
             public void CloseXmppConnection()
                    {
                        try
                        {
                            if (_xmppClientConnection != null)
                            {
                                    //Close xmpp Client Connection
                                 _xmppClientConnection.Close();
                            }
                            
                          
                        }
                        catch (Exception ex)
                        {
                        }
                    }
        }

