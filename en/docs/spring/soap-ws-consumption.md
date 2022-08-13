---
title: "SOAP WS Consumption"
slug: "soap-ws-consumption"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Consuming a SOAP WS with Basic auth
Create your own WSMessageSender:

    import java.io.IOException;
    import java.net.HttpURLConnection;
    
    import org.springframework.ws.transport.http.HttpUrlConnectionMessageSender;
    
    import sun.misc.BASE64Encoder;
    
    public class CustomWSMessageSender extends HttpUrlConnectionMessageSender{
    
        @Override
        protected void prepareConnection(HttpURLConnection connection)
                throws IOException {
            
            BASE64Encoder enc = new sun.misc.BASE64Encoder();
            String userpassword = "yourUser:yourPassword";
            String encodedAuthorization = enc.encode( userpassword.getBytes() );
            connection.setRequestProperty("Authorization", "Basic " + encodedAuthorization);
    
            super.prepareConnection(connection);
        }
    }

In your WS configuration class set the MessageSender you just created:

    myWSClient.setMessageSender(new CustomWSMessageSender());

