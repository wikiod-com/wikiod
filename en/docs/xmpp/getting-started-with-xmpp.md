---
title: "Getting started with xmpp"
slug: "getting-started-with-xmpp"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Connecting and sending a message
## SleekXMPP (Python) ##

    import sleekxmpp

    client = sleekxmpp.Client("address@example.net", "password")
    client.connect()
    client.process(blocking=False)
    client.send_message(mto="remote@example.net", mbody=self.msg)


----------

Smack (Java / Android)
----------------------

    XMPPTCPConnection connection = new XMPPTCPConnection("user", "password", "example.org")
    connection.connect().login();
    Message message = new Message("otheruser@example.net", "Hi, how are you?");
    connection.sendStanza(message);
    connection.disconnect();

## Creating a Chat Session and sending a message
Smack (Java)

 * Using Smack 4.1
 * It is recommended to include Smack as Maven dependency in your project (e.g. by using gradle or Maven).
 * Otherwhise the following Smack artifacts/jars have to be added manually to the classpath: smack-core, smack-extensions,
 smack-experimental, smack-im, smnack-tcp, smack-java7

<!-- language: java -->

    import org.jivesoftware.smack.ConnectionConfiguration.SecurityMode;
    import org.jivesoftware.smack.SmackException;
    import org.jivesoftware.smack.XMPPException;
    import org.jivesoftware.smack.chat.Chat;
    import org.jivesoftware.smack.chat.ChatManager;
    import org.jivesoftware.smack.chat.ChatMessageListener;
    import org.jivesoftware.smack.packet.Message;
    import org.jivesoftware.smack.packet.Presence;
    import org.jivesoftware.smack.tcp.XMPPTCPConnection;
    import org.jivesoftware.smack.tcp.XMPPTCPConnectionConfiguration;

    public void sendMessage() {

    XMPPTCPConnectionConfiguration config = 
      XMPPTCPConnectionConfiguration.builder()
                .setServiceName("mydomain.local")
                .setHost("127.0.0.1")
                .setPort(5222)
                .build();
 
    XMPPTCPConnection connection = new XMPPTCPConnection(config);

    connection.connect();
    connection.login("test1", "test1pwd"); 

    ChatManager chatManager = ChatManager.getInstanceFor(connection);
    String test2JID = "test2@domain.example";
    Chat chat = chatManager.createChat(test2JID);
    chat.sendMessage("Hello, how are you?");

    connection.disconnect();
    }


## Create Xmpp Client Connection Using agsxmpp library
    public void OpenXmppConnection(int port, bool useSsl, string serverJid, string userName, string password)
            {
                try
                {
                    _xmppClientConnection.AutoResolveConnectServer = true;
                    _xmppClientConnection.Port = port;
                    _xmppClientConnection.UseSSL = useSsl;
                    _xmppClientConnection.Server = serverJid;
                    _xmppClientConnection.Username = userName;
                    _xmppClientConnection.Password = password;
                    _xmppClientConnection.Resource = "web";
    
    
                    //authenticate and open connection with server
                    _xmppClientConnection.Open();
                }
                catch (Exception ex)
                {
                }
            }

## Send a message  using agsxmpp library
            public class ConversationManager
            {
                #region ClassMemeber
        
                private XmppClientConnection _xmppClientConnection = null;
        
        
        
         public ConversationManager(XmppClientConnection con)
                {
                    _xmppClientConnection = con;
                 }
    
     public void SendMessage(string message, string to, string guid, string type)
            {
                try
                {
                    if (_xmppClientConnection != null)
                    {
                        Jid jidTo = new Jid(to);
                        agsXMPP.protocol.client.Message mesg = new agsXMPP.protocol.client.Message(jidTo, _ConnectionWrapper.MyJid,
                            agsXMPP.protocol.client.MessageType.chat,
                                          message);
    
                        mesg.Id = guid;
                        mesg.AddChild(new agsXMPP.protocol.extensions.msgreceipts.Request());//request delievery
                        _xmppClientConnection.Send(mesg);
                    }
                }
                catch (Exception ex)
                {
                }
            }
    
    }



