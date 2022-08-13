---
title: "The Javamail API"
slug: "the-javamail-api"
draft: false
images: []
weight: 9966
type: docs
toc: true
---



The JavaMail page on the Oracle website describes it as follows

> The JavaMail API provides a platform-independent and protocol-independent framework to build mail and messaging applications. The JavaMail API is available as an optional package for use with the Java SE platform and is also included in the Java EE platform.

The primary site for the JavaMail project is now on [java.net][1]. From there you can find the javadocs for many versions of the APIs, links to the source code repositories, links for downloads, examples and hints for using JavaMail with some popular Email service providers.


  [1]: https://java.net/projects/javamail/pages/Home

## Send HTML Formatted Mail


## Send Simple Email


## Sending an email to a POP3 email server
This example shows how to establish a connection to an SSL-enabled POP3 email server and send a simple (text only) email.  

        // Configure mail provider
        Properties props = new Properties();
        props.put("mail.smtp.host", "smtp.mymailprovider.com");
        props.put("mail.pop3.host", "pop3.mymailprovider.com");
        // Enable SSL
        props.put("mail.pop3.ssl.enable", "true");
        props.put("mail.smtp.starttls.enable", "true");

        // Enable SMTP Authentication
        props.put("mail.smtp.auth","true");

        Authenticator auth = new PasswordAuthentication("user", "password");
        Session session = Session.getDefaultInstance(props, auth);

        // Get the store for authentication
        final Store store;
        try {
            store = session.getStore("pop3");
        } catch (NoSuchProviderException e) {
            throw new IllegalStateException(e);
        }

        try {
            store.connect();
        } catch (AuthenticationFailedException | MessagingException e) {
            throw new IllegalStateException(e);
        }
        
        try {
          // Setting up the mail
          InternetAddress from = new InternetAddress("sender@example.com");
          InternetAddress to = new InternetAddress("receiver@example.com");

          MimeMessage message = new MimeMessage(session);
          message.setFrom(from);
          message.addRecipient(Message.RecipientType.TO, to);

          message.setSubject("Test Subject");
          message.setText("Hi, I'm a Mail sent with Java Mail API.");

          // Send the mail
          Transport.send(message);
        } catch (AddressException | MessagingException e)
            throw new IllegalStateException(e);
        } 

Caveats:

  - Various details have been hardwired into the code above for illustration purposes.
  - The exception handling is NOT examplary.  The `IllegalStateException` is a bad choice, for a start.
  - No attempt has been made to handle *resources* correctly.


