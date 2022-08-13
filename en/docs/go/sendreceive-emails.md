---
title: "Sendreceive emails"
slug: "sendreceive-emails"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Syntax
- func PlainAuth(identity, username, password, host string) Auth
- func SendMail(addr string, a Auth, from string, to []string, msg []byte) error

## Sending Email with smtp.SendMail()
Sending email is pretty simple in Go. It helps to understand the RFC 822, which specifies the style an email need to be in, the code below sends a RFC 822 compliant email. 

```
package main

import (
    "fmt"
    "net/smtp"
)

func main() {
    // user we are authorizing as
    from := "someuser@example.com"

    // use we are sending email to
    to := "otheruser@example.com"

    // server we are authorized to send email through
    host := "mail.example.com"

    // Create the authentication for the SendMail()
    // using PlainText, but other authentication methods are encouraged
    auth := smtp.PlainAuth("", from, "password", host)

    // NOTE: Using the backtick here ` works like a heredoc, which is why all the 
    // rest of the lines are forced to the beginning of the line, otherwise the 
    // formatting is wrong for the RFC 822 style
    message := `To: "Some User" <someuser@example.com>
From: "Other User" <otheruser@example.com>
Subject: Testing Email From Go!!

This is the message we are sending. That's it!
`

    if err := smtp.SendMail(host+":25", auth, from, []string{to}, []byte(message)); err != nil {
        fmt.Println("Error SendMail: ", err)
        os.Exit(1)
    }
    fmt.Println("Email Sent!")
}
```

The above will send a message like the following:

```
To: "Other User" <otheruser@example.com>
From: "Some User" <someuser@example.com>
Subject: Testing Email From Go!!

This is the message we are sending. That's it!
.
```

