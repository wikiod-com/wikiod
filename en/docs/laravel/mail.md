---
title: "Mail"
slug: "mail"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Basic example
You can configure Mail by just adding/changing these lines in the app's **.ENV** file with your email provider login details, for example for using it with gmail you can use:

    MAIL_DRIVER=smtp
    MAIL_HOST=smtp.gmail.com
    MAIL_PORT=587
    MAIL_USERNAME=yourEmail@gmail.com
    MAIL_PASSWORD=yourPassword
    MAIL_ENCRYPTION=tls

Then you can start sending emails using Mail, for example:

    $variable = 'Hello world!'; // A variable which can be use inside email blade template.
    Mail::send('your.blade.file', ['variable' => $variable], function ($message) {
                $message->from('john@doe.com');
                $message->sender('john@doe.com');
                $message->to(foo@bar.com);
                $message->subject('Hello World');
            });

