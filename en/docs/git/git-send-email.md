---
title: "git send-email"
slug: "git-send-email"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Syntax
 - git send-email [options] <file|directory|rev-list options>…​
 - git send-email --dump-aliases
 

https://git-scm.com/docs/git-send-email

## Use git send-email with Gmail
Background: if you work on a project like the Linux kernel, rather than make a pull request you will need to submit your commits to a listserv for review. This entry details how to use git-send email with Gmail.

Add the following to your .gitconfig file:

    [sendemail]
        smtpserver = smtp.googlemail.com
        smtpencryption = tls
        smtpserverport = 587
        smtpuser = name@gmail.com

Then on the web:
Go to Google -> My Account -> Connected Apps & Sites -> Allow less secure apps -> Switch ON

To create a patch set:

    git format-patch HEAD~~~~ --subject-prefix="PATCH <project-name>"
Then send the patches to a listserv:

    git send-email --annotate --to project-developers-list@listserve.example.com 00*.patch

To create and send updated version (version 2 in this example) of the patch:

    git format-patch -v 2 HEAD~~~~  ......
    git send-email --to project-developers-list@listserve.example.com v2-00*.patch

## Composing
--from                  <str>  * Email From:
    --[no-]to               <str>  * Email To:
    --[no-]cc               <str>  * Email Cc:
    --[no-]bcc              <str>  * Email Bcc:
    --subject               <str>  * Email "Subject:"
    --in-reply-to           <str>  * Email "In-Reply-To:"
    --[no-]xmailer                 * Add "X-Mailer:" header (default).
    --[no-]annotate                * Review each patch that will be sent in an editor.
    --compose                      * Open an editor for introduction.
    --compose-encoding      <str>  * Encoding to assume for introduction.
    --8bit-encoding         <str>  * Encoding to assume 8bit mails if undeclared
    --transfer-encoding     <str>  * Transfer encoding to use (quoted-printable, 8bit, base64)

## Sending patches by mail
Suppose you’ve got a lot of commit against a project (here ulogd2, official branch is git-svn) and that you wan to send your patchset to the Mailling list devel@netfilter.org. To do so, just open a shell at the root of the git directory and use:

```
git format-patch --stat -p --raw --signoff  --subject-prefix="ULOGD PATCH" -o /tmp/ulogd2/ -n git-svn
git send-email --compose --no-chain-reply-to --to devel@netfilter.org /tmp/ulogd2/
```

First command will create a serie of mail from patches in /tmp/ulogd2/ with statistic report and second will start your editor to compose an introduction mail to the patchset. To avoid awful threaded mail series, one can use :

```
git config sendemail.chainreplyto false
```

[source](https://home.regit.org/technical-articles/git-for-the-newbie/)

