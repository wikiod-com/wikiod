---
title: "System.Net.Mail"
slug: "systemnetmail"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Il est important de disposer d'un System.Net.MailMessage car chaque pièce jointe contient un flux et ces flux doivent être libérés dès que possible. L'instruction using garantit que l'objet Disposable est Disposed également en cas d'exceptions

## MailMessage
Voici l'exemple de la création d'un message électronique avec des pièces jointes. Après la création, nous envoyons ce message à l'aide de la classe `SmtpClient`. Le port 25 par défaut est utilisé ici.

    public class clsMail
    {
        private static bool SendMail(string mailfrom, List<string>replytos, List<string> mailtos, List<string> mailccs, List<string> mailbccs, string body, string subject, List<string> Attachment)
        {
            try
            {
                using(MailMessage MyMail = new MailMessage())
                {
                    MyMail.From = new MailAddress(mailfrom);
                    foreach (string mailto in mailtos)
                        MyMail.To.Add(mailto);

                    if (replytos != null && replytos.Any())
                    {
                        foreach (string replyto in replytos)
                            MyMail.ReplyToList.Add(replyto);
                    }

                    if (mailccs != null && mailccs.Any())
                    {
                        foreach (string mailcc in mailccs)
                            MyMail.CC.Add(mailcc);
                    }

                    if (mailbccs != null && mailbccs.Any())
                    {
                        foreach (string mailbcc in mailbccs)
                            MyMail.Bcc.Add(mailbcc);
                    }                         

                    MyMail.Subject = subject;
                    MyMail.IsBodyHtml = true;
                    MyMail.Body = body;
                    MyMail.Priority = MailPriority.Normal;

                    if (Attachment != null && Attachment.Any())
                    {
                        System.Net.Mail.Attachment attachment;
                        foreach (var item in Attachment)
                        {
                            attachment = new System.Net.Mail.Attachment(item);
                            MyMail.Attachments.Add(attachment);
                        }
                    }

                    SmtpClient smtpMailObj = new SmtpClient();
                    smtpMailObj.Host = "your host";
                    smtpMailObj.Port = 25;
                    smtpMailObj.Credentials = new System.Net.NetworkCredential("uid", "pwd");

                    smtpMailObj.Send(MyMail);
                    return true;
                }
            }
            catch
            {
                return false;
            }
        }
    }


## Courrier avec pièce jointe
`MailMessage` représente un message électronique qui peut être envoyé ultérieurement à l'aide de la classe `SmtpClient`. Plusieurs pièces jointes (fichiers) peuvent être ajoutées au message électronique.
    
    using System.Net.Mail;

    using(MailMessage myMail = new MailMessage())
    {
         Attachment attachment = new Attachment(path);
         myMail.Attachments.Add(attachment);

         // further processing to send the mail message

    }



