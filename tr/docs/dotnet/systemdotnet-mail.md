---
title: "System.Net.Mail"
slug: "systemnetmail"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Bir System.Net.MailMessage'ı Atmak önemlidir çünkü her ek bir Akış içerir ve bu Akışların mümkün olan en kısa sürede serbest bırakılması gerekir. using ifadesi, İstisnalar durumunda da Disposable nesnesinin Atılmasını sağlar.

## PostaMesajı
Ekli posta mesajı oluşturma örneği. Oluşturduktan sonra bu mesajı `SmtpClient` sınıfı yardımıyla gönderiyoruz. Burada varsayılan 25 bağlantı noktası kullanılır.

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


## Ekli Posta
'MailMessage', 'SmtpClient' sınıfı kullanılarak daha fazla gönderilebilen posta mesajını temsil eder. Posta mesajına birkaç ek (dosya) eklenebilir.
    
    using System.Net.Mail;

    using(MailMessage myMail = new MailMessage())
    {
         Attachment attachment = new Attachment(path);
         myMail.Attachments.Add(attachment);

         // further processing to send the mail message

    }



