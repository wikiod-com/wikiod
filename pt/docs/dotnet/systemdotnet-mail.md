---
title: "System.Net.Mail"
slug: "systemnetmail"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

É importante descartar um System.Net.MailMessage porque cada anexo contém um Stream e esses Streams precisam ser liberados o mais rápido possível. A instrução using garante que o objeto Descartável seja Descartado também em caso de Exceções

##Mensagem de correio
Aqui está o exemplo de criação de mensagem de correio com anexos. Após a criação enviamos esta mensagem com a ajuda da classe `SmtpClient`. A porta padrão 25 é usada aqui.

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


## E-mail com anexo
`MailMessage` representa a mensagem de correio que pode ser enviada usando a classe `SmtpClient`. Vários anexos (arquivos) podem ser adicionados à mensagem de correio.
    
    using System.Net.Mail;

    using(MailMessage myMail = new MailMessage())
    {
         Attachment attachment = new Attachment(path);
         myMail.Attachments.Add(attachment);

         // further processing to send the mail message

    }



