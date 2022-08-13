---
title: "Sistema.Net.Mail"
slug: "sistemanetmail"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Es importante desechar un System.Net.MailMessage porque cada uno de los archivos adjuntos contiene un flujo y estos flujos deben liberarse lo antes posible. La declaración de uso garantiza que el objeto Desechable se elimine también en caso de Excepciones

## Mensaje de correo
Aquí está el ejemplo de la creación de un mensaje de correo con archivos adjuntos. Después de crear, enviamos este mensaje con la ayuda de la clase `SmtpClient`. Aquí se utiliza el puerto 25 predeterminado.

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


## Correo con archivo adjunto
`MailMessage` representa un mensaje de correo que se puede enviar más usando la clase `SmtpClient`. Se pueden agregar varios archivos adjuntos (archivos) al mensaje de correo.
    
    using System.Net.Mail;

    using(MailMessage myMail = new MailMessage())
    {
         Attachment attachment = new Attachment(path);
         myMail.Attachments.Add(attachment);

         // further processing to send the mail message

    }



