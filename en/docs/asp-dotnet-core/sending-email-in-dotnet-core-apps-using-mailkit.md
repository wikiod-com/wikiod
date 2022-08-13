---
title: "Sending Email in .Net Core apps using MailKit"
slug: "sending-email-in-net-core-apps-using-mailkit"
draft: false
images: []
weight: 9926
type: docs
toc: true
---

Currently .Net Core does not include support to send emails like `System.Net.Mail` from .Net. [MailKit project](https://github.com/jstedfast/MailKit) (which is available on [nuget](https://www.nuget.org/packages/MailKit/)) is a nice library for this purpose.

## Simple implementation for sending emails

    using MailKit.Net.Smtp;
    using MimeKit;
    using MimeKit.Text;
    using System.Threading.Tasks;
    
    namespace Project.Services
    {
        /// Using a static class to store sensitive credentials
        /// for simplicity. Ideally these should be stored in
        /// configuration files
        public static class Constants
        {
            public static string SenderName => "<sender_name>";
            public static string SenderEmail => "<sender_email>";
            public static string EmailPassword => "email_password";
            public static string SmtpHost => "<smtp_host>";
            public static int SmtpPort => "smtp_port";
        }
        public class EmailService : IEmailSender
        {
            public Task SendEmailAsync(string recipientEmail, string subject, string message)
            {
                MimeMessage mimeMessage = new MimeMessage();
                mimeMessage.From.Add(new MailboxAddress(Constants.SenderName, Constants.SenderEmail));
                mimeMessage.To.Add(new MailboxAddress("", recipientEmail));
                mimeMessage.Subject = subject;
    
                mimeMessage.Body = new TextPart(TextFormat.Html)
                {
                    Text = message,
                };
    
                using (var client = new SmtpClient())
                {
    
                    client.ServerCertificateValidationCallback = (s, c, h, e) => true;
    
                    client.Connect(Constants.SmtpHost, Constants.SmtpPort, false);    
    
                    client.AuthenticationMechanisms.Remove("XOAUTH2");
    
                    // Note: only needed if the SMTP server requires authentication
                    client.Authenticate(Constants.SenderEmail, Constants.EmailPassword);
    
                    client.Send(mimeMessage);
    
                    client.Disconnect(true);
                    return Task.FromResult(0);
                }
            }        
        }       
        
    }



## Installing nuget package
    Install-Package MailKit

