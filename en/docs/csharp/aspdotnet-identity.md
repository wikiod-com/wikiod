---
title: "ASP.NET Identity"
slug: "aspnet-identity"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

Tutorials concerning asp.net Identity such as user management, role management, creating tokens and more.

## How to implement password reset token in asp.net identity using user manager.
 1. Create a new folder called MyClasses and create and add the following class
    
    <pre>public class GmailEmailService:SmtpClient
    {
        // Gmail user-name
        public string UserName { get; set; }

        public GmailEmailService() :
            base(ConfigurationManager.AppSettings["GmailHost"], Int32.Parse(ConfigurationManager.AppSettings["GmailPort"]))
        {
            //Get values from web.config file:
            this.UserName = ConfigurationManager.AppSettings["GmailUserName"];
            this.EnableSsl = Boolean.Parse(ConfigurationManager.AppSettings["GmailSsl"]);
            this.UseDefaultCredentials = false;
            this.Credentials = new System.Net.NetworkCredential(this.UserName, ConfigurationManager.AppSettings["GmailPassword"]);
        }
    }</pre>

 2. Configure your Identity Class

    <pre>public async Task SendAsync(IdentityMessage message)
    {
        MailMessage email = new MailMessage(new MailAddress("youremailadress@domain.com", "(any subject here)"),
        new MailAddress(message.Destination));
        email.Subject = message.Subject;
        email.Body = message.Body;

        email.IsBodyHtml = true;

        GmailEmailService mailClient = new GmailEmailService();
        await mailClient.SendMailAsync(email);
    }</pre>

 3. Add your credentials to the web.config. I did not use gmail in this portion because the use of gmail is blocked in my workplace and it still works perfectly.

    <appSettings>
        <add key="webpages:Version" value="3.0.0.0" />
        <add key="webpages:Enabled" value="false" />
        <add key="ClientValidationEnabled" value="true" />
        <add key="UnobtrusiveJavaScriptEnabled" value="true" />
    
        <add key="GmailUserName" value="youremail@yourdomain.com"/>
        <add key="GmailPassword" value="yourPassword"/>
        <add key="GmailHost" value="yourServer"/>
        <add key="GmailPort" value="yourPort"/>
        <add key="GmailSsl" value="chooseTrueOrFalse"/>
        <!--Smptp Server (confirmations emails)-->
    </appSettings>

 4. Make necessary changes to your Account Controller. Add the following highlighted code.

[![First do this][1]][1]

[![Then This][2]][2]
    


  [1]: https://i.stack.imgur.com/mJz6k.jpg
  [2]: https://i.stack.imgur.com/S8jvL.jpg

Compile then run. Cheers!

