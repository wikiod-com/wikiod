---
title: "Identidad de ASP.NET"
slug: "identidad-de-aspnet"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

Tutoriales sobre la identidad de asp.net, como la gestión de usuarios, la gestión de roles, la creación de tokens y más.

## Cómo implementar el token de restablecimiento de contraseña en la identidad de asp.net usando el administrador de usuarios.
1. Cree una nueva carpeta llamada MyClasses y cree y agregue la siguiente clase
    
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

2. Configure su clase de identidad

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

3. Agregue sus credenciales a web.config. No utilicé gmail en esta parte porque el uso de gmail está bloqueado en mi lugar de trabajo y todavía funciona perfectamente.

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

4. Realice los cambios necesarios en su controlador de cuenta. Agregue el siguiente código resaltado.

[![Primero haz esto][1]][1]

[![Entonces esto][2]][2]
    


[1]: https://i.stack.imgur.com/mJz6k.jpg
[2]: https://i.stack.imgur.com/S8jvL.jpg

Compilar y luego ejecutar. ¡Salud!

