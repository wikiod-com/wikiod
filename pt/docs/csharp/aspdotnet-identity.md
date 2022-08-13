---
title: "Identidade ASP.NET"
slug: "identidade-aspnet"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

Tutoriais sobre identidade asp.net, como gerenciamento de usuários, gerenciamento de funções, criação de tokens e muito mais.

## Como implementar o token de redefinição de senha na identidade asp.net usando o gerenciador de usuários.
1. Crie uma nova pasta chamada MyClasses e crie e adicione a seguinte classe
    
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

2. Configure sua classe de identidade

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

3. Adicione suas credenciais ao web.config. Eu não usei o gmail nesta parte porque o uso do gmail está bloqueado no meu local de trabalho e ainda funciona perfeitamente.

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

4. Faça as alterações necessárias no seu Controlador de Conta. Adicione o seguinte código destacado.

[![Primeiro faça isso][1]][1]

[![Então isso][2]][2]
    


[1]: https://i.stack.imgur.com/mJz6k.jpg
[2]: https://i.stack.imgur.com/S8jvL.jpg

Compile e execute. Felicidades!

