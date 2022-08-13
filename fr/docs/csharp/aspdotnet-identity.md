---
title: "Identité ASP.NET"
slug: "identite-aspnet"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

Tutoriels concernant asp.net Identity tels que la gestion des utilisateurs, la gestion des rôles, la création de jetons et plus encore.

## Comment implémenter un jeton de réinitialisation de mot de passe dans l'identité asp.net à l'aide du gestionnaire d'utilisateurs.
1. Créez un nouveau dossier appelé MyClasses et créez et ajoutez la classe suivante
    
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

2. Configurez votre classe d'identité

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

3. Ajoutez vos informations d'identification au fichier web.config. Je n'ai pas utilisé gmail dans cette partie car l'utilisation de gmail est bloquée sur mon lieu de travail et cela fonctionne toujours parfaitement.

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

4. Apportez les modifications nécessaires à votre contrôleur de compte. Ajoutez le code en surbrillance suivant.

[![Commencez par ceci][1]][1]

[![Alors ça][2]][2]
    


[1] : https://i.stack.imgur.com/mJz6k.jpg
[2] : https://i.stack.imgur.com/S8jvL.jpg

Compilez puis exécutez. Acclamations!

