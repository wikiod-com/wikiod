---
title: "ASP.NET Kimliği"
slug: "aspnet-kimligi"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

Kullanıcı yönetimi, rol yönetimi, belirteç oluşturma ve daha fazlası gibi asp.net Kimliği ile ilgili öğreticiler.

## Kullanıcı yöneticisini kullanarak asp.net kimliğinde parola sıfırlama belirteci nasıl uygulanır.
1. Sınıflarım adlı yeni bir klasör oluşturun ve aşağıdaki sınıfı oluşturun ve ekleyin
    
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

2. Kimlik Sınıfınızı Yapılandırın

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

3. Web.config'e kimlik bilgilerinizi ekleyin. Bu kısımda gmail kullanmadım çünkü iş yerimde gmail kullanımı engelleniyor ve hala mükemmel çalışıyor.

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

4. Hesap Denetleyicinizde gerekli değişiklikleri yapın. Aşağıdaki vurgulanan kodu ekleyin.

[![Önce şunu yapın][1]][1]

[![Sonra Bu][2]][2]
    


[1]: https://i.stack.imgur.com/mJz6k.jpg
[2]: https://i.stack.imgur.com/S8jvL.jpg

Derleyin ve çalıştırın. Şerefe!

