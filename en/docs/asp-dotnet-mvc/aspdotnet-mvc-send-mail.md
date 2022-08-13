---
title: "Asp.net  mvc send mail"
slug: "aspnet--mvc-send-mail"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Contact Form In Asp MVC
**1. Model :**

     public class ContactModel
     {
        [Required, Display(Name="Sender Name")]
        public string SenderName { get; set; }
        [Required, Display(Name = "Sender Email"), EmailAddress]
        public string SenderEmail { get; set; }
        [Required]
        public string Message { get; set; }
     }

**2. Controller :**

    public class HomeController
    {
        [HttpPost]
        [ValidateAntiForgeryToken]
        public async Task<ActionResult> Contact(ContectModel model)
        {
            if (ModelState.IsValid)
            {
                var mail = new MailMessage();
                mail.To.Add(new MailAddress(model.SenderEmail));
                mail.Subject = "Your Email Subject";
                mail.Body = string.Format("<p>Email From: {0} ({1})</p><p>Message:</p><p>{2}</p>", model.SenderName, mail.SenderEmail, model.Message);
                mail.IsBodyHtml = true;
                using (var smtp = new SmtpClient())
                {
                    await smtp.SendMailAsync(mail);
                    return RedirectToAction("SuccessMessage");
                }
            }
            return View(model);
        }
    
        public ActionResult SuccessMessage()
        {
            return View();
        }
    
    }

**3. Web.Config :**

    <system.net>
      <mailSettings>
        <smtp from="you@outlook.com">
          <network host="smtp-mail.outlook.com" 
                   port="587" 
                   userName="you@outlook.com"
                   password="password" 
                   enableSsl="true" />
        </smtp>
      </mailSettings>
    </system.net>

**4. View :**

**Contact.cshtml**

     @model ContectModel
        @using (Html.BeginForm())
        {
            @Html.AntiForgeryToken()
            <h4>Send your comments.</h4>
            <hr />
            <div class="form-group">
                @Html.LabelFor(m => m.SenderName, new { @class = "col-md-2 control-label" })
                <div class="col-md-10">
                    @Html.TextBoxFor(m => m.SenderName, new { @class = "form-control" })
                    @Html.ValidationMessageFor(m => m.SenderName)
                </div>
            </div>
            <div class="form-group">
                @Html.LabelFor(m => m.SenderEmail, new { @class = "col-md-2 control-label" })
                <div class="col-md-10">
                    @Html.TextBoxFor(m => m.SenderEmail, new { @class = "form-control" })
                    @Html.ValidationMessageFor(m => m.SenderEmail)
                </div>
            </div>
            <div class="form-group">
                @Html.LabelFor(m => m.Message, new { @class = "col-md-2 control-label" })
                <div class="col-md-10">
                    @Html.TextAreaFor(m => m.Message, new { @class = "form-control" })
                    @Html.ValidationMessageFor(m => m.Message)
                </div>
            </div>
            <div class="form-group">
                <div class="col-md-offset-2 col-md-10">
                    <input type="submit" class="btn btn-default" value="Send" />
                </div>
            </div>
        }

**SuccessMessage.cshtml** 

    <h2>Your message has been sent</h2>




## Sending Email From Class
This way can be so helpfull, but, some people (like me) are afreak of repeat code, and like you are showin us, it means that I need to create a contact controller with the same code on each proyect that we have, so, I thing that this can be helpfull too


This is my class, that can be on a DLL or whatever 

     public class Emails
        {
            public static void SendHtmlEmail(string receiverEmail, string subject, string body, bool Ssl = false)
            {
                //Those are read it from webconfig or appconfig
                var client = new SmtpClient(ConfigurationManager.AppSettings["MailServer"], Convert.ToInt16
    
                    (ConfigurationManager.AppSettings["MailPort"]))
                {
                    Credentials = new NetworkCredential(ConfigurationManager.AppSettings["MailSender"], ConfigurationManager.AppSettings["MailSenderPassword"]),
                    EnableSsl = Ssl
                };
    
                MailMessage message = new MailMessage();
                message.From = new MailAddress(ConfigurationManager.AppSettings["MailSender"]);
                message.To.Add(receiverEmail);
                // message.To.Add("sgermosen@praysoft.net");
                message.Subject = subject;
                message.IsBodyHtml = true;
                message.Body = body;
                client.Send(message);
            }
    
        }

like you see it will read from the webconfig, so, we need to configured it, this configuration is for Gmail, but, every host have their own configuration

     <appSettings>
        <add key="webpages:Version" value="3.0.0.0" />
        <add key="webpages:Enabled" value="false" />
        <add key="ClientValidationEnabled" value="true" />
        <add key="UnobtrusiveJavaScriptEnabled" value="true" />
        <add key="AdminUser" value="sgrysoft@gmail.com" />
        <add key="AdminPassWord" value="123456789" />
        <add key="SMTPName" value="smtp.gmail.com" />
        <add key="SMTPPort" value="587" />
    
      </appSettings>


 

