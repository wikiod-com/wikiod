---
title: "Microsoft.Exchange.WebHizmetler"
slug: "microsoftexchangewebhizmetler"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Belirtilen Kullanıcının Ofis Dışında Ayarlarını Al
Önce yapıcının bizim için servislere bağlanacağı bir `ExchangeManager` nesnesi oluşturalım. Ayrıca, belirtilen e-posta adresi için "OofSettings" nesnesini döndürecek bir "GetOofSettings" yöntemine sahiptir:


    using System;
    using System.Web.Configuration;
    using Microsoft.Exchange.WebServices.Data;

    namespace SetOutOfOffice
    {
        class ExchangeManager
        {
            private ExchangeService Service;

            public ExchangeManager()
            {
                var password = WebConfigurationManager.ConnectionStrings["Password"].ConnectionString;
                Connect("exchangeadmin", password);
            }
            private void Connect(string username, string password)
            {
                var service = new ExchangeService(ExchangeVersion.Exchange2010_SP2);
                service.Credentials = new WebCredentials(username, password);
                service.AutodiscoverUrl("autodiscoveremail@domain.com" , RedirectionUrlValidationCallback);
                
                Service = service;
            }
            private static bool RedirectionUrlValidationCallback(string redirectionUrl)
            {
                return redirectionUrl.Equals("https://mail.domain.com/autodiscover/autodiscover.xml");
            }    
            public OofSettings GetOofSettings(string email)
            {
                return Service.GetUserOofSettings(email);
            }            
        }
    }

Şimdi bunu başka bir yerde şöyle adlandırabiliriz:

    var em = new ExchangeManager();
    var oofSettings = em.GetOofSettings("testemail@domain.com");

## Belirli Kullanıcının Ofis Dışında Ayarlarını Güncelleme
Aşağıdaki sınıfı kullanarak Exchange'e bağlanabilir ve ardından `UpdateUserOof` ile belirli bir kullanıcının ofis dışı ayarlarını belirleyebiliriz:

    using System;
    using System.Web.Configuration;
    using Microsoft.Exchange.WebServices.Data;
    
    class ExchangeManager
    {
        private ExchangeService Service;

        public ExchangeManager()
        {
            var password = WebConfigurationManager.ConnectionStrings["Password"].ConnectionString;
            Connect("exchangeadmin", password);
        }
        private void Connect(string username, string password)
        {
            var service = new ExchangeService(ExchangeVersion.Exchange2010_SP2);
            service.Credentials = new WebCredentials(username, password);
            service.AutodiscoverUrl("autodiscoveremail@domain.com" , RedirectionUrlValidationCallback);
            
            Service = service;
        }
        private static bool RedirectionUrlValidationCallback(string redirectionUrl)
        {
            return redirectionUrl.Equals("https://mail.domain.com/autodiscover/autodiscover.xml");
        }    
        /// <summary>
        /// Updates the given user's Oof settings with the given details
        /// </summary>
        public void UpdateUserOof(int oofstate, DateTime starttime, DateTime endtime, int externalaudience, string internalmsg, string externalmsg, string emailaddress)
        {
            var newSettings = new OofSettings
            {
                State = (OofState)oofstate,
                Duration = new TimeWindow(starttime, endtime),
                ExternalAudience = (OofExternalAudience)externalaudience,
                InternalReply = internalmsg,
                ExternalReply = externalmsg
            };

            Service.SetUserOofSettings(emailaddress, newSettings);
        }     
    }
    
Kullanıcı ayarlarını aşağıdakilerle güncelleyin:
    
    var oofState = 1;
    var startDate = new DateTime(01,08,2016);
    var endDate = new DateTime(15,08,2016);
    var externalAudience = 1;
    var internalMessage = "I am not in the office!";
    var externalMessage = "I am not in the office <strong>and neither are you!</strong>"
    var theUser = "theuser@domain.com";
    
    var em = new ExchangeManager();
    em.UpdateUserOof(oofstate, startDate, endDate, externalAudience, internalMessage, externalMessage, theUser);
    
Mesajları standart "html" etiketlerini kullanarak biçimlendirebileceğinizi unutmayın.

