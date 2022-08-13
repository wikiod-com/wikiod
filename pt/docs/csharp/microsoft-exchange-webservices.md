---
title: "Microsoft.Exchange.WebServices"
slug: "microsoftexchangewebservices"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Recuperar as configurações de ausência temporária do usuário especificado
Primeiro vamos criar um objeto `ExchangeManager`, onde o construtor se conectará aos serviços para nós. Ele também possui um método `GetOofSettings`, que retornará o objeto `OofSettings` para o endereço de e-mail especificado:


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

Agora podemos chamar isso em outro lugar assim:

    var em = new ExchangeManager();
    var oofSettings = em.GetOofSettings("testemail@domain.com");

## Atualizar as configurações de ausência temporária do usuário específico
Usando a classe abaixo, podemos nos conectar ao Exchange e, em seguida, definir as configurações de ausência temporária de um usuário específico com `UpdateUserOof`:

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
    
Atualize as configurações do usuário com o seguinte:
    
    var oofState = 1;
    var startDate = new DateTime(01,08,2016);
    var endDate = new DateTime(15,08,2016);
    var externalAudience = 1;
    var internalMessage = "I am not in the office!";
    var externalMessage = "I am not in the office <strong>and neither are you!</strong>"
    var theUser = "theuser@domain.com";
    
    var em = new ExchangeManager();
    em.UpdateUserOof(oofstate, startDate, endDate, externalAudience, internalMessage, externalMessage, theUser);
    
Observe que você pode formatar as mensagens usando tags `html` padrão.

