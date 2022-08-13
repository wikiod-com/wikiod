---
title: "Microsoft.Exchange.WebServicesMicrosoft.Exchange.WebServices"
slug: "microsoftexchangewebservicesmicrosoftexchangewebservices"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Récupérer les paramètres d'absence du bureau de l'utilisateur spécifié
Commençons par créer un objet `ExchangeManager`, où le constructeur se connectera aux services pour nous. Il a également une méthode `GetOofSettings`, qui renverra l'objet `OofSettings` pour l'adresse e-mail spécifiée :


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

Nous pouvons maintenant appeler cela ailleurs comme ceci :

    var em = new ExchangeManager();
    var oofSettings = em.GetOofSettings("testemail@domain.com");

## Mettre à jour les paramètres d'absence du bureau d'un utilisateur spécifique
En utilisant la classe ci-dessous, nous pouvons nous connecter à Exchange, puis définir les paramètres d'absence du bureau d'un utilisateur spécifique avec `UpdateUserOof` :

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
    
Mettez à jour les paramètres utilisateur avec les éléments suivants :
    
    var oofState = 1;
    var startDate = new DateTime(01,08,2016);
    var endDate = new DateTime(15,08,2016);
    var externalAudience = 1;
    var internalMessage = "I am not in the office!";
    var externalMessage = "I am not in the office <strong>and neither are you!</strong>"
    var theUser = "theuser@domain.com";
    
    var em = new ExchangeManager();
    em.UpdateUserOof(oofstate, startDate, endDate, externalAudience, internalMessage, externalMessage, theUser);
    
Notez que vous pouvez formater les messages en utilisant les balises `html` standard.

