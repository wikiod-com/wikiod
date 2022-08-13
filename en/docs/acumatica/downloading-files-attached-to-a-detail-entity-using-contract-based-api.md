---
title: "Downloading Files Attached to a Detail Entity Using Contract-Based API"
slug: "downloading-files-attached-to-a-detail-entity-using-contract-based-api"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

This topic will demonstrate how to download files attached to a detail entity inside Acumatica ERP by using the Contract-Based API.

The code snippet above was created using the [Json.NET framework][1] (**Newtonsoft.Json.dll**).

To obtain HTTP cookie header from a SOAP response, add a reference to the .Net framework **System.ServiceModel** and **System.ServiceModel.Web** assemblies and the following 2 using directives in your code file:

    using System.ServiceModel;
    using System.ServiceModel.Web;


  [1]: http://www.newtonsoft.com/json

## HTTP Cookie Header from a SOAP Response Shared by SOAP and REST Clients
There is a limitation in Acumatica's SOAP Contract-Based API allowing to download attachments only for a top-level entity. Any attempt to use the [GetFiles()][1] method to get the attachments of a detail entity will, unfortunately, result in the error "**Entity without screen binding cannot be used as top level entity.**" telling us it can only be used with a top-level entity defined in the web service endpoint.

Another limitation with the [GetFiles()][1] method is that it always returns the content of all files attached to an entity. There is no option to first retrieve only file names and then decide what particular file(s) to download from Acumatica.

Thankfully, there is a better and more controllable way to work with attachments provided with the Contract-Based REST API. The `files` array returned as part of every entity exported by the Contract-Based REST API contains only:

 - file names (the **filename** property)
 - file identifiers (the **id** property)
 - hypertext references (the **href** property), which can be used later to download file content

For an example of obtaining a list of files attached to any entity from the web service endpoint and retrieving particular file content though the Contract-Based REST API, please check [Acumatica Product Help][2]

How can one download the files attached to a detail entity if the entire integration project was developed with the SOAP Contract-Based API? As shown in the code snippet below, it is possible to pass HTTP cookie header from a SOAP response into the REST API client  exclusively used to work with the attachments:

    using (var soapClient = new DefaultSoapClient())
    {
        var address = new Uri("http://localhost/AcumaticaERP/entity/Default/6.00.001/");
        CookieContainer cookieContainer;
        using (new OperationContextScope(soapClient.InnerChannel))
        {
            soapClient.Login(login, password, null, null, null);
            string sharedCookie = WebOperationContext.Current.IncomingResponse.Headers["Set-Cookie"];
            cookieContainer = new CookieContainer();
            cookieContainer.SetCookies(address, sharedCookie);
        }
        try
        {
            var shipment = new Shipment()
            {
                ShipmentNbr = new StringSearch { Value = "001301" },
                ReturnBehavior = ReturnBehavior.OnlySpecified
            };
            shipment = soapClient.Get(shipment) as Shipment;

            var restClient = new HttpClient(
                new HttpClientHandler
                {
                    UseCookies = true,
                    CookieContainer = cookieContainer
                });
            restClient.BaseAddress = address;// new Uri("http://localhost/059678/entity/Default/6.00.001/");

            var res = restClient.GetAsync("Shipment/" + shipment.ID + "?$expand=Packages")
                .Result.EnsureSuccessStatusCode();
            var shipmentWithPackages = res.Content.ReadAsStringAsync().Result;

            JObject jShipment = JObject.Parse(shipmentWithPackages);
            JArray jPackages = jShipment.Value<JArray>("Packages");
            foreach (var jPackage in jPackages)
            {
                JArray jFiles = jPackage.Value<JArray>("files");
                string outputDirectory = ".\\Output\\";
                if (!Directory.Exists(outputDirectory))
                {
                    Directory.CreateDirectory(outputDirectory);
                }

                foreach (var jFile in jFiles)
                {
                    string fullFileName = jFile.Value<string>("filename");
                    string fileName = Path.GetFileName(fullFileName);
                    string href = jFile.Value<string>("href");

                    res = restClient.GetAsync(href).Result.EnsureSuccessStatusCode();
                    byte[] file = res.Content.ReadAsByteArrayAsync().Result;
                    System.IO.File.WriteAllBytes(outputDirectory + fileName, file);
                }
            }
        }
        finally
        {
            soapClient.Logout();
        }
    }

  [1]: https://help.acumatica.com/?ScreenId=ShowWiki&pageid=f8b87bde-8af9-48ef-9474-56adabfa1e5e
  [2]: https://help.acumatica.com/?ScreenId=ShowWiki&pageid=b1bc82ee-ae6b-442a-a369-863d98f14630

