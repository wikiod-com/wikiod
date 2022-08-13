---
title: "Exporting Records via REST Contract-Based API"
slug: "exporting-records-via-rest-contract-based-api"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

This topic will demonstrate how to export records from Acumatica ERP via the REST Contract-Based API. In contrast to the Screen-Based API of Acumatica ERP, the Contract-Based API provides both SOAP and REST interfaces. For more information on the Contract-Based API, see [Acumatica ERP Documentation][1]

  [1]: https://docref.acumatica.com/wiki/ShowWiki.aspx?pageid=91dda8ed-5e92-48a5-a176-9a255506d0d6

To communicate with the REST Contract-Based API of Acumatica ERP your client application must always perform the following 3 steps:

 1. log into Acumatica ERP instance and get cookie with user session information

 2. interact with one of Contract-Based API endpoints available on Acumatica ERP instance

 3. log out from Acumatica ERP to close user session

All samples provided in this topic were built with the ***Default*** endpoint, always deployed as part of the standard Acumatica ERP installation process. On the **Web Service Endpoints** screen (SM.20.70.60) you can view the details of existing endpoints or configure your custom endpoints of the Acumatica ERP contract-based web services:

[![enter image description here][1]][1]

For your reference, below is implementation of the **RestService** class used in all samples above to interact with the Contract-Based web service of Acumatica ERP:

    public class RestService : IDisposable
    {
        private readonly HttpClient _httpClient;
        private readonly string _acumaticaBaseUrl;
        private readonly string _acumaticaEndpointUrl;

        public RestService(string acumaticaBaseUrl, string endpoint,
            string userName, string password,
            string company, string branch)
        {
            _acumaticaBaseUrl = acumaticaBaseUrl;
            _acumaticaEndpointUrl = _acumaticaBaseUrl + "/entity/" + endpoint + "/";
            _httpClient = new HttpClient(
                new HttpClientHandler
                {
                    UseCookies = true,
                    CookieContainer = new CookieContainer()
                })
            {
                BaseAddress = new Uri(_acumaticaEndpointUrl),
                DefaultRequestHeaders =
                {
                    Accept = {MediaTypeWithQualityHeaderValue.Parse("text/json")}
                }
            };

            var str = new StringContent(
                new JavaScriptSerializer()
                    .Serialize(
                        new
                        {
                            name = userName,
                            password = password,
                            company = company,
                            branch = branch
                        }),
                        Encoding.UTF8, "application/json");

            _httpClient.PostAsync(acumaticaBaseUrl + "/entity/auth/login", str)
                .Result.EnsureSuccessStatusCode();
        }

        void IDisposable.Dispose()
        {
            _httpClient.PostAsync(_acumaticaBaseUrl + "/entity/auth/logout",
                new ByteArrayContent(new byte[0])).Wait();
            _httpClient.Dispose();
        }

        public string GetList(string entityName)
        {
            var res = _httpClient.GetAsync(_acumaticaEndpointUrl + entityName)
                .Result.EnsureSuccessStatusCode();

            return res.Content.ReadAsStringAsync().Result;
        }

        public string GetList(string entityName, string parameters)
        {
            var res = _httpClient.GetAsync(_acumaticaEndpointUrl + entityName + "?" + parameters)
                .Result.EnsureSuccessStatusCode();

            return res.Content.ReadAsStringAsync().Result;
        }
    }


  [1]: https://i.stack.imgur.com/T1UFJ.png

## Data Export in a Single REST Call
In this example you will explore how to export the following data from Acumatica ERP in a single call via the REST Contract-Based API:

 - all stock items existing in the application
 - all sales order of the IN type

If you need to export records from Acumatica ERP, use the following URL: 
`http://<Acumatica ERP instance URL>/entity/<Endpoint name>/<Endpoint version>/<Top-level entity>`

`<Top-level entity>` is the name of the entity which you are going to export

# To export all stock items in a single REST call: # 

To export stock item records from a local `AcumaticaERP` instance by using the ***Default*** endpoint of version ***6.00.001***, you should use the following URL: `http://localhost/AcumaticaERP/entity/Default/6.00.001/StockItem`

Below is the sample code written in C# to export all stock items by sending a single REST call to the ***Default*** endpoint of version ***6.00.001***:

    using (RestService rs = new RestService(
        @"http://localhost/AcumaticaERP/", "Default/6.00.001",
        username, password, company, branch))
    {
        string stockItems = rs.GetList("StockItem");
    }

# To export all sales order of the IN type in a single REST call: # 

To export sales orders of the ***IN*** type from a local `AcumaticaERP` instance by using the ***Default*** endpoint of version ***6.00.001***, you should use the following URL: `http://localhost/AcumaticaERP/entity/Default/6.00.001/SalesOrder?$filter=OrderType eq 'IN'`

Below is the sample code written in C# to export all sales orders of the ***IN*** type by sending a single REST call to the ***Default*** endpoint of version ***6.00.001***:

    using (RestService rs = new RestService(
        @"http://localhost/StackOverflow/", "Default/6.00.001",
        username, password, company, branch))
    {
        var parameters = "$filter=OrderType eq 'IN'";
        string inSalesOrders = rs.GetList("SalesOrder", parameters);
    }

## Implementing Pagination on Multiple REST Requests
In this example you will explore how to export the following data from Acumatica ERP in batches via the REST Contract-Based API:

 - stock items existing in the application in batches of 10 records
 - all sales orders in batches of 100 records

# To export stock items in batches of 10 records with multiple REST calls: # 

To export first 10 stock items from a local `AcumaticaERP` instance by using the ***Default*** endpoint of version ***6.00.001***, you should use the following URL: `http://localhost/AcumaticaERP/entity/Default/6.00.001/StockItem?$top=10`

Accordingly, to request stock items from 10 to 20, you simply extend the URL above with ***filter*** parameter: `http://localhost/AcumaticaERP/entity/Default/6.00.001/StockItem?$top=10&$filter=InventoryID gt '<InventoryID>'`

`<InventoryID>` is the ID of the last stock item exported with a previous REST call

Below is the sample code written in C# to export all stock items in batches of 10 records by sending multiple REST calls to the ***Default*** endpoint of version ***6.00.001***:

    using (RestService rs = new RestService(
        @"http://localhost/StackOverflow/", "Default/6.00.001",
        username, password, company, branch))
    {
        var json = new JavaScriptSerializer();
        string parameters = "$top=10";
        string items = rs.GetList("StockItem", parameters);
        var records = json.Deserialize<List<Dictionary<string, object>>>(items);

        while (records.Count == 10)
        {
            var inventoryID = records[records.Count - 1]["InventoryID"] as Dictionary<string, object>;
            var inventoryIDValue = inventoryID.Values.First();
            string nextParameters = parameters + "&" + 
                "$filter=" + string.Format("InventoryID gt '{0}'", inventoryIDValue);
            items = rs.GetList("StockItem", nextParameters);
            records = json.Deserialize<List<Dictionary<string, object>>>(items);
        }
    }

# To export all sales orders in batches of 100 records with multiple REST calls: # 

To export first 100 sales orders from a local `AcumaticaERP` instance by using the ***Default*** endpoint of version ***6.00.001***, you should use the following URL: `http://localhost/AcumaticaERP/entity/Default/6.00.001/SalesOrder?$top=100`

Since the primary key of the **Sales Order** entity is composed by the ***Order Type*** and the ***Order Number***, in this example you will be using a combination of **filter** parameters for the ***Order Type*** and ***Order Number*** fields:
 
 - to request sales orders from 100 to 200 of the ***SO*** type, you should use the following URL: `http://localhost/AcumaticaERP/entity/Default/6.00.001/SalesOrder?$top=100&$filter=OrderType eq 'SO' and OrderNbr gt '<OrderNbr>'`

`<OrderNbr>` is the number of the last sales order exported with a previous REST call

 - accordingly, to request first 100 sales orders of the next to ***SO*** type, you should use the following URL: `http://localhost/AcumaticaERP/entity/Default/6.00.001/SalesOrder?$top=100&$filter=OrderType gt 'SO' and OrderNbr gt ''`

Below is the sample code written in C# to export all sales orders in batches of 100 records with multiple REST calls to the ***Default*** endpoint of version ***6.00.001***:

    using (RestService rs = new RestService(
        @"http://localhost/StackOverflow/", "Default/6.00.001",
        username, password, company, branch))
    {
        var json = new JavaScriptSerializer();
        string parameters = "$top=100";
        string items = rs.GetList("SalesOrder", parameters);
        var records = json.Deserialize<List<Dictionary<string, object>>>(items);

        bool sameOrderType = true;
        while (records.Count > 0 && (records.Count == 100 || !sameOrderType))
        {
            var orderType = records[records.Count - 1]["OrderType"] as Dictionary<string, object>;
            var orderTypeValue = orderType.Values.First();
            var orderNbr = records[records.Count - 1]["OrderNbr"] as Dictionary<string, object>;
            var orderNbrValue = orderNbr.Values.First();

            string nextParameters = parameters + "&" + "$filter=" +
                string.Format("OrderType {0} '{1}'", sameOrderType ? "eq" : "gt", orderTypeValue) + " and " +
                string.Format("OrderNbr gt '{0}'", sameOrderType ? orderNbrValue : "''" );
            items = rs.GetList("SalesOrder", nextParameters);
            records = json.Deserialize<List<Dictionary<string, object>>>(items);
            sameOrderType = records.Count == 100;
        }
    }

