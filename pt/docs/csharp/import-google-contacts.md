---
title: "Importar contatos do Google"
slug: "importar-contatos-do-google"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

Os dados dos contatos do usuário serão recebidos no formato JSON, nós os extraímos e por fim fazemos um loop por esses dados e assim obtemos os contatos do google.

## Requisitos
Para importar contatos do Google(Gmail) no aplicativo ASP.NET MVC, primeiro [faça o download de "Configuração da API do Google"][1] Isso concederá as seguintes referências:

    using Google.Contacts;
    using Google.GData.Client;
    using Google.GData.Contacts;
    using Google.GData.Extensions;

Adicione-os ao aplicativo relevante.


[1]: https://code.google.com/archive/p/google-gdata/downloads

## Código fonte no controlador
    using Google.Contacts;
    using Google.GData.Client;
    using Google.GData.Contacts;
    using Google.GData.Extensions;
    using Newtonsoft.Json;
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
    using System.Net;
    using System.Text;
    using System.Web;
    using System.Web.Mvc;
    
    namespace GoogleContactImport.Controllers
    {
        public class HomeController : Controller
        {
            public ActionResult Index()
            {
                return View();
            }
    
            public ActionResult Import()
            {
                string clientId = "";  // here you need to add your google client id
                string redirectUrl = "http://localhost:1713/Home/AddGoogleContacts"; // here your redirect action method NOTE: you need to configure same url in google console
                Response.Redirect("https://accounts.google.com/o/oauth2/auth?redirect_uri=" + redirectUrl + "&amp&response_type=code&amp&client_id=" + clientId + "&amp&scope=https://www.google.com/m8/feeds/&amp;approval_prompt=force&amp;access_type=offline");
                return View();
            }
    
            public ActionResult AddGoogleContacts()
            {
                string code = Request.QueryString["code"];
                if (!string.IsNullOrEmpty(code))
                {
                    var contacts = GetAccessToken().ToArray();
                    if (contacts.Length > 0)
                    {
                      // You will get all contacts here
                        return View("Index",contacts);
                    }
                    else
                    {
                        return RedirectToAction("Index","Home");
                    }
                }
                else
                {
                    return RedirectToAction("Index", "Home");
                }
            }
            public List<GmailContacts> GetAccessToken()
            {
                string code = Request.QueryString["code"];
                string google_client_id = ""; //your google client Id
                string google_client_sceret = ""; // your google secret key
                string google_redirect_url = "http://localhost:1713/MyContact/AddGoogleContacts";
    
    
                HttpWebRequest webRequest = (HttpWebRequest)WebRequest.Create("https://accounts.google.com/o/oauth2/token");
                webRequest.Method = "POST";
                string parameters = "code=" + code + "&client_id=" + google_client_id + "&client_secret=" + google_client_sceret + "&redirect_uri=" + google_redirect_url + "&grant_type=authorization_code";
                byte[] byteArray = Encoding.UTF8.GetBytes(parameters);
                webRequest.ContentType = "application/x-www-form-urlencoded";
                webRequest.ContentLength = byteArray.Length;
                Stream postStream = webRequest.GetRequestStream();
                // Add the post data to the web request
                postStream.Write(byteArray, 0, byteArray.Length);
                postStream.Close();
                WebResponse response = webRequest.GetResponse();
                postStream = response.GetResponseStream();
                StreamReader reader = new StreamReader(postStream);
                string responseFromServer = reader.ReadToEnd();
                GooglePlusAccessToken serStatus = JsonConvert.DeserializeObject<GooglePlusAccessToken>(responseFromServer);
                /*End*/
                return GetContacts(serStatus);
            }
    
            public List<GmailContacts> GetContacts(GooglePlusAccessToken serStatus)
            {
                string google_client_id = "";  //client id
                string google_client_sceret = ""; //secret key
                /*Get Google Contacts From Access Token and Refresh Token*/
                // string refreshToken = serStatus.refresh_token;
                string accessToken = serStatus.access_token;
                string scopes = "https://www.google.com/m8/feeds/contacts/default/full/";
                OAuth2Parameters oAuthparameters = new OAuth2Parameters()
                {
                    ClientId = google_client_id,
                    ClientSecret = google_client_sceret,
                    RedirectUri = "http://localhost:1713/Home/AddGoogleContacts",
                    Scope = scopes,
                    AccessToken = accessToken,
                    //  RefreshToken = refreshToken
                };
    
                RequestSettings settings = new RequestSettings("App Name", oAuthparameters);
                ContactsRequest cr = new ContactsRequest(settings);
                ContactsQuery query = new ContactsQuery(ContactsQuery.CreateContactsUri("default"));
                query.NumberToRetrieve = 5000;
                Feed<Contact> ContactList = cr.GetContacts();
    
                List<GmailContacts> olist = new List<GmailContacts>();
                foreach (Contact contact in ContactList.Entries)
                {
                    foreach (EMail email in contact.Emails)
                    {
                        GmailContacts gc = new GmailContacts();
                        gc.EmailID = email.Address;
                        var a = contact.Name.FullName;
                        olist.Add(gc);
                    }
                }
                return olist;
            }
    
            public class GmailContacts
            {
                public string EmailID
                {
                    get { return _EmailID; }
                    set { _EmailID = value; }
                }
                private string _EmailID;
            }
    
            public class GooglePlusAccessToken
            {
    
                public GooglePlusAccessToken()
                { }
    
    
                public string access_token
                {
                    get { return _access_token; }
                    set { _access_token = value; }
                }
                private string _access_token;
    
                public string token_type
                {
                    get { return _token_type; }
                    set { _token_type = value; }
                }
                private string _token_type;
    
                public string expires_in
                {
                    get { return _expires_in; }
                    set { _expires_in = value; }
                }
                private string _expires_in;
    
            }
        }
    }

## Código-fonte na visualização.
O único método de ação que você precisa adicionar é adicionar um link de ação presente abaixo

    <a href='@Url.Action("Import", "Home")'>Import Google Contacts</a>



