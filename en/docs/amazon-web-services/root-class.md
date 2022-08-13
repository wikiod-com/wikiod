---
title: "Root Class"
slug: "root-class"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Amazon api root class is as following.
        public class AmazonRootobject
        {
            public Itemsearchresponse ItemSearchResponse { get; set; }
        }

        public class Itemsearchresponse
        {
            public string xmlns { get; set; }
            public Operationrequest OperationRequest { get; set; }
            public Items Items { get; set; }
        }

        public class Operationrequest
        {
            public Httpheaders HTTPHeaders { get; set; }
            public string RequestId { get; set; }
            public Arguments Arguments { get; set; }
            public string RequestProcessingTime { get; set; }
        }

        public class Httpheaders
        {
            public Header Header { get; set; }
        }

        public class Header
        {
            public string Name { get; set; }
            public string Value { get; set; }
        }

        public class Arguments
        {
            public Argument[] Argument { get; set; }
        }

        public class Argument
        {
            public string Name { get; set; }
            public object Value { get; set; }
        }

        public class Items
        {
            public Request Request { get; set; }
            public string TotalResults { get; set; }
            public string TotalPages { get; set; }
            public string MoreSearchResultsUrl { get; set; }
            public Item[] Item { get; set; }
        }

        public class Request
        {
            public string IsValid { get; set; }
            public Itemsearchrequest ItemSearchRequest { get; set; }
        }

        public class Itemsearchrequest
        {
            public string Keywords { get; set; }
            public string[] ResponseGroup { get; set; }
            public string SearchIndex { get; set; }
            public string Sort { get; set; }
        }

        public class Item
        {
            public string ASIN { get; set; }
            public string ParentASIN { get; set; }
            public string DetailPageURL { get; set; }
            public Itemlinks ItemLinks { get; set; }
            public Smallimage SmallImage { get; set; }
            public Mediumimage MediumImage { get; set; }
            public Largeimage LargeImage { get; set; }
            public Imagesets ImageSets { get; set; }
            public Itemattributes ItemAttributes { get; set; }
            public OfferSummary OfferSummary { get; set; }
            public Offers Offers { get; set; }

            public Variationsummary VariationSummary { get; set; }
        }

        public class Variationsummary
        {
            public Highestprice HighestPrice { get; set; }
            public Lowestprice LowestPrice { get; set; }
            public Highestsaleprice HighestSalePrice { get; set; }
            public Lowestsaleprice LowestSalePrice { get; set; }
        }

        public class Highestprice
        {
            public string Amount { get; set; }
            public string CurrencyCode { get; set; }
            public string FormattedPrice { get; set; }
        }

        public class Lowestprice
        {
            public string Amount { get; set; }
            public string CurrencyCode { get; set; }
            public string FormattedPrice { get; set; }
        }

        public class Highestsaleprice
        {
            public string Amount { get; set; }
            public string CurrencyCode { get; set; }
            public string FormattedPrice { get; set; }
        }

        public class Lowestsaleprice
        {
            public string Amount { get; set; }
            public string CurrencyCode { get; set; }
            public string FormattedPrice { get; set; }
        }

        public class Itemlinks
        {
            public Itemlink[] ItemLink { get; set; }
        }

        public class Itemlink
        {
            public string Description { get; set; }
            public string URL { get; set; }
        }

        public class Smallimage
        {
            public string URL { get; set; }
            public Height Height { get; set; }
            public Width Width { get; set; }
        }

        public class Height
        {
            public string Units { get; set; }
            public string text { get; set; }
        }

        public class Width
        {
            public string Units { get; set; }
            public string text { get; set; }
        }

        public class Mediumimage
        {
            public string URL { get; set; }
            public Height1 Height { get; set; }
            public Width1 Width { get; set; }
        }

        public class Height1
        {
            public string Units { get; set; }
            public string text { get; set; }
        }

        public class Width1
        {
            public string Units { get; set; }
            public string text { get; set; }
        }

        public class Largeimage
        {
            public string URL { get; set; }
            public Height2 Height { get; set; }
            public Width2 Width { get; set; }
        }

        public class Height2
        {
            public string Units { get; set; }
            public string text { get; set; }
        }

        public class Width2
        {
            public string Units { get; set; }
            public string text { get; set; }
        }

        public class Imagesets
        {
            public object ImageSet { get; set; }
        }

        public class Itemattributes
        {
            public string Binding { get; set; }
            public string Brand { get; set; }
            public string Color { get; set; }
            public string Model { get; set; }
            public string Manufacturer { get; set; }
            public string ProductGroup { get; set; }
            public string Title { get; set; }
            public ListPrice ListPrice { get; set; }
        }

        public class ListPrice
        {
            public string Amount { get; set; }
            public string CurrencyCode { get; set; }
            public string FormattedPrice { get; set; }
        }

        public class OfferSummary
        {
            public Lowestnewprice LowestNewPrice { get; set; }
            public Lowestusedprice LowestUsedPrice { get; set; }
            public string TotalNew { get; set; }
            public string TotalUsed { get; set; }
            public string TotalCollectible { get; set; }
            public string TotalRefurbished { get; set; }
            public Lowestrefurbishedprice LowestRefurbishedPrice { get; set; }
        }

        public class Lowestnewprice
        {
            public string Amount { get; set; }
            public string CurrencyCode { get; set; }
            public string FormattedPrice { get; set; }
        }

        public class Lowestusedprice
        {
            public string Amount { get; set; }
            public string CurrencyCode { get; set; }
            public string FormattedPrice { get; set; }
        }

        public class Lowestrefurbishedprice
        {
            public string Amount { get; set; }
            public string CurrencyCode { get; set; }
            public string FormattedPrice { get; set; }
        }

        public class Offers
        {
            public string TotalOffers { get; set; }
            public string TotalOfferPages { get; set; }
            public string MoreOffersUrl { get; set; }
            public Offer Offer { get; set; }
        }

        public class Offer
        {
            public Merchant Merchant { get; set; }
            public Offerattributes OfferAttributes { get; set; }
            public Offerlisting OfferListing { get; set; }
        }

        public class Merchant
        {
            public string Name { get; set; }
        }

        public class Offerattributes
        {
            public string Condition { get; set; }
        }

        public class Offerlisting
        {
            public string OfferListingId { get; set; }
            public string PricePerUnit { get; set; }
            public Price Price { get; set; }
            public string Availability { get; set; }
            public Availabilityattributes AvailabilityAttributes { get; set; }
            public string IsEligibleForSuperSaverShipping { get; set; }
            public string IsEligibleForPrime { get; set; }
            public Saleprice SalePrice { get; set; }
            public Amountsaved AmountSaved { get; set; }
            public string PercentageSaved { get; set; }
        }

        public class Price
        {
            public string Amount { get; set; }
            public string CurrencyCode { get; set; }
            public string FormattedPrice { get; set; }
        }

        public class Availabilityattributes
        {
            public string AvailabilityType { get; set; }
            public string MinimumHours { get; set; }
            public string MaximumHours { get; set; }
        }

        public class Saleprice
        {
            public string Amount { get; set; }
            public string CurrencyCode { get; set; }
            public string FormattedPrice { get; set; }
        }

        public class Amountsaved
        {
            public string Amount { get; set; }
            public string CurrencyCode { get; set; }
            public string FormattedPrice { get; set; }
        }

## Business class
        
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Reflection;
    using System.Text;
    using System.Threading.Tasks;
    using System.Xml;
    using System.Xml.Linq;
    using ApplicationDataServices.SBEntityBox;
    
    namespace ApplicationManagementLayer.Affiliate
    {
        public class Amazon
        {
            private int ItemPage { get; set; }
            public int TotalNumberOfItem { get; set; }

    public Amazon()
    {
        ItemPage = 1;
        TotalNumberOfItem = 0;
    }

     
    string XMLURL = string.Empty;

  

     public async Task<AmazonRootobject> getProductsByKeywords(string q, Dictionary<string, string> CategoryNames, int ItemPage, string categorynamebyid)
    {
        try
        {
            AWSSignedRequestHelper helper = new AWSSignedRequestHelper("para1", "para2", "webservices.amazon.in", "para3");
            IDictionary<string, string> r1 = new Dictionary<string, String>();
            r1["Service"] = "AWSECommerceService";

            r1["Operation"] = "ItemSearch";


            if (CategoryNames != null && CategoryNames.Any() && CategoryNames.Where(o => o.Key.Equals("AmazonCategoryName")).Any())
                r1["SearchIndex"] = CategoryNames.Where(o => o.Key.Equals("AmazonCategoryName")).First().Value;
            else
                r1["SearchIndex"] = "All";

            if (!r1["SearchIndex"].Equals("All") && CategoryNames != null && CategoryNames.Any() && CategoryNames.Where(o => o.Key.Equals("AmazonReferenceCategoryId")).Any())
                r1["BrowseNode"] = CategoryNames.Where(o => o.Key.Equals("AmazonReferenceCategoryId")).First().Value;

            if (!string.IsNullOrEmpty(q))
                r1["Keywords"] = q;
            else if (!string.IsNullOrEmpty(categorynamebyid))
                r1["Keywords"] = categorynamebyid;
            else if (CategoryNames != null && CategoryNames.Any() && CategoryNames.Where(o => o.Key.Equals("AmazonCategoryName")).Any())
                r1["Keywords"] = CategoryNames.Where(o => o.Key.Equals("AmazonCategoryName")).First().Value;
            else
                return null;

            r1["ResponseGroup"] = "Images,ItemAttributes,OfferFull,Offers,Variations";
            r1["Version"] = "2013-08-01";
            r1["ItemPage"] = ItemPage.ToString();
            //r1["Sort"] = "salesrank";

            string strRequestUrl = helper.Sign(r1);

            string output = null;
            using (System.Net.Http.HttpClient wc = new System.Net.Http.HttpClient())
            {
                var request = new System.Net.Http.HttpRequestMessage()
                {
                    RequestUri = new Uri(strRequestUrl),
                    Method = System.Net.Http.HttpMethod.Get,
                };

                /*var task =*/
                await wc.SendAsync(request)
                    .ContinueWith((taskwithmsg) =>
                    {
                        var response = taskwithmsg.Result;

                        var jsonTask = response.Content.ReadAsStringAsync();
                        jsonTask.Wait();
                        output = jsonTask.Result;
                    });
                //task.Wait();

            }

            XmlDocument doc = new XmlDocument();
            doc.LoadXml(output);
            string outputJson = XmlToJSON(doc);

            var pro = new System.Web.Script.Serialization.JavaScriptSerializer().Deserialize<AmazonRootobject>(outputJson);
            TotalNumberOfItem = !string.IsNullOrEmpty(pro.ItemSearchResponse.Items.TotalResults) ? Convert.ToInt32(pro.ItemSearchResponse.Items.TotalResults) : 0;
            return pro;
            //return "";
        }
        catch
        {
            return null;
        }
    }

    https://www.wikiod.com/amazon-web-services
     


        }
    }



