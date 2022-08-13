---
title: "Calling Web Services programmatically using C#.net and Xamarin app development"
slug: "calling-web-services-programmatically-using-cnet-and-xamarin-app-development"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Here we will see Pro-grammatically calling and using web services in ASP.Net C# . For the purpose you will required to download following ddl which provides you many functions.
Download ImportJson from 
https://drive.google.com/open?id=0B-2bGoHKJvnOckdPUHVjdFZTcFU

This article is very useful for those of you who are going to develop a project using ASP.NET C# Web services/ Web API Services. This article is also useful for those who are developing a project using Xamarin: Mobile App Development 

You required to give reference of ImportJson dll and restsharp ddl.
  ImportJson can be downloaded from here
https://drive.google.com/open?id=0B-2bGoHKJvnOckdPUHVjdFZTcFU
And restsharp.dll will get from internet


Any suggestion/contact,  please note **akhandagale65@gmail.com** 

## Calling Simple GET Method
    /// <summary>
        /// Simple Get method
        /// </summary>
        /// <returns> Json formated data </returns>
        public string GetJsonData1()
        {
            IOperations _Obj = ClsOperations.GetOperations();
            string url = "http://1.2.3.4:1234/Services/rest/CallService/WebRequest/"; 
            string jsonResult = _Obj.GetJsonResult(url);
            return jsonResult;
        }


## Calling Web Service with Data POST/ POST Method
            /// <summary>
        /// Post Method with input parameter
        /// </summary>
        /// <returns> Json formated data </returns>
        public string GetJsonData2()
        {
            IOperations _Obj = ClsOperations.GetOperations();
            string url = "http://1.2.3.4:1234/Services/rest/CallService/WebRequest/";
            Dictionary<string, object> objDec = new Dictionary<string, object>();
            objDec.Add("@FirstParameter", "Value1");
            objDec.Add("@SecondParameter", "Value2");
            objDec.Add("@ThirdParameter", "Value3");
            string jsonResult = _Obj.GetJsonResult(url, objDec);
            return jsonResult;
        }



## Calling Web Service with Data POST/ POST Method (Posted Data in JSON Format)
    /// <summary>
        /// Post Method with Input/ data to post in JSON format 
        /// </summary>
        /// <returns> Json formated data </returns>
        public string GetJsonData3()
        {
            IOperations _Obj = ClsOperations.GetOperations();
            string url = "http://1.2.3.4:1234/Services/rest/CallService/WebRequest/";
            string inputjson = "{\"@FirstParameter\": \"Value1\",\"@SecondParameter\": \"Value2\",\"@ThirdParameter\": \"Value3\"}";
            string jsonResult = _Obj.GetJsonResult(url, null,inputjson );
            return jsonResult;
        }


## Web Service call with output As IEnumerator object
    /// <summary>
        /// Post Method with Input/ data to post in JSON format Or you can send dictionary as shown in previous methods
        /// </summary>
        /// <returns> Json formated data </returns>
        public void  GetJsonData4()
        {
            IOperations _Obj = ClsOperations.GetOperations();
            string url = "http://1.2.3.4:1234/Services/rest/CallService/WebRequest/";
            string inputjson = "{\"@FirstParameter\": \"Value1\",\"@SecondParameter\": \"Value2\",\"@ThirdParameter\": \"Value3\"}";
            string jsonResult = _Obj.GetJsonResult(url, null, inputjson);
            IEnumerator objIEnumerator = _Obj.GetJsonEnumerableResult(jsonResult);
            // you can perform further operations on it

        }


## Web Service Output in List format or DataTable Format
        /// <summary>
            /// Post Method with Input/ data to post in JSON format Or you can send dictionary as shown in previous methods
            /// </summary>
            /// <returns> Json formated data </returns>
            public DataTable  GetJsonData6()
            {
                IOperations _Obj = ClsOperations.GetOperations();
                string url = "http://1.2.3.4:1234/Services/rest/CallService/WebRequest/";
                string inputjson = "{\"@FirstParameter\": \"Value1\",\"@SecondParameter\": \"Value2\",\"@ThirdParameter\": \"Value3\"}";
                IEnumerator objIEnumerator = _Obj.GetJsonEnumerableResult(url, null, inputjson);
                // you can perform further operations on it
    
                // If you want to convert it in Datatable / List
    
                List<ClsMyPropertyClass> lst = new List<ClsMyPropertyClass>();
                while (objIEnumerator.MoveNext())
                {
                    lst.Add(Newtonsoft.Json.JsonConvert.DeserializeObject<ClsLineEDoDetails>(objIEnumerator.Current.ToString()));
                } 
    // Upto this you will get List , and you can perform operations on it
    
                // Now if youu want result in datatable, here i written function for List to datatable conversion
                
                return CommonServiceCall.ToDataTable(lst);
    
            }



## Forcefully make method GET OR POST
    /* By Default if you send only url then automatically it will recognize as GET Method and if service having parameters with, Then automatically will convert to POST Method. But I observed some of the services having only URL but are POST Type. For the purpose you can forcefully make the method as you want. As bellow:  */
            /// <summary>
            /// If you want make the service call GET OR POST forcefully then 
            /// </summary>
            /// <returns> Json formated data </returns>
            public void GetJsonData5()
            {
                IOperations _Obj = ClsOperations.GetOperations();
                string url = "http://1.2.3.4:1234/Services/rest/CallService/WebRequest/";
                string inputjson = "{\"@FirstParameter\": \"Value1\",\"@SecondParameter\": \"Value2\",\"@ThirdParameter\": \"Value3\"}";
    string  _result =   _ Obj.GetJsonResult(url, null, inputjson, ServiceType.POST);;
                         }



