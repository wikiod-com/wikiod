---
title: "Getting started with wcf"
slug: "getting-started-with-wcf"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## WCF Restful Service Demo
svc
 

     public class WCFRestfulService : IWCFRestfulService 
        {
           public string GetServiceName(int Id)
            {
                return "This is a WCF Restful Service";
            }
        }

Interface


    [ServiceContract(Name = "WCRestfulService ")]
    public interface IWCFRestfulService 
    {
        [OperationContract]
        [WebInvoke(Method = "GET", ResponseFormat = WebMessageFormat.Json, BodyStyle = WebMessageBodyStyle.Wrapped, UriTemplate = "GetServiceName?Id={Id}")]
        string GetServiceName(int Id);
    }


svc Markup (Right Click on svc file & click view MarkUp)

    <%@ ServiceHost Language="C#" Debug="true" Service="NamespaceName.WCFRestfulService" CodeBehind="WCFRestfulService.svc.cs" %>


Web Config

    <services>
          <service name="NamespaceName.WCFRestfulService" behaviorConfiguration="ServiceBehaviour">
            <endpoint address="" binding="webHttpBinding" contract="NamespaceName.IWCFRestfulService" behaviorConfiguration="web"/>
          </service>
        </services>
        <behaviors>
          <serviceBehaviors>
            <behavior name="ServiceBehaviour">
              <!-- To avoid disclosing metadata information, set the values below to false before deployment -->
              <serviceMetadata httpGetEnabled="true" httpsGetEnabled="true"/>
              <!-- To receive exception details in faults for debugging purposes, set the value below to true.  Set to false before deployment to avoid disclosing exception information -->
              <serviceDebug includeExceptionDetailInFaults="true"/>
            </behavior>
          </serviceBehaviors>
          <endpointBehaviors>
            <behavior name="web">
              <webHttp/>
            </behavior>
          </endpointBehaviors>
        </behaviors>

Now Simply Run the Service or host in a port.
And access the service using "http://hostname/WCFRestfulService/GetServiceName?Id=1"

## Simple WCF service
Minimal requirements for WCF service is one ServiceContract with one OperationContract.

Service contract:

    [ServiceContract]
    public interface IDemoService
    {
        [OperationContract]
        CompositeType SampleMethod();
    } 

Service contract implementation:

    public class DemoService : IDemoService
    {
        public CompositeType SampleMethod()
        {
            return new CompositeType { Value = "foo", Quantity = 3 };
        }
    }

Configuration file:

    <?xml version="1.0"?>
    <configuration>
      <appSettings>
        <add key="aspnet:UseTaskFriendlySynchronizationContext" value="true" />
      </appSettings>
      <system.web>
        <compilation debug="true" targetFramework="4.5.2" />
        <httpRuntime targetFramework="4.5.2"/>
      </system.web>
      <system.serviceModel>
        <behaviors>
          <serviceBehaviors>
            <behavior>
              <serviceMetadata httpGetEnabled="true"/>
              <serviceDebug includeExceptionDetailInFaults="false"/>
            </behavior>
          </serviceBehaviors>
        </behaviors>
        <serviceHostingEnvironment aspNetCompatibilityEnabled="true" multipleSiteBindingsEnabled="true" />
      </system.serviceModel>
      <system.webServer>
        <modules runAllManagedModulesForAllRequests="true"/>
        <directoryBrowse enabled="true"/>
      </system.webServer>
    </configuration>

DTO:

    [DataContract]
    public class CompositeType
    {
        [DataMember]
        public string Value { get; set; }

        [DataMember]
        public int Quantity { get; set; }
    }

