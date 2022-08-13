---
title: "WCF - Http and Https on SOAP and REST"
slug: "wcf---http-and-https-on-soap-and-rest"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Sample web.config
here is a sample `web.config` in order to have both `http` and `https` support.

    <?xml version="1.0" encoding="utf-8"?>
    <configuration>
    
      <appSettings>
        <add key="aspnet:UseTaskFriendlySynchronizationContext" value="true" />
      </appSettings>
    
      <system.web>
        <compilation debug="true" targetFramework="4.5.2" />
        <httpRuntime targetFramework="4.5.2" />
      </system.web>
    
      <system.serviceModel>
        <bindings>
          <basicHttpBinding>
            <binding name="SoapBinding" />
          </basicHttpBinding>
          <basicHttpsBinding>
            <binding name="SecureSoapBinding" />
          </basicHttpsBinding>
    
          <webHttpBinding>
            <binding name="RestBinding" />
            <binding name="SecureRestBinding">
              <security mode="Transport" />
            </binding>
          </webHttpBinding>
    
          <mexHttpBinding>
            <binding name="MexBinding" />
          </mexHttpBinding>
          <mexHttpsBinding>
            <binding name="SecureMexBinding" />
          </mexHttpsBinding>
        </bindings>
    
        <client />
    
        <services>
          <service behaviorConfiguration="ServiceBehavior" name="Interface.Core">
            <endpoint address="soap" binding="basicHttpBinding" bindingConfiguration="SoapBinding" name="Soap" contract="Interface.ICore" />
            <endpoint address="soap" binding="basicHttpsBinding" bindingConfiguration="SecureSoapBinding" name="SecureSoap" contract="Interface.ICore" />
            <endpoint address="" behaviorConfiguration="Web" binding="webHttpBinding" bindingConfiguration="RestBinding" name="Rest" contract="Interface.ICore" />
            <endpoint address="" behaviorConfiguration="Web" binding="webHttpBinding" bindingConfiguration="SecureRestBinding" name="SecureRest" contract="Interface.ICore" />
            <endpoint address="mex" binding="mexHttpBinding" bindingConfiguration="MexBinding" name="Mex" contract="IMetadataExchange" />
            <endpoint address="mex" binding="mexHttpsBinding" bindingConfiguration="SecureMexBinding" name="SecureMex" contract="IMetadataExchange" />
          </service>
        </services>
    
        <behaviors>
          <endpointBehaviors>
            <behavior name="Web">
              <webHttp helpEnabled="true" defaultBodyStyle="Bare" defaultOutgoingResponseFormat="Json" automaticFormatSelectionEnabled="true" />
            </behavior>
          </endpointBehaviors>
    
          <serviceBehaviors>
            <behavior name="ServiceBehavior">
              <serviceMetadata httpGetEnabled="true" httpsGetEnabled="true" />
              <serviceDebug includeExceptionDetailInFaults="true" />
            </behavior>
          </serviceBehaviors>
        </behaviors>
    
        <protocolMapping>
          <add binding="basicHttpsBinding" scheme="https" />
        </protocolMapping>
        <serviceHostingEnvironment aspNetCompatibilityEnabled="true" multipleSiteBindingsEnabled="true" />
      </system.serviceModel>
    
      <system.webServer>
        <modules runAllManagedModulesForAllRequests="true" />
        <directoryBrowse enabled="false" />
      </system.webServer>
    
    </configuration>

