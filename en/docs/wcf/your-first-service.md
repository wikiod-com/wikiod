---
title: "Your first service"
slug: "your-first-service"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Adding a metadata endpoint to your service
SOAP services can publish metadata that describes the methods that may be invoked by clients. Clients can use tools such as *Visual Studio* to automatically generate code (known as _client proxies_).  The proxies hide the complexity of invoking a service.  To invoke a service, one merely invokes a method on a client proxy.

First you must add a metadata endpoint to your service. Assuming your service looks the same as the one defined in the "First service and host" example, you can make the following changes to the configuration file. 

    <system.serviceModel>
      <behaviors>
        <serviceBehaviors>
          <behavior name="serviceBehaviour">
            <serviceMetadata httpGetEnabled="true" />
          </behavior>
        </serviceBehaviors>
      </behaviors>
      <services>
        <service name="Service.Example" behaviorConfiguration="serviceBehaviour">
          <endpoint address="mex" binding="mexHttpBinding" name="mexExampleService" contract="IMetadataExchange" />
          <endpoint name="netTcpExample" contract="Service.IExample" binding="netTcpBinding" />
          <host>
            <baseAddresses>
              <add baseAddress="net.tcp://localhost:9000/Example" />
              <add baseAddress="http://localhost:8000/Example" />
            </baseAddresses>
          </host>
        </service>
      </services>      
    </system.serviceModel>

The mexHttpbinding exposes the interface over http, so now you can go with a web browser to

http://localhost:8000/Example
http://localhost:8000/Example?wsdl

and it will display the service and its metadata.

## 1. Your first service and host
Create an interface decorated with a ServiceContract attribute and member functions decorated with the OperationContract attribute. 

    namespace Service
    {
        [ServiceContract]
        interface IExample
        {
            [OperationContract]
            string Echo(string s);
        }
     }

Create a concrete class implementing this interface and you have the service.

    namespace Service
    {
        public class Example : IExample
        {
            public string Echo(string s)
            {
                return s;
            }
        }
    }

Then create the host where the service will run from. This can be any type of application. Console, service, GUI application or web server.

    namespace Console
    {
        using Service;
      
        class Console
        {
            Servicehost mHost;
    
            public Console()
            {
                mHost = new ServiceHost(typeof(Example));               
            } 

            public void Open()
            {
                mHost.Open();
            }
        
            public void Close()
            {
                mHost.Close();
            }
    
            public static void Main(string[] args)
            {
                Console host = new Console();
                
                host.Open();    

                Console.Readline();

                host.Close();
            }
        }
    }

The ServiceHost class will read the configuration file to initialize the service.

    <system.serviceModel>
      <services>
        <service name="Service.Example">
          <endpoint name="netTcpExample" contract="Service.IExample" binding="netTcpBinding" />
          <host>
            <baseAddresses>
              <add baseAddress="net.tcp://localhost:9000/Example" />
            </baseAddresses>
          </host>
        </service>
      </services>      
    </system.serviceModel>

## First Service Client
Let's say you have a service the same as the one defined in the "First service and host" example.

To create a client, define the client configuration section in the system.serviceModel section of your client configuration file. 

    <system.serviceModel>
      <services>
        <client name="Service.Example">
          <endpoint name="netTcpExample" contract="Service.IExample" binding="netTcpBinding" address="net.tcp://localhost:9000/Example" />
        </client>
      </services>      
    </system.serviceModel>

Then copy the service contract definition from the service: 

    namespace Service
    {
        [ServiceContract]
        interface IExample
        {
            [OperationContract]
            string Echo(string s);
        }
    }

(NOTE: You could also consume this via adding a binary reference instead to the assembly containing the service contract instead.)

Then you can create the actual client using `ChannelFactory<T>`, and call the operation on the service:

    namespace Console
    {
        using Service;
      
        class Console
        {
            public static void Main(string[] args)
            {
                var client = new System.ServiceModel.ChannelFactory<IExample>("Service.Example").CreateChannel();
                var s = client.Echo("Hello World");
                Console.Write(s);
                Console.Read();
            }
        }
    }

    



## Create a ServiceHost programmatically
Creating a ServiceHost programmatically (**without** config file) in its most basic form:

    namespace ConsoleHost
    {
      class ConsoleHost
      {
        ServiceHost mHost;
    
        public Console()
        {
          mHost = new ServiceHost(typeof(Example), new Uri("net.tcp://localhost:9000/Example"));
    
          NetTcpBinding tcp = new NetTcpBinding();
    
          mHost.AddServiceEndpoint(typeof(IExample),tcp,"net.tcp://localhost:9000/Example");                      
        }

        public void Open()
        {
            mHost.Open();
        }
        
        public void Close()
        {
            mHost.Close();
        }
    
        public static void Main(string[] args)
        {
            ConsoleHost host = new ConsoleHost();
            
            host.Open();
            
            Console.ReadLine();
            
            host.Close();
        }
      }
    }

1. Create a ServiceHost instance passing the concrete class type and zero or more baseaddress Uri's.
2. Construct the desired binding, NetTcpBinding in this case.
3. call AddServiceEndpoint passing the **A**ddress, **B**inding and **C**ontract. (ABC mnemonic for WCF endpoints).
4. Open the host.
5. Keep host open until user press key on console.
6. Close the host.

## Programmatically adding a metadata endpoint to a service
When you also want to expose metadata without a config file you can build on the example programmatically creating a ServiceHost:

    public ConsoleHost()
    {
        mHost = new ServiceHost(typeof(Example), new Uri("http://localhost:8000/Example"), new Uri("net.tcp://9000/Example"));

        NetTcpBinding tcp = new NetTcpBinding();

        mHost.AddServiceEndpoint(typeof(IExample), tcp, "net.tcp://localhost:9000/Example");            

        ServiceMetadataBehavior metaBehavior = mHost.Description.Behaviors.Find<ServiceMetadataBehavior>();

        if (metaBehavior == null)
        {
            metaBehavior = new ServiceMetadataBehavior();
            metaBehavior.MetadataExporter.PolicyVersion = PolicyVersion.Policy15;
            metaBehavior.HttpGetEnabled = true;

            mHost.Description.Behaviors.Add(metaBehavior);
            mHost.AddServiceEndpoint(ServiceMetadataBehavior.MexContractName, MetadataExchangeBindings.CreateMexHttpBinding(), "mex");
        }

        mHost.Open();
    }

1. Create a ServiceHost instance passing the concrete class type and zero or more baseaddress Uri's.
2. When you use mexHttpBinding you have to add http://localhost:8000/Example baseaddress
3. Construct the desired binding, NetTcpBinding in this case.
4. call AddServiceEndpoint passing the Address, Binding and Contract. (ABC).
5. Construct a ServiceMetadataBehavior
6. Set HttpGetEnabled to true
7. Add the metadata behavior to the behaviors collection.
8. call AddServiceEndpoint passing the constants for metadata exchange
9. Open the host.


