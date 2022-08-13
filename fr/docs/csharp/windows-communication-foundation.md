---
title: "Fondation de communication Windows"
slug: "fondation-de-communication-windows"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Windows Communication Foundation (WCF) est une infrastructure permettant de créer des applications orientées services. À l'aide de WCF, vous pouvez envoyer des données sous forme de messages asynchrones d'un point de terminaison de service à un autre. Un point de terminaison de service peut faire partie d'un service disponible en permanence hébergé par IIS, ou il peut s'agir d'un service hébergé dans une application. Les messages peuvent être aussi simples qu'un seul caractère ou mot envoyé en XML, ou aussi complexes qu'un flux de données binaires.

## Exemple de démarrage
Le service décrit les opérations qu'il effectue dans un contrat de service qu'il expose publiquement sous forme de métadonnées.

    // Define a service contract.  
    [ServiceContract(Namespace="http://StackOverflow.ServiceModel.Samples")]  
    public interface ICalculator  
    {  
        [OperationContract]  
        double Add(double n1, double n2);
    }

L'implémentation du service calcule et renvoie le résultat approprié, comme illustré dans l'exemple de code suivant.

    // Service class that implements the service contract.  
    public class CalculatorService : ICalculator  
    {  
        public double Add(double n1, double n2)  
        {  
            return n1 + n2;  
        }
    }

Le service expose un point de terminaison pour communiquer avec le service, défini à l'aide d'un fichier de configuration (Web.config), comme illustré dans l'exemple de configuration suivant.

    <services>  
        <service   
            name="StackOverflow.ServiceModel.Samples.CalculatorService"  
            behaviorConfiguration="CalculatorServiceBehavior">  
            <!-- ICalculator is exposed at the base address provided by  
             host: http://localhost/servicemodelsamples/service.svc.  -->  
           <endpoint address=""  
                  binding="wsHttpBinding"  
                  contract="StackOverflow.ServiceModel.Samples.ICalculator" />  
           ...  
        </service>  
    </services>

Le framework n'expose pas les métadonnées par défaut. En tant que tel, le service active le ServiceMetadataBehavior et expose un point de terminaison d'échange de métadonnées (MEX) à l'adresse http://localhost/servicemodelsamples/service.svc/mex. La configuration suivante le démontre.

    <system.serviceModel>  
      <services>  
        <service   
            name="StackOverflow.ServiceModel.Samples.CalculatorService"  
            behaviorConfiguration="CalculatorServiceBehavior">  
          ...  
          <!-- the mex endpoint is explosed at  
           http://localhost/servicemodelsamples/service.svc/mex -->  
          <endpoint address="mex"  
                    binding="mexHttpBinding"  
                    contract="IMetadataExchange" />  
        </service>  
      </services>  
    
      <!--For debugging purposes set the includeExceptionDetailInFaults  
       attribute to true-->  
      <behaviors>  
        <serviceBehaviors>  
          <behavior name="CalculatorServiceBehavior">  
            <serviceMetadata httpGetEnabled="True"/>  
            <serviceDebug includeExceptionDetailInFaults="False" />  
          </behavior>  
        </serviceBehaviors>  
      </behaviors>  
    </system.serviceModel>  

Le client communique à l'aide d'un type de contrat donné à l'aide d'une classe de client générée par l'outil utilitaire de métadonnées ServiceModel (Svcutil.exe).

Exécutez la commande suivante à partir de l'invite de commande du SDK dans le répertoire client pour générer le proxy typé :

    svcutil.exe /n:"http://StackOverflow.ServiceModel.Samples,StackOverflow.ServiceModel.Samples" http://localhost/servicemodelsamples/service.svc/mex /out:generatedClient.cs  

Comme le service, le client utilise un fichier de configuration (App.config) pour spécifier le point de terminaison avec lequel il veut communiquer. La configuration du point de terminaison client se compose d'une adresse absolue pour le point de terminaison de service, la liaison et le contrat, comme illustré dans l'exemple suivant.

    <client>  
         <endpoint  
             address="http://localhost/servicemodelsamples/service.svc"   
             binding="wsHttpBinding"   
             contract="StackOverflow.ServiceModel.Samples.ICalculator" />  
    </client>  

L'implémentation du client instancie le client et utilise l'interface typée pour commencer à communiquer avec le service, comme illustré dans l'exemple de code suivant.

    // Create a client.  
    CalculatorClient client = new CalculatorClient();  
    
    // Call the Add service operation.  
    double value1 = 100.00D;  
    double value2 = 15.99D;  
    double result = client.Add(value1, value2);  
    Console.WriteLine("Add({0},{1}) = {2}", value1, value2, result); 
    
    //Closing the client releases all communication resources.  
    client.Close();  

 

