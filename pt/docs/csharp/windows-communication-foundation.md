---
title: "Fundação de Comunicação do Windows"
slug: "fundacao-de-comunicacao-do-windows"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

O Windows Communication Foundation (WCF) é uma estrutura para criar aplicativos orientados a serviços. Usando o WCF, você pode enviar dados como mensagens assíncronas de um ponto de extremidade de serviço para outro. Um ponto de extremidade de serviço pode fazer parte de um serviço continuamente disponível hospedado pelo IIS ou pode ser um serviço hospedado em um aplicativo. As mensagens podem ser tão simples quanto um único caractere ou palavra enviada como XML, ou tão complexas quanto um fluxo de dados binários.

## Exemplo de introdução
O serviço descreve as operações que realiza em um contrato de serviço que expõe publicamente como metadados.

    // Define a service contract.  
    [ServiceContract(Namespace="http://StackOverflow.ServiceModel.Samples")]  
    public interface ICalculator  
    {  
        [OperationContract]  
        double Add(double n1, double n2);
    }

A implementação do serviço calcula e retorna o resultado apropriado, conforme mostrado no código de exemplo a seguir.

    // Service class that implements the service contract.  
    public class CalculatorService : ICalculator  
    {  
        public double Add(double n1, double n2)  
        {  
            return n1 + n2;  
        }
    }

O serviço expõe um ponto de extremidade para comunicação com o serviço, definido usando um arquivo de configuração (Web.config), conforme mostrado na configuração de exemplo a seguir.

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

A estrutura não expõe metadados por padrão. Como tal, o serviço ativa o ServiceMetadataBehavior e expõe um ponto de extremidade de troca de metadados (MEX) em http://localhost/servicemodelsamples/service.svc/mex. A configuração a seguir demonstra isso.

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

O cliente se comunica usando um determinado tipo de contrato usando uma classe de cliente que é gerada pela ServiceModel Metadata Utility Tool (Svcutil.exe).

Execute o seguinte comando no prompt de comando do SDK no diretório do cliente para gerar o proxy digitado:

    svcutil.exe /n:"http://StackOverflow.ServiceModel.Samples,StackOverflow.ServiceModel.Samples" http://localhost/servicemodelsamples/service.svc/mex /out:generatedClient.cs  

Assim como o serviço, o cliente usa um arquivo de configuração (App.config) para especificar o terminal com o qual deseja se comunicar. A configuração do endpoint do cliente consiste em um endereço absoluto para o endpoint de serviço, a associação e o contrato, conforme mostrado no exemplo a seguir.

    <client>  
         <endpoint  
             address="http://localhost/servicemodelsamples/service.svc"   
             binding="wsHttpBinding"   
             contract="StackOverflow.ServiceModel.Samples.ICalculator" />  
    </client>  

A implementação do cliente instancia o cliente e usa a interface tipada para iniciar a comunicação com o serviço, conforme mostrado no código de exemplo a seguir.

    // Create a client.  
    CalculatorClient client = new CalculatorClient();  
    
    // Call the Add service operation.  
    double value1 = 100.00D;  
    double value2 = 15.99D;  
    double result = client.Add(value1, value2);  
    Console.WriteLine("Add({0},{1}) = {2}", value1, value2, result); 
    
    //Closing the client releases all communication resources.  
    client.Close();  

 

