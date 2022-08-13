---
title: "Fundación de comunicación de Windows"
slug: "fundacion-de-comunicacion-de-windows"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Windows Communication Foundation (WCF) es un marco para crear aplicaciones orientadas a servicios. Con WCF, puede enviar datos como mensajes asincrónicos de un extremo de servicio a otro. Un extremo de servicio puede ser parte de un servicio disponible de forma continua hospedado por IIS o puede ser un servicio hospedado en una aplicación. Los mensajes pueden ser tan simples como un solo carácter o palabra enviada como XML, o tan complejos como un flujo de datos binarios.

## Ejemplo de introducción
El servicio describe las operaciones que realiza en un contrato de servicio que expone públicamente como metadatos.

    // Define a service contract.  
    [ServiceContract(Namespace="http://StackOverflow.ServiceModel.Samples")]  
    public interface ICalculator  
    {  
        [OperationContract]  
        double Add(double n1, double n2);
    }

La implementación del servicio calcula y devuelve el resultado apropiado, como se muestra en el siguiente código de ejemplo.

    // Service class that implements the service contract.  
    public class CalculatorService : ICalculator  
    {  
        public double Add(double n1, double n2)  
        {  
            return n1 + n2;  
        }
    }

El servicio expone un extremo para comunicarse con el servicio, definido mediante un archivo de configuración (Web.config), como se muestra en la siguiente configuración de ejemplo.

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

El marco no expone los metadatos de forma predeterminada. Como tal, el servicio activa ServiceMetadataBehavior y expone un extremo de intercambio de metadatos (MEX) en http://localhost/servicemodelsamples/service.svc/mex. La siguiente configuración demuestra esto.

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

El cliente se comunica mediante un tipo de contrato determinado mediante una clase de cliente generada por la herramienta de utilidad de metadatos de ServiceModel (Svcutil.exe).

Ejecute el siguiente comando desde el símbolo del sistema SDK en el directorio del cliente para generar el proxy escrito:

    svcutil.exe /n:"http://StackOverflow.ServiceModel.Samples,StackOverflow.ServiceModel.Samples" http://localhost/servicemodelsamples/service.svc/mex /out:generatedClient.cs  

Al igual que el servicio, el cliente utiliza un archivo de configuración (App.config) para especificar el punto final con el que desea comunicarse. La configuración del punto de conexión del cliente consta de una dirección absoluta para el punto de conexión del servicio, el enlace y el contrato, como se muestra en el siguiente ejemplo.

    <client>  
         <endpoint  
             address="http://localhost/servicemodelsamples/service.svc"   
             binding="wsHttpBinding"   
             contract="StackOverflow.ServiceModel.Samples.ICalculator" />  
    </client>  

La implementación del cliente crea una instancia del cliente y usa la interfaz escrita para comenzar a comunicarse con el servicio, como se muestra en el siguiente código de ejemplo.

    // Create a client.  
    CalculatorClient client = new CalculatorClient();  
    
    // Call the Add service operation.  
    double value1 = 100.00D;  
    double value2 = 15.99D;  
    double result = client.Add(value1, value2);  
    Console.WriteLine("Add({0},{1}) = {2}", value1, value2, result); 
    
    //Closing the client releases all communication resources.  
    client.Close();  

 

