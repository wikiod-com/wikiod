---
title: "Windows İletişim Vakfı"
slug: "windows-iletisim-vakf"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Windows Communication Foundation (WCF), hizmet odaklı uygulamalar oluşturmaya yönelik bir çerçevedir. WCF kullanarak, verileri bir hizmet uç noktasından diğerine zaman uyumsuz iletiler olarak gönderebilirsiniz. Hizmet uç noktası, IIS tarafından barındırılan sürekli olarak kullanılabilen bir hizmetin parçası olabilir veya bir uygulamada barındırılan bir hizmet olabilir. Mesajlar, XML olarak gönderilen tek bir karakter veya kelime kadar basit veya bir ikili veri akışı kadar karmaşık olabilir.

## Başlarken örneği
Hizmet, meta veri olarak herkese açık olarak sunduğu bir hizmet sözleşmesinde gerçekleştirdiği işlemleri açıklar.

    // Define a service contract.  
    [ServiceContract(Namespace="http://StackOverflow.ServiceModel.Samples")]  
    public interface ICalculator  
    {  
        [OperationContract]  
        double Add(double n1, double n2);
    }

Hizmet uygulaması, aşağıdaki örnek kodda gösterildiği gibi uygun sonucu hesaplar ve döndürür.

    // Service class that implements the service contract.  
    public class CalculatorService : ICalculator  
    {  
        public double Add(double n1, double n2)  
        {  
            return n1 + n2;  
        }
    }

Hizmet, aşağıdaki örnek yapılandırmada gösterildiği gibi, bir yapılandırma dosyası (Web.config) kullanılarak tanımlanan hizmetle iletişim kurmak için bir uç nokta sunar.

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

Çerçeve, varsayılan olarak meta verileri göstermez. Bu nedenle, hizmet ServiceMetadataBehavior'u açar ve http://localhost/servicemodelsamples/service.svc/mex adresinde bir meta veri değişimi (MEX) uç noktası sunar. Aşağıdaki yapılandırma bunu göstermektedir.

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

İstemci, ServiceModel Meta Veri Yardımcı Programı Aracı (Svcutil.exe) tarafından oluşturulan bir istemci sınıfını kullanarak belirli bir sözleşme türünü kullanarak iletişim kurar.

Yazılan proxy'yi oluşturmak için istemci dizinindeki SDK komut isteminden aşağıdaki komutu çalıştırın:

    svcutil.exe /n:"http://StackOverflow.ServiceModel.Samples,StackOverflow.ServiceModel.Samples" http://localhost/servicemodelsamples/service.svc/mex /out:generatedClient.cs  

Hizmet gibi, istemci de iletişim kurmak istediği son noktayı belirtmek için bir yapılandırma dosyası (app.config) kullanır. İstemci uç noktası yapılandırması, aşağıdaki örnekte gösterildiği gibi, hizmet uç noktası, bağlama ve sözleşme için mutlak bir adresten oluşur.

    <client>  
         <endpoint  
             address="http://localhost/servicemodelsamples/service.svc"   
             binding="wsHttpBinding"   
             contract="StackOverflow.ServiceModel.Samples.ICalculator" />  
    </client>  

İstemci uygulaması, istemciyi başlatır ve aşağıdaki örnek kodda gösterildiği gibi hizmetle iletişim kurmaya başlamak için yazılan arabirimi kullanır.

    // Create a client.  
    CalculatorClient client = new CalculatorClient();  
    
    // Call the Add service operation.  
    double value1 = 100.00D;  
    double value2 = 15.99D;  
    double result = client.Add(value1, value2);  
    Console.WriteLine("Add({0},{1}) = {2}", value1, value2, result); 
    
    //Closing the client releases all communication resources.  
    client.Close();  

 

