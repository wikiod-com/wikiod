---
title: "Redes"
slug: "redes"
draft: false
images: []
weight: 9931
type: docs
toc: true
---

## Sintaxis
- TcpClient(cadena host, puerto int);

Puede obtener el `NetworkStream` de un `TcpClient` con `client.GetStream()` y pasarlo a un `StreamReader/StreamWriter` para obtener acceso a sus métodos de lectura y escritura asíncronos.

## Cliente de comunicación TCP básico
Este ejemplo de código crea un cliente TCP, envía "Hello World" a través de la conexión de socket y luego escribe la respuesta del servidor en la consola antes de cerrar la conexión.

    // Declare Variables
    string host = "stackoverflow.com";
    int port = 9999;
    int timeout = 5000;

    // Create TCP client and connect
    using (var _client = new TcpClient(host, port))
    using (var _netStream = _client.GetStream()) 
    {
        _netStream.ReadTimeout = timeout;
    
        // Write a message over the socket
        string message = "Hello World!";
        byte[] dataToSend = System.Text.Encoding.ASCII.GetBytes(message);
        _netStream.Write(dataToSend, 0, dataToSend.Length);
        
        // Read server response
        byte[] recvData = new byte[256];
        int bytes = _netStream.Read(recvData, 0, recvData.Length);
        message = System.Text.Encoding.ASCII.GetString(recvData, 0, bytes);
        Console.WriteLine(string.Format("Server: {0}", message));                
    };// The client and stream will close as control exits the using block (Equivilent but safer than calling Close();
    

## Descargar un archivo desde un servidor web
Descargar un archivo de Internet es una tarea muy común requerida por casi todas las aplicaciones que probablemente cree.

Para lograr esto, puede usar la clase "[System.Net.WebClient][1]".

El uso más simple de esto, usando el patrón "usando", se muestra a continuación:

    using (var webClient = new WebClient())
    {
        webClient.DownloadFile("http://www.server.com/file.txt", "C:\\file.txt");
    }

Lo que hace este ejemplo es usar "usar" para asegurarse de que su cliente web se limpie correctamente cuando termine, y simplemente transfiere el recurso con nombre de la URL en el primer parámetro, al archivo con nombre en su disco duro local en el segundo parámetro.

El primer parámetro es del tipo "[System.Uri][2]", el segundo parámetro es del tipo "[System.String][3]"

También puede usar esta función en un formulario asíncrono, de modo que se apaga y realiza la descarga en segundo plano, mientras su aplicación continúa con otra cosa, usar la llamada de esta manera es de gran importancia en las aplicaciones modernas, ya que ayuda para mantener su interfaz de usuario receptiva.

Cuando usa los métodos Async, puede conectar controladores de eventos que le permitan monitorear el progreso, de modo que pueda, por ejemplo, actualizar una barra de progreso, algo como lo siguiente:

    var webClient = new WebClient())
    webClient.DownloadFileCompleted += new AsyncCompletedEventHandler(Completed);
    webClient.DownloadProgressChanged += new DownloadProgressChangedEventHandler(ProgressChanged);
    webClient.DownloadFileAsync("http://www.server.com/file.txt", "C:\\file.txt");

Sin embargo, un punto importante para recordar si usa las versiones Async es "Tenga mucho cuidado al usarlos en una sintaxis de 'uso'".

La razón de esto es bastante simple. Una vez que llame al método de descarga de archivos, volverá inmediatamente. Si tiene esto en un bloque de uso, regresará y luego saldrá de ese bloque e inmediatamente desechará el objeto de clase y, por lo tanto, cancelará la descarga en curso.

Si usa la forma de 'usar' para realizar una transferencia asíncrona, asegúrese de permanecer dentro del bloque adjunto hasta que se complete la transferencia.


[1]: https://msdn.microsoft.com/en-us/library/system.net.webclient.aspx%22System.Net.WebClient%22
[2]: https://msdn.microsoft.com/en-us/library/system.uri.aspx%22System.Uri%22
[3]: https://msdn.microsoft.com/en-us/library/system.string.aspx%22System.String%22

## Cliente TCP asíncrono
El uso de `async/await` en las aplicaciones de C# simplifica los subprocesos múltiples. Así es como puede usar `async/await` junto con un TcpClient.

    // Declare Variables
    string host = "stackoverflow.com";
    int port = 9999;
    int timeout = 5000;
    
    // Create TCP client and connect
    // Then get the netstream and pass it
    // To our StreamWriter and StreamReader
    using (var client = new TcpClient())
    using (var netstream = client.GetStream()) 
    using (var writer = new StreamWriter(netstream))
    using (var reader = new StreamReader(netstream))
    {
        // Asynchronsly attempt to connect to server
        await client.ConnectAsync(host, port);
        
        // AutoFlush the StreamWriter
        // so we don't go over the buffer
        writer.AutoFlush = true;
        
        // Optionally set a timeout
        netstream.ReadTimeout = timeout;
    
        // Write a message over the TCP Connection
        string message = "Hello World!";
        await writer.WriteLineAsync(message);
        
        // Read server response
        string response = await reader.ReadLineAsync();
        Console.WriteLine(string.Format($"Server: {response}"));                
    }
    // The client and stream will close as control exits
    // the using block (Equivilent but safer than calling Close();

## Cliente UDP básico
Este ejemplo de código crea un cliente UDP y luego envía "Hello World" a través de la red al destinatario deseado. Un oyente no tiene que estar activo, ya que UDP no tiene conexión y transmitirá el mensaje independientemente. Una vez que se envía el mensaje, el trabajo del cliente está hecho.

    byte[] data = Encoding.ASCII.GetBytes("Hello World");
    string ipAddress = "192.168.1.141";
    string sendPort = 55600;
    try
    {
         using (var client = new UdpClient())
         {
             IPEndPoint ep = new IPEndPoint(IPAddress.Parse(ipAddress), sendPort);
             client.Connect(ep);
             client.Send(data, data.Length);
         }
    }
    catch (Exception ex)
    {
         Console.WriteLine(ex.ToString());
    }
            

A continuación se muestra un ejemplo de un oyente UDP para complementar el cliente anterior. Se sentará y escuchará constantemente el tráfico en un puerto determinado y simplemente escribirá esos datos en la consola. Este ejemplo contiene un indicador de control '`done`' que no está configurado internamente y se basa en algo para configurar esto para permitir finalizar el oyente y salir.

    bool done = false;
    int listenPort = 55600;
    using(UdpClinet listener = new UdpClient(listenPort))
    {
        IPEndPoint listenEndPoint = new IPEndPoint(IPAddress.Any, listenPort);
        while(!done)
        {
            byte[] receivedData = listener.Receive(ref listenPort);

            Console.WriteLine("Received broadcast message from client {0}", listenEndPoint.ToString());

            Console.WriteLine("Decoded data is:");
            Console.WriteLine(Encoding.ASCII.GetString(receivedData)); //should be "Hello World" sent from above client
        }
    }




