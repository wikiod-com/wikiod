---
title: "Rede"
slug: "rede"
draft: false
images: []
weight: 9931
type: docs
toc: true
---

## Sintaxe
- TcpClient(string host, int port);

Você pode obter o `NetworkStream` de um `TcpClient` com `client.GetStream()` e passá-lo para um `StreamReader/StreamWriter` para obter acesso aos métodos assíncronos de leitura e gravação.

## Cliente de comunicação TCP básico
Este exemplo de código cria um cliente TCP, envia "Hello World" pela conexão de soquete e grava a resposta do servidor no console antes de fechar a conexão.

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
    

## Baixe um arquivo de um servidor web
Baixar um arquivo da Internet é uma tarefa muito comum exigida por quase todos os aplicativos que você provavelmente criará.

Para fazer isso, você pode usar a classe "[System.Net.WebClient][1]".

O uso mais simples disso, usando o padrão "usando", é mostrado abaixo:

    using (var webClient = new WebClient())
    {
        webClient.DownloadFile("http://www.server.com/file.txt", "C:\\file.txt");
    }

O que este exemplo faz é usar "usando" para garantir que seu cliente da Web seja limpo corretamente quando concluído e simplesmente transfere o recurso nomeado da URL no primeiro parâmetro para o arquivo nomeado em seu disco rígido local no segundo parâmetro.

O primeiro parâmetro é do tipo "[System.Uri][2]", o segundo parâmetro é do tipo "[System.String][3]"

Você também pode usar essa função de forma assíncrona, para que ela desligue e faça o download em segundo plano, enquanto sua aplicação continua com outra coisa, usar a chamada dessa forma é de grande importância nas aplicações modernas, pois ajuda para manter sua interface de usuário responsiva.

Ao usar os métodos Async, você pode conectar manipuladores de eventos que permitem monitorar o progresso, para que você possa, por exemplo, atualizar uma barra de progresso, algo como o seguinte:

    var webClient = new WebClient())
    webClient.DownloadFileCompleted += new AsyncCompletedEventHandler(Completed);
    webClient.DownloadProgressChanged += new DownloadProgressChangedEventHandler(ProgressChanged);
    webClient.DownloadFileAsync("http://www.server.com/file.txt", "C:\\file.txt");

Um ponto importante a ser lembrado se você usar as versões assíncronas, no entanto, é "Tenha muito cuidado ao usá-las em uma sintaxe 'usando'".

A razão para isso é bastante simples. Depois de chamar o método de download do arquivo, ele retornará imediatamente. Se você tiver isso em um bloco de uso, você retornará, sairá desse bloco e descartará imediatamente o objeto de classe e, assim, cancelará seu download em andamento.

Se você usar a maneira 'usando' de realizar uma transferência assíncrona, certifique-se de permanecer dentro do bloco de inclusão até que a transferência seja concluída.


[1]: https://msdn.microsoft.com/en-us/library/system.net.webclient.aspx%22System.Net.WebClient%22
[2]: https://msdn.microsoft.com/en-us/library/system.uri.aspx%22System.Uri%22
[3]: https://msdn.microsoft.com/en-us/library/system.string.aspx%22System.String%22

## Cliente TCP assíncrono
Usar `async/await` em aplicativos C# simplifica o multi-threading. É assim que você pode usar `async/await` em conjunto com um TcpClient.

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

## Cliente UDP Básico
Este exemplo de código cria um cliente UDP e envia "Hello World" pela rede para o destinatário pretendido. Um ouvinte não precisa estar ativo, pois o UDP não tem conexão e transmitirá a mensagem independentemente. Uma vez que a mensagem é enviada, o trabalho do cliente é feito.

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
            

Abaixo está um exemplo de um ouvinte UDP para complementar o cliente acima. Ele ficará constantemente sentado e ouvindo o tráfego em uma determinada porta e simplesmente gravará esses dados no console. Este exemplo contém um sinalizador de controle '`done`' que não está definido internamente e depende de algo para definir isso para permitir encerrar o ouvinte e sair.

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




