---
title: "La mise en réseau"
slug: "la-mise-en-reseau"
draft: false
images: []
weight: 9931
type: docs
toc: true
---

## Syntaxe
- TcpClient (hôte de chaîne, port int) ;

Vous pouvez obtenir le `NetworkStream` d'un `TcpClient` avec `client.GetStream()` et le transmettre à un `StreamReader/StreamWriter` pour accéder à leurs méthodes de lecture et d'écriture asynchrones.

## Client de communication TCP de base
Cet exemple de code crée un client TCP, envoie "Hello World" via la connexion socket, puis écrit la réponse du serveur sur la console avant de fermer la connexion.

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
    

## Télécharger un fichier depuis un serveur Web
Le téléchargement d'un fichier à partir d'Internet est une tâche très courante requise par presque toutes les applications que vous êtes susceptible de créer.

Pour ce faire, vous pouvez utiliser la classe "[System.Net.WebClient][1]".

L'utilisation la plus simple de ceci, en utilisant le modèle "using", est illustrée ci-dessous :

    using (var webClient = new WebClient())
    {
        webClient.DownloadFile("http://www.server.com/file.txt", "C:\\file.txt");
    }

Ce que fait cet exemple, c'est qu'il utilise "using" pour s'assurer que votre client Web est correctement nettoyé une fois terminé, et transfère simplement la ressource nommée de l'URL dans le premier paramètre, vers le fichier nommé sur votre disque dur local dans le second paramètre.

Le premier paramètre est de type "[System.Uri][2]", le deuxième paramètre est de type "[System.String][3]"

Vous pouvez également utiliser cette fonction est un formulaire asynchrone, de sorte qu'il se déclenche et effectue le téléchargement en arrière-plan, pendant que votre application s'occupe d'autre chose, utiliser l'appel de cette manière est d'une importance majeure dans les applications modernes, car cela aide pour garder votre interface utilisateur réactive.

Lorsque vous utilisez les méthodes Async, vous pouvez connecter des gestionnaires d'événements qui vous permettent de surveiller la progression, de sorte que vous puissiez, par exemple, mettre à jour une barre de progression, quelque chose comme ceci :

    var webClient = new WebClient())
    webClient.DownloadFileCompleted += new AsyncCompletedEventHandler(Completed);
    webClient.DownloadProgressChanged += new DownloadProgressChangedEventHandler(ProgressChanged);
    webClient.DownloadFileAsync("http://www.server.com/file.txt", "C:\\file.txt");

Un point important à retenir si vous utilisez les versions Async cependant, c'est "Soyez très prudent lorsque vous les utilisez dans une syntaxe 'using'".

La raison en est assez simple. Une fois que vous appelez la méthode de fichier de téléchargement, elle reviendra immédiatement. Si vous avez ceci dans un bloc using, vous reviendrez puis quitterez ce bloc, et disposerez immédiatement de l'objet de classe, et annulerez ainsi votre téléchargement en cours.

Si vous utilisez la méthode "utiliser" pour effectuer un transfert asynchrone, veillez à rester à l'intérieur du bloc englobant jusqu'à la fin du transfert.


[1] : https://msdn.microsoft.com/en-us/library/system.net.webclient.aspx%22System.Net.WebClient%22
[2] : https://msdn.microsoft.com/en-us/library/system.uri.aspx%22System.Uri%22
[3] : https://msdn.microsoft.com/en-us/library/system.string.aspx%22System.String%22

## Client TCP asynchrone
L'utilisation de `async/wait` dans les applications C# simplifie le multi-threading. C'est ainsi que vous pouvez utiliser `async/wait` en conjonction avec un TcpClient.

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

## Client UDP de base
Cet exemple de code crée un client UDP puis envoie "Hello World" sur le réseau au destinataire prévu. Un écouteur n'a pas besoin d'être actif, car UDP est sans connexion et diffusera le message malgré tout. Une fois le message envoyé, le travail des clients est terminé.

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
            

Vous trouverez ci-dessous un exemple d'écouteur UDP pour compléter le client ci-dessus. Il restera constamment assis et écoutera le trafic sur un port donné et écrira simplement ces données sur la console. Cet exemple contient un indicateur de contrôle ''done'' qui n'est pas défini en interne et s'appuie sur quelque chose pour le définir afin de permettre la fin de l'écouteur et la sortie.

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




