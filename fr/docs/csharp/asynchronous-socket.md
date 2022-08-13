---
title: "Socket asynchrone"
slug: "socket-asynchrone"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

En utilisant des sockets asynchrones, un serveur peut écouter les connexions entrantes et appliquer une autre logique en attendant, contrairement au socket synchrone, lorsqu'il écoute, il bloque le thread principal et l'application ne répond plus et se fige jusqu'à ce qu'un client se connecte.







**Prise et réseau**

Comment accéder à un serveur en dehors de mon propre réseau ?
Il s'agit d'une question courante et lorsqu'elle est posée, elle est généralement signalée comme sujet.

**Du côté serveur**

Sur le réseau de votre serveur, vous devez rediriger votre routeur vers votre serveur.

Par exemple, un PC sur lequel le serveur s'exécute :

IP locale = '192.168.1.115'

Le serveur écoute le port 1234.

Transférez les connexions entrantes sur le routeur "Port 1234" vers "192.168.1.115"

**Côté client**

La seule chose que vous devez changer est l'adresse IP. Vous ne voulez pas vous connecter à votre adresse de bouclage mais à l'adresse IP publique du réseau sur lequel votre serveur s'exécute. Cette IP, vous pouvez l'obtenir [ici][1].

     _connectingSocket.Connect(new IPEndPoint(IPAddress.Parse("10.10.10.10"), 1234));

Alors maintenant, vous créez une requête sur ce point de terminaison : `10.10.10.10:1234` si vous avez bien transféré le port de votre routeur vers votre serveur et
le client se connectera sans aucun problème.

Si vous souhaitez vous connecter à une adresse IP locale, vous n'aurez pas à transférer l'adresse de bouclage en `192.168.1.178` ou quelque chose comme ça.

**Envoi de données :**

Les données sont envoyées dans un tableau d'octets. Vous devez emballer vos données dans un tableau d'octets et les décompresser de l'autre côté.

Si vous connaissez socket, vous pouvez également essayer de chiffrer votre tableau d'octets avant de l'envoyer. Cela empêchera quiconque de voler votre colis.

[1] : http://whatismyipaddress.com/

## Exemple de socket asynchrone (client / serveur).
> **Exemple côté serveur**

**Créer un écouteur pour le serveur**

Commencez par créer un serveur qui gérera les clients qui se connectent et les requêtes qui seront envoyées. Créez donc une classe d'écoute qui gérera cela.

    class Listener
    {
        public Socket ListenerSocket; //This is the socket that will listen to any incoming connections
        public short Port = 1234; // on this port we will listen

        public Listener()
        {
            ListenerSocket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        }
     }

        
Nous devons d'abord initialiser le socket Listener sur lequel nous pouvons écouter toutes les connexions. Nous allons utiliser un socket Tcp, c'est pourquoi nous utilisons SocketType.Stream. Nous spécifions également sur quel port le serveur doit écouter

Ensuite, nous commençons à écouter toutes les connexions entrantes.

**Les méthodes d'arborescence que nous utilisons ici sont :**

1. [ListenerSocket.Bind();][1]
 
    This method binds the socket to an [IPEndPoint][2]. This class contains the host and local or remote port information needed by an application to connect to a service on a host.
2. [ListenerSocket.Listen(10);][3]

    The backlog parameter specifies the number of incoming connections that can be queued for acceptance.
3. [ListenerSocket.BeginAccept();][4]

    The server will start listening for incoming connections and will go on with other logic. When there is an connection the server switches back to this method and will run the AcceptCallBack methodt


        public void StartListening()
        {
            try
            {                
                    MessageBox.Show($"Listening started port:{Port} protocol type: {ProtocolType.Tcp}");                    
                    ListenerSocket.Bind(new IPEndPoint(IPAddress.Any, Port));
                    ListenerSocket.Listen(10);
                    ListenerSocket.BeginAccept(AcceptCallback, ListenerSocket);                
            }
            catch(Exception ex)
            {
                throw new Exception("listening error" + ex);
            }
        }
        
Ainsi, lorsqu'un client se connecte, nous pouvons les accepter par cette méthode :

**Trois méthodes que nous utilisons ici sont :**

1. [ListenerSocket.EndAccept()][5]

     We started the callback with `Listener.BeginAccept()` end now we have to end that call back. `The EndAccept()` method accepts an IAsyncResult parameter, this will store the state of the asynchronous method, From this state we can extract the socket where the incoming connection was coming from.

2. `ClientController.AddClient()`

    With the socket we got from `EndAccept()` we create an Client with an own made method *(code ClientController below server example)*. 

3. [ListenerSocket.BeginAccept()][4]

    We need to start listening again when the socket is done with handling the new connection. Pass in the method who will catch this callback. And also pass int the Listener socket so we can reuse this socket for upcoming connections.


        public void AcceptCallback(IAsyncResult ar)
        {
            try
            {
                Console.WriteLine($"Accept CallBack port:{Port} protocol type: {ProtocolType.Tcp}");
                Socket acceptedSocket = ListenerSocket.EndAccept(ar);               
                ClientController.AddClient(acceptedSocket);

                ListenerSocket.BeginAccept(AcceptCallback, ListenerSocket);
            }
            catch (Exception ex)
            {
                throw new Exception("Base Accept error"+ ex);
            }
        }

Nous avons maintenant un socket d'écoute, mais comment recevons-nous les données envoyées par le client, c'est ce que montre le code suivant.

**Créer un serveur récepteur pour chaque client**

Tout d'abord, créez une classe de réception avec un constructeur qui prend un Socket en paramètre :

        public class ReceivePacket
        {
            private byte[] _buffer;
            private Socket _receiveSocket;

            public ReceivePacket(Socket receiveSocket)
            {
               _receiveSocket = receiveSocket;
            }
        }
Dans la méthode suivante, nous commençons d'abord par donner au tampon une taille de 4 octets (Int32) ou le paquet contient des parties {longueur, données réelles}. Ainsi, les 4 premiers octets que nous réservons pour la longueur des données, le reste pour les données réelles.

Ensuite, nous utilisons la méthode [BeginReceive()][6]. Cette méthode est utilisée pour commencer à recevoir des clients connectés et lorsqu'elle recevra des données, elle exécutera la fonction "ReceiveCallback".

        public void StartReceiving()
        {
            try
            {
                _buffer = new byte[4];
                _receiveSocket.BeginReceive(_buffer, 0, _buffer.Length, SocketFlags.None, ReceiveCallback, null);
            }
            catch {}
        }

        private void ReceiveCallback(IAsyncResult AR)
        {
            try
            {
                // if bytes are less than 1 takes place when a client disconnect from the server.
                // So we run the Disconnect function on the current client
                if (_receiveSocket.EndReceive(AR) > 1)
                {
                    // Convert the first 4 bytes (int 32) that we received and convert it to an Int32 (this is the size for the coming data).
                    _buffer = new byte[BitConverter.ToInt32(_buffer, 0)];  
                    // Next receive this data into the buffer with size that we did receive before
                    _receiveSocket.Receive(_buffer, _buffer.Length, SocketFlags.None); 
                    // When we received everything its onto you to convert it into the data that you've send.
                    // For example string, int etc... in this example I only use the implementation for sending and receiving a string.

                    // Convert the bytes to string and output it in a message box
                    string data = Encoding.Default.GetString(_buffer);
                    MessageBox.Show(data);
                    // Now we have to start all over again with waiting for a data to come from the socket.
                    StartReceiving();
                }
                else
                {
                    Disconnect();
                }
            }
            catch
            {
                // if exeption is throw check if socket is connected because than you can startreive again else Dissconect
                if (!_receiveSocket.Connected)
                {
                    Disconnect();
                }
                else
                {
                    StartReceiving();
                }
            }
        }

        private void Disconnect()
        {
            // Close connection
            _receiveSocket.Disconnect(true);
            // Next line only apply for the server side receive
            ClientController.RemoveClient(_clientId);
            // Next line only apply on the Client Side receive
            Here you want to run the method TryToConnect()
        }

Nous avons donc configuré un serveur qui peut recevoir et écouter les connexions entrantes. Lorsqu'un client se connecte, il est ajouté à une liste de clients et chaque client a sa propre classe de réception. Pour que le serveur écoute :

    Listener listener = new Listener();
    listener.StartListening();

**Certaines classes que j'utilise dans cet exemple**

        class Client
        {
            public Socket _socket { get; set; }
            public ReceivePacket Receive { get; set; }
            public int Id { get; set; }

            public Client(Socket socket, int id)
            {
                Receive = new ReceivePacket(socket, id);
                Receive.StartReceiving();
                _socket = socket;
                Id = id;
            }
        }

         static class ClientController
         {
              public static List<Client> Clients = new List<Client>();

              public static void AddClient(Socket socket)
              {
                  Clients.Add(new Client(socket,Clients.Count));
              }

              public static void RemoveClient(int id)
              {
                  Clients.RemoveAt(Clients.FindIndex(x => x.Id == id));
              }
          }


> **Exemple côté client**

**Connexion au serveur**

Tout d'abord nous voulons créer une classe qui se connecte au serveur dont nous lui donnons le nom : Connector :

     
    class Connector
    {
        private Socket _connectingSocket;
    }

La méthode suivante pour cette classe est TryToConnect()

Cette méthode contient quelques éléments intéressants :

1. Créez la prise ;
2. Ensuite, je boucle jusqu'à ce que la prise soit connectée
3. Chaque boucle tient juste le Thread pendant 1 seconde, nous ne voulons pas DOS le serveur XD
4. Avec [Connect()][7] il essaiera de se connecter au serveur. S'il échoue, il lancera une exception mais la ruse maintiendra la connexion du programme au serveur. Vous pouvez utiliser une méthode [Connect CallBack] [8] pour cela, mais je vais simplement appeler une méthode lorsque le Socket est connecté.
5. Notez que le client essaie maintenant de se connecter à votre ordinateur local sur le port 1234.

        public void TryToConnect()
        {
            _connectingSocket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            
             while (!_connectingSocket.Connected)
             {
                 Thread.Sleep(1000);

                 try
                 {
                     _connectingSocket.Connect(new IPEndPoint(IPAddress.Parse("127.0.0.1"), 1234));
                 }
                 catch { }
             }
             SetupForReceiveing();
            }
        }

        private void SetupForReceiveing()
        {
           // View Client Class bottom of Client Example
            Client.SetClient(_connectingSocket);
            Client.StartReceiving();
        }

**Envoi d'un message au serveur**

Nous avons donc maintenant une application presque finie ou Socket. La seule chose que nous n'avons pas jet est une classe pour envoyer un message au serveur.

    public class SendPacket
    {
        private Socket _sendSocked;

        public SendPacket(Socket sendSocket)
        {
            _sendSocked = sendSocket;
        }

        public void Send(string data)
        {
            try
            {         
                /* what hapends here:
                     1. Create a list of bytes
                     2. Add the length of the string to the list.
                        So if this message arrives at the server we can easily read the length of the coming message.
                     3. Add the message(string) bytes
                */
      
                var fullPacket = new List<byte>();
                fullPacket.AddRange(BitConverter.GetBytes(data.Length));
                fullPacket.AddRange(Encoding.Default.GetBytes(data));

                /* Send the message to the server we are currently connected to.
                Or package stucture is {length of data 4 bytes (int32), actual data}*/
                _sendSocked.Send(fullPacket.ToArray());
            }
            catch (Exception ex)
            {
                throw new Exception();
            }
        }

Enfin, créez deux boutons, l'un pour se connecter et l'autre pour envoyer un message :

        private void ConnectClick(object sender, EventArgs e)
        {
            Connector tpp = new Connector();
            tpp.TryToConnect();
        }

        private void SendClick(object sender, EventArgs e)
        {
            Client.SendString("Test data from client");
        }


**La classe client que j'ai utilisée dans cet exemple**
 
        public static void SetClient(Socket socket)
        {
            Id = 1;
            Socket = socket;
            Receive = new ReceivePacket(socket, Id);
            SendPacket = new SendPacket(socket);
        }

**Remarquer**

La classe de réception du serveur est identique à la classe de réception du client.

> **Conclusion**

Vous avez maintenant un serveur et un client. Vous pouvez travailler cet exemple de base. Par exemple, faites en sorte que le serveur puisse également recevoir des fichiers ou d'autres éléments. Ou envoyer un message au client. Dans le serveur, vous avez une liste de clients, donc lorsque vous recevez quelque chose dont vous saurez avec le client, il provient.

**Résultat final:**
[![entrez la description de l'image ici][9]][9]


[1] : https://msdn.microsoft.com/en-us/library/system.net.sockets.socket.bind(v=vs.110).aspx
[2] : https://msdn.microsoft.com/en-us/library/system.net.ipendpoint(v=vs.110).aspx
[3] : https://msdn.microsoft.com/nl-nl/library/system.net.sockets.socket.listen(v=vs.110).aspx
[4] : https://msdn.microsoft.com/en-us/library/5bb431f9(v=vs.110).aspx
[5] : https://msdn.microsoft.com/en-us/library/zdee4kd7(v=vs.110).aspx
[6] : https://msdn.microsoft.com/en-us/library/dxkwh6zw(v=vs.110).aspx
[7] : https://msdn.microsoft.com/en-us/library/4xzx2d41(v=vs.110).aspx
[8] : https://msdn.microsoft.com/en-us/library/ms145129(v=vs.110).aspx
[9] : https://i.stack.imgur.com/TC2Af.png

