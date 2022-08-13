---
title: "Zócalo asíncrono"
slug: "zocalo-asincrono"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

Al usar sockets asíncronos, un servidor puede escuchar las conexiones entrantes y hacer alguna otra lógica mientras tanto en contraste con el socket síncrono cuando están escuchando, bloquean el hilo principal y la aplicación deja de responder y se congelará hasta que un cliente se conecte.







**Socket y red**

¿Cómo acceder a un Servidor fuera de mi propia red?
Esta es una pregunta común y cuando se hace se marca principalmente como tema.

**Lado del servidor**

En la red de su servidor, debe reenviar el puerto de su enrutador a su servidor.

Por ejemplo, PC donde se ejecuta el servidor:

IP local = `192.168.1.115`

El servidor está escuchando el puerto 1234.

Reenviar las conexiones entrantes en el enrutador `Port 1234` a `192.168.1.115`

**Lado del cliente**

Lo único que necesita cambiar es la IP. No desea conectarse a su dirección de bucle invertido, sino a la IP pública de la red en la que se ejecuta su servidor. Esta IP la puedes obtener [aquí][1].

     _connectingSocket.Connect(new IPEndPoint(IPAddress.Parse("10.10.10.10"), 1234));

Así que ahora crea una solicitud en este punto final: `10.10.10.10:1234` si reenvió el puerto de propiedad de su enrutador a su servidor y
el cliente se conectará sin ningún problema.

Si desea conectarse a una IP local, no tendrá que cambiar la dirección de bucle invertido a `192.168.1.178` o algo así.

**Enviando datos:**

Los datos se envían en una matriz de bytes. Debe empaquetar sus datos en una matriz de bytes y descomprimirlos en el otro lado.

Si está familiarizado con el socket, también puede intentar cifrar su matriz de bytes antes de enviarla. Esto evitará que alguien robe su paquete.

[1]: http://whatismyipaddress.com/

## Ejemplo de socket asíncrono (cliente/servidor).
> **Ejemplo del lado del servidor**

**Crear escucha para el servidor**

Comience con la creación de un servidor que manejará los clientes que se conecten y las solicitudes que se enviarán. Así que cree una clase de oyente que se encargue de esto.

    class Listener
    {
        public Socket ListenerSocket; //This is the socket that will listen to any incoming connections
        public short Port = 1234; // on this port we will listen

        public Listener()
        {
            ListenerSocket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        }
     }

        
Primero necesitamos inicializar el socket Listener donde podemos escuchar cualquier conexión. Vamos a usar un Tcp Socket, por eso usamos SocketType.Stream. También especificamos el puerto que el servidor debe escuchar

Luego comenzamos a escuchar cualquier conexión entrante.

**Los métodos de árbol que usamos aquí son:**

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
        
Entonces, cuando un cliente se conecta, podemos aceptarlo por este método:

**Tres métodos que usamos aquí son:**

1. [ListenerSocket.EndAccept()][5]

     We started the callback with `Listener.BeginAccept()` end now we have to end that call back. `The EndAccept()` method accepts an IAsyncResult parameter, this will store the state of the asynchronous method, From this state we can extract the socket where the incoming connection was coming from.

2. `ClienteControlador.AddClient()`

    With the socket we got from `EndAccept()` we create an Client with an own made method *(code ClientController below server example)*. 

3. [Socket de escucha.BeginAccept()][4]

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

Ahora tenemos un conector de escucha, pero ¿cómo recibimos los datos enviados por el cliente? Eso es lo que muestra el siguiente código.

**Crear Server Receiver para cada cliente**

Primero cree una clase de recepción con un constructor que tome un Socket como parámetro:

        public class ReceivePacket
        {
            private byte[] _buffer;
            private Socket _receiveSocket;

            public ReceivePacket(Socket receiveSocket)
            {
               _receiveSocket = receiveSocket;
            }
        }
En el siguiente método, primero comenzamos dando al búfer un tamaño de 4 bytes (Int32) o el paquete contiene partes {longitud, datos reales}. Entonces, los primeros 4 bytes los reservamos para la longitud de los datos, el resto para los datos reales.

A continuación, usamos el método [BeginReceive()][6]. Este método se usa para comenzar a recibir de clientes conectados y cuando reciba datos, ejecutará la función `ReceiveCallback`.

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

Así que hemos configurado un servidor que puede recibir y escuchar conexiones entrantes. Cuando un cliente se conecta, se agregará a una lista de clientes y cada cliente tiene su propia clase de recepción. Para hacer que el servidor escuche:

    Listener listener = new Listener();
    listener.StartListening();

**Algunas clases que uso en este ejemplo**

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


> **Ejemplo del lado del cliente**

**Conectando al servidor**

En primer lugar queremos crear una clase que se conecte al servidor, el nombre que le damos es: Conector:

     
    class Connector
    {
        private Socket _connectingSocket;
    }

El siguiente método para esta clase es TryToConnect()

Este método tiene algunas cosas interesantes:

1. Cree el zócalo;
2. A continuación, hago un bucle hasta que el enchufe esté conectado.
3. Cada bucle es solo mantener el hilo durante 1 segundo, no queremos DOS en el servidor XD
4. Con [Connect()][7] intentará conectarse al servidor. Si falla, lanzará una excepción, pero mantendrá el programa conectado al servidor. Puede usar un método [Connect CallBack][8] para esto, pero solo llamaré a un método cuando el Socket esté conectado.
5. Observe que el Cliente ahora está intentando conectarse a su PC local en el puerto 1234.

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

**Enviando un mensaje al servidor**

Así que ahora tenemos una aplicación casi terminada o Socket. Lo único que no tenemos jet es una Clase para enviar un mensaje al servidor.

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

Finalmente, cree dos botones, uno para conectarse y el otro para enviar un mensaje:

        private void ConnectClick(object sender, EventArgs e)
        {
            Connector tpp = new Connector();
            tpp.TryToConnect();
        }

        private void SendClick(object sender, EventArgs e)
        {
            Client.SendString("Test data from client");
        }


**La clase de cliente que usé en este ejemplo**
 
        public static void SetClient(Socket socket)
        {
            Id = 1;
            Socket = socket;
            Receive = new ReceivePacket(socket, Id);
            SendPacket = new SendPacket(socket);
        }

**Aviso**

La clase de recepción del servidor es la misma que la clase de recepción del cliente.

> **Conclusión**

Ahora tiene un servidor y un cliente. Puedes resolver este ejemplo básico. Por ejemplo, haz que el servidor también pueda recibir archivos u otras cosas. O enviar un mensaje al cliente. En el servidor, tiene una lista de clientes, de modo que cuando reciba algo, sabrá de dónde proviene el cliente.

**Resultado final:**
[![ingrese la descripción de la imagen aquí][9]][9]


[1]: https://msdn.microsoft.com/en-us/library/system.net.sockets.socket.bind(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.net.ipendpoint(v=vs.110).aspx
[3]: https://msdn.microsoft.com/nl-nl/library/system.net.sockets.socket.listen(v=vs.110).aspx
[4]: https://msdn.microsoft.com/en-us/library/5bb431f9(v=vs.110).aspx
[5]: https://msdn.microsoft.com/en-us/library/zdee4kd7(v=vs.110).aspx
[6]: https://msdn.microsoft.com/en-us/library/dxkwh6zw(v=vs.110).aspx
[7]: https://msdn.microsoft.com/en-us/library/4xzx2d41(v=vs.110).aspx
[8]: https://msdn.microsoft.com/en-us/library/ms145129(v=vs.110).aspx
[9]: https://i.stack.imgur.com/TC2Af.png

