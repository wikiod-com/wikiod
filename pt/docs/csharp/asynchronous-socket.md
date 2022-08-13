---
title: "Soquete Assíncrono"
slug: "soquete-assincrono"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

Ao usar soquetes assíncronos, um servidor pode ouvir as conexões de entrada e fazer alguma outra lógica nesse meio tempo, em contraste com o soquete síncrono, quando eles estão ouvindo, eles bloqueiam o thread principal e o aplicativo não responde e congela até que um cliente se conecte.







**Soquete e rede**

Como acessar um Servidor fora da minha própria rede?
Essa é uma pergunta comum e, quando é feita, geralmente é sinalizada como do tópico.

**Lado do servidor**

Na rede do seu servidor, você precisa encaminhar o roteador para o servidor.

Por exemplo, PC em que o servidor está sendo executado:

IP local = `192.168.1.115`

O servidor está escutando a porta 1234.

Encaminhar conexões de entrada no roteador `Porta 1234` para `192.168.1.115`

**Lado do cliente**

A única coisa que você precisa mudar é o IP. Você não deseja se conectar ao seu endereço de loopback, mas ao IP público da rede em que seu servidor está sendo executado. Este IP você pode obter [aqui][1].

     _connectingSocket.Connect(new IPEndPoint(IPAddress.Parse("10.10.10.10"), 1234));

Então agora você cria uma solicitação neste endpoint: `10.10.10.10:1234` se você fez a porta de propriedade encaminhar seu roteador para seu servidor e
cliente se conectará sem nenhum problema.

Se você quiser se conectar a um IP local, não precisará fazer o portforwart, basta alterar o endereço de loopback para `192.168.1.178` ou algo assim.

**Enviando dados:**

Os dados são enviados em uma matriz de bytes. Você precisa empacotar seus dados em uma matriz de bytes e descompactá-los do outro lado.

Se você estiver familiarizado com socket, você também pode tentar criptografar sua matriz de bytes antes de enviar. Isso impedirá que alguém roube seu pacote.

[1]: http://whatismyipaddress.com/

## Exemplo de soquete assíncrono (cliente/servidor).
> **Exemplo do lado do servidor**

**Criar Ouvinte para o servidor**

Comece criando um servidor que lidará com clientes que se conectam e solicitações que serão enviadas. Portanto, crie uma classe de ouvinte que lidará com isso.

    class Listener
    {
        public Socket ListenerSocket; //This is the socket that will listen to any incoming connections
        public short Port = 1234; // on this port we will listen

        public Listener()
        {
            ListenerSocket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        }
     }

        
Primeiro precisamos inicializar o socket Listener onde podemos escutar qualquer conexão. Vamos usar um Socket Tcp, por isso usamos SocketType.Stream. Também especificamos a porta de bruxa que o servidor deve ouvir

Em seguida, começamos a ouvir todas as conexões de entrada.

**Os métodos de árvore que usamos aqui são:**

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
        
Então, quando um cliente se conecta, podemos aceitá-lo por este método:

**Três métodos que usamos aqui são:**

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

Agora temos um Listening Socket, mas como recebemos os dados enviados pelo cliente é isso que o próximo código está mostrando.

**Crie um receptor de servidor para cada cliente**

Primeiro crie uma classe de recebimento com um construtor que receba um Socket como parâmetro:

        public class ReceivePacket
        {
            private byte[] _buffer;
            private Socket _receiveSocket;

            public ReceivePacket(Socket receiveSocket)
            {
               _receiveSocket = receiveSocket;
            }
        }
No próximo método, começamos dando ao buffer um tamanho de 4 bytes (Int32) ou o pacote contém partes {comprimento, dados reais}. Assim, os primeiros 4 bytes reservamos para o comprimento dos dados e o restante para os dados reais.

Em seguida, usamos o método [BeginReceive()][6]. Este método é usado para iniciar o recebimento de clientes conectados e quando ele for receber dados ele executará a função `ReceiveCallback`.

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

Então nós configuramos um servidor que pode receber e escutar conexões de entrada. Quando um cliente se conectar, ele será adicionado a uma lista de clientes e cada cliente terá sua própria classe de recebimento. Para fazer o servidor escutar:

    Listener listener = new Listener();
    listener.StartListening();

**Algumas classes que uso neste exemplo**

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


> **Exemplo do lado do cliente**

**Conectando ao servidor**

Antes de tudo queremos criar uma classe que se conecta ao servidor, o nome que damos a ele é: Conector:

     
    class Connector
    {
        private Socket _connectingSocket;
    }

O próximo método para esta classe é TryToConnect()

Este método tem algumas coisas interessantes:

1. Crie o soquete;
2. Em seguida, faço um loop até que o soquete esteja conectado
3. A cada loop é só segurar o Thread por 1 segundo não queremos DOS no servidor XD
4. Com [Connect()][7] ele tentará se conectar ao servidor. Se falhar, lançará uma exceção, mas o truque manterá o programa conectado ao servidor. Você pode usar um método [Connect CallBack][8] para isso, mas vou apenas chamar um método quando o Socket estiver conectado.
5. Observe que o Cliente está tentando se conectar ao seu PC local na porta 1234.

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

**Enviando uma mensagem para o servidor**

Então agora temos uma aplicação quase finalizada ou Socket. A única coisa que não temos jet é uma classe para enviar uma mensagem para o servidor.

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

Por fim, crie dois botões, um para conectar e outro para enviar uma mensagem:

        private void ConnectClick(object sender, EventArgs e)
        {
            Connector tpp = new Connector();
            tpp.TryToConnect();
        }

        private void SendClick(object sender, EventArgs e)
        {
            Client.SendString("Test data from client");
        }


**A classe de cliente que usei neste exemplo**
 
        public static void SetClient(Socket socket)
        {
            Id = 1;
            Socket = socket;
            Receive = new ReceivePacket(socket, Id);
            SendPacket = new SendPacket(socket);
        }

**Perceber**

A classe de recebimento do servidor é a mesma que a classe de recebimento do cliente.

> **Conclusão**

Agora você tem um servidor e um cliente. Você pode trabalhar este exemplo básico. Por exemplo, faça com que o servidor também possa receber arquivos ou outras coisas. Ou envie uma mensagem para o cliente. No servidor você tem uma lista de clientes, então quando você receber algo você saberá com o cliente de onde veio.

**Resultado final:**
[![digite a descrição da imagem aqui][9]][9]


[1]: https://msdn.microsoft.com/en-us/library/system.net.sockets.socket.bind(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.net.ipendpoint(v=vs.110).aspx
[3]: https://msdn.microsoft.com/nl-nl/library/system.net.sockets.socket.listen(v=vs.110).aspx
[4]: https://msdn.microsoft.com/en-us/library/5bb431f9(v=vs.110).aspx
[5]: https://msdn.microsoft.com/en-us/library/zdee4kd7(v=vs.110).aspx
[6]: https://msdn.microsoft.com/en-us/library/dxkwh6zw(v=vs.110).aspx
[7]: https://msdn.microsoft.com/en-us/library/4xzx2d41(v=vs.110).aspx
[8]: https://msdn.microsoft.com/en-us/library/ms145129(v=vs.110).aspx
[9]: https://i.stack.imgur.com/TC2Af.png

