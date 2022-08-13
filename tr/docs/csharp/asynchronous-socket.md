---
title: "Asenkron Soket"
slug: "asenkron-soket"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

Eşzamansız soketler kullanarak, bir sunucu gelen bağlantıları dinleyebilir ve ortalama zamanda senkron soketin aksine başka bir mantık yapabilir, dinlerken ana iş parçacığını engeller ve uygulama yanıt vermez hale gelir ve bir istemci bağlanana kadar donar.







**Soket ve ağ**

Kendi ağımın dışındaki bir Sunucuya nasıl erişilir?
Bu yaygın bir sorudur ve sorulduğunda çoğunlukla konuya göre işaretlenir.

**Sunucu Tarafı**

Sunucunuzun ağında, yönlendiricinizi sunucunuza yönlendirmeniz gerekir.

Sunucunun çalıştığı Örnek PC için:

yerel IP = "192.168.1.115"

Sunucu 1234 numaralı bağlantı noktasını dinliyor.

'Port 1234' yönlendiricisindeki gelen bağlantıları '192.168.1.115'e yönlendir

**Müşteri Tarafı**

Değiştirmeniz gereken tek şey IP'dir. Geri döngü adresinize değil, sunucunuzun üzerinde çalıştığı ağdan genel IP'ye bağlanmak istiyorsunuz. Bu IP'yi [buradan][1] alabilirsiniz.

     _connectingSocket.Connect(new IPEndPoint(IPAddress.Parse("10.10.10.10"), 1234));

Şimdi bu uç noktada bir istek oluşturuyorsunuz: `10.10.10.10:1234` özelliği yaptıysanız port yönlendiricinizi sunucunuza yönlendirin ve
istemci herhangi bir sorun olmadan bağlanacaktır.

Yerel bir IP'ye bağlanmak istiyorsanız, geridöngü adresini '192.168.1.178' veya bunun gibi bir şeyle değiştirmek zorunda kalmazsınız.

**Veri gönderiliyor:**

Veriler bayt dizisinde gönderilir. Verilerinizi bir bayt dizisine paketlemeniz ve diğer tarafta paketini açmanız gerekir.

Sokete aşina iseniz, göndermeden önce bayt dizinizi şifrelemeyi de deneyebilirsiniz. Bu, herhangi birinin paketinizi çalmasını önleyecektir.

[1]: http://whatismyipaddress.com/

## Asenkron Soket (İstemci / Sunucu) örneği.
> **Sunucu Tarafı örneği**

**Sunucu için Dinleyici oluştur**

Bağlanan istemcileri ve gönderilecek istekleri idare edecek bir sunucu oluşturmaya başlayın. Bu yüzden, bunu halledecek bir Dinleyici Sınıfı oluşturun.

    class Listener
    {
        public Socket ListenerSocket; //This is the socket that will listen to any incoming connections
        public short Port = 1234; // on this port we will listen

        public Listener()
        {
            ListenerSocket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        }
     }

        
Öncelikle, herhangi bir bağlantı için dinleyebileceğimiz Listener soketini başlatmamız gerekiyor. Bir Tcp Socket kullanacağız, bu yüzden SocketType.Stream kullanıyoruz. Ayrıca sunucunun dinlemesi gereken cadı portunu belirtiyoruz

Ardından gelen bağlantıları dinlemeye başlarız.

**Burada kullandığımız ağaç yöntemleri şunlardır:**

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
        
Böylece bir istemci bağlandığında onları şu yöntemle kabul edebiliriz:

**Burada kullandığımız üç yöntem şunlardır:**

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

Şimdi bir Dinleme Soketimiz var ama istemci tarafından gönderilen verileri nasıl alıyoruz, bir sonraki kodun gösterdiği şey bu.

**Her istemci için Sunucu Alıcısı oluşturun**

İlk önce, bir Socket'i parametre olarak alan bir kurucu ile bir alma sınıfı oluşturun:

        public class ReceivePacket
        {
            private byte[] _buffer;
            private Socket _receiveSocket;

            public ReceivePacket(Socket receiveSocket)
            {
               _receiveSocket = receiveSocket;
            }
        }
Bir sonraki yöntemde ilk olarak arabelleğe 4 bayt (Int32) boyutunda veya paketin içerdiği kısımlara {uzunluk, gerçek veri} vererek başlıyoruz. Bu nedenle, verilerin uzunluğu için ayırdığımız ilk 4 bayt, gerçek veriler için geri kalanı.

Sonra [BeginReceive()][6] yöntemini kullanıyoruz. Bu yöntem, bağlı istemcilerden almaya başlamak için kullanılır ve veri alacağı zaman `ReceiveCallback` işlevini çalıştırır.

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

Bu yüzden gelen bağlantıları alabilen ve dinleyebilen bir sunucu kurduk. Bir istemci bağlandığında, bir istemci listesine eklenecektir ve her istemcinin kendi alıcı sınıfı vardır. Sunucunun dinlemesini sağlamak için:

    Listener listener = new Listener();
    listener.StartListening();

**Bu örnekte kullandığım bazı Sınıflar**

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


> **İstemci Tarafı örneği**

**Sunucuya baglanıyor**

Öncelikle bir sınıf oluşturmak istiyoruz ona verdiğimiz sunucuya bağlanan isim: Bağlayıcı:

     
    class Connector
    {
        private Socket _connectingSocket;
    }

Bu sınıf için sonraki Yöntem TryToConnect()

Bu yöntemde birkaç ilginç şey var:

1. Soketi oluşturun;
2. Sonra soket bağlanana kadar döngü yapıyorum
3. Her döngüde İpliği 1 saniye tutuyor, sunucuya DOS yapmak istemiyoruz XD
4. [Connect()][7] ile sunucuya bağlanmaya çalışacaktır. Başarısız olursa bir istisna atar, ancak hile programı sunucuya bağlanmaya devam eder. Bunun için [Connect CallBack][8] yöntemini kullanabilirsiniz, ancak ben sadece Socket bağlıyken bir yöntemi çağırmaya gideceğim.
5. İstemcinin şimdi yerel bilgisayarınıza 1234 numaralı bağlantı noktasından bağlanmaya çalıştığına dikkat edin.

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

**Sunucuya mesaj gönderme**

Artık neredeyse bitirmek veya Soket uygulamamız var. Jetimiz olmayan tek şey sunucuya mesaj göndermek için bir Sınıf.

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

Son olarak, biri bağlanmak, diğeri mesaj göndermek için iki düğme oluşturun:

        private void ConnectClick(object sender, EventArgs e)
        {
            Connector tpp = new Connector();
            tpp.TryToConnect();
        }

        private void SendClick(object sender, EventArgs e)
        {
            Client.SendString("Test data from client");
        }


**Bu örnekte kullandığım istemci sınıfı**
 
        public static void SetClient(Socket socket)
        {
            Id = 1;
            Socket = socket;
            Receive = new ReceivePacket(socket, Id);
            SendPacket = new SendPacket(socket);
        }

**Fark etme**

Sunucudan Alma Sınıfı, istemciden alma sınıfıyla aynıdır.

> **Sonuç**

Artık bir sunucunuz ve bir istemciniz var. Bu temel örnek üzerinde çalışabilirsiniz. Örneğin, sunucunun dosyaları veya diğer bilgileri almasını sağlayın. Veya müşteriye bir mesaj gönderin. Sunucuda bir istemci listesi var, bu nedenle bir şey aldığınızda istemciden geldiğini bileceksiniz.

**Son sonuç:**
[![buraya resim açıklamasını girin][9]][9]


[1]: https://msdn.microsoft.com/en-us/library/system.net.sockets.socket.bind(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.net.ipendpoint(v=vs.110).aspx
[3]: https://msdn.microsoft.com/nl-nl/library/system.net.sockets.socket.listen(v=vs.110).aspx
[4]: https://msdn.microsoft.com/en-us/library/5bb431f9(v=vs.110).aspx
[5]: https://msdn.microsoft.com/en-us/library/zdee4kd7(v=vs.110).aspx
[6]: https://msdn.microsoft.com/en-us/library/dxkwh6zw(v=vs.110).aspx
[7]: https://msdn.microsoft.com/en-us/library/4xzx2d41(v=vs.110).aspx
[8]: https://msdn.microsoft.com/en-us/library/ms145129(v=vs.110).aspx
[9]: https://i.stack.imgur.com/TC2Af.png

