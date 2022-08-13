---
title: "ağ"
slug: "ag"
draft: false
images: []
weight: 9931
type: docs
toc: true
---

## Sözdizimi
- TcpClient(dize ana bilgisayar, int bağlantı noktası);

'Client.GetStream()' ile bir 'TcpClient'ten 'NetworkStream'i alabilir ve zaman uyumsuz okuma ve yazma yöntemlerine erişmek için 'StreamReader/StreamWriter'a aktarabilirsiniz.

## Temel TCP İletişim İstemcisi
Bu kod örneği bir TCP istemcisi oluşturur, soket bağlantısı üzerinden "Merhaba Dünya" gönderir ve ardından bağlantıyı kapatmadan önce sunucu yanıtını konsola yazar.

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
    

## Bir web sunucusundan dosya indirin
İnternetten bir dosya indirmek, oluşturma ihtimaliniz olan hemen hemen her uygulamanın gerektirdiği çok yaygın bir iştir.

Bunu başarmak için "[System.Net.WebClient][1]" sınıfını kullanabilirsiniz.

Bunun en basit kullanımı, "kullanma" kalıbı kullanılarak aşağıda gösterilmiştir:

    using (var webClient = new WebClient())
    {
        webClient.DownloadFile("http://www.server.com/file.txt", "C:\\file.txt");
    }

Bu örneğin yaptığı şey, web istemcinizin bittiğinde doğru şekilde temizlendiğinden emin olmak için "kullanarak" kullanır ve yalnızca adlandırılmış kaynağı ilk parametredeki URL'den ikinci parametrede yerel sabit sürücünüzdeki adlandırılmış dosyaya aktarır. parametre.

İlk parametre "[System.Uri][2]" türünde, ikinci parametre "[System.String][3]" türünde

Bu işlevi zaman uyumsuz bir form olarak da kullanabilirsiniz, böylece uygulamanız başka bir şeyle devam ederken, arka planda söner ve indirme işlemini gerçekleştirir, çağrıyı bu şekilde kullanmak modern uygulamalarda yardımcı olduğu için büyük önem taşır. kullanıcı arayüzünüzü duyarlı tutmak için.

Async yöntemlerini kullandığınızda, ilerlemeyi izlemenize izin veren olay işleyicileri bağlayabilirsiniz, böylece örneğin aşağıdaki gibi bir ilerleme çubuğunu güncelleyebilirsiniz:

    var webClient = new WebClient())
    webClient.DownloadFileCompleted += new AsyncCompletedEventHandler(Completed);
    webClient.DownloadProgressChanged += new DownloadProgressChangedEventHandler(ProgressChanged);
    webClient.DownloadFileAsync("http://www.server.com/file.txt", "C:\\file.txt");

Bununla birlikte, Async sürümlerini kullanıyorsanız hatırlamanız gereken önemli bir nokta ve bu, "Onları 'kullanma' sözdiziminde kullanırken çok dikkatli olun".

Bunun nedeni oldukça basittir. Dosya indirme yöntemini çağırdığınızda, hemen geri dönecektir. Buna bir using bloğunda sahipseniz, geri dönüp o bloktan çıkacak ve sınıf nesnesini hemen elden çıkaracak ve böylece devam eden indirme işleminizi iptal edeceksiniz.

Bir Async aktarımı gerçekleştirmenin 'kullanma' yolunu kullanırsanız, aktarım tamamlanana kadar çevreleyen bloğun içinde kaldığınızdan emin olun.


[1]: https://msdn.microsoft.com/en-us/library/system.net.webclient.aspx%22System.Net.WebClient%22
[2]: https://msdn.microsoft.com/en-us/library/system.uri.aspx%22System.Uri%22
[3]: https://msdn.microsoft.com/en-us/library/system.string.aspx%22System.String%22

## Eşzamansız TCP İstemcisi
C# uygulamalarında "async/await" kullanmak çoklu iş parçacığını basitleştirir. Bir TcpClient ile birlikte "async/await"i bu şekilde kullanabilirsiniz.

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

## Temel UDP İstemcisi
Bu kod örneği, bir UDP istemcisi oluşturur ve ardından ağ üzerinden istenen alıcıya "Merhaba Dünya" gönderir. UDP bağlantısız olduğundan ve mesajı ne olursa olsun yayınlayacağından, bir dinleyicinin aktif olması gerekmez. Mesaj gönderildikten sonra, istemcilerin işi yapılır.

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
            

Aşağıda, yukarıdaki istemciyi tamamlayacak bir UDP dinleyicisi örneği verilmiştir. Belirli bir bağlantı noktasındaki trafiği sürekli olarak dinleyecek ve bu verileri konsola yazacaktır. Bu örnek, dahili olarak ayarlanmayan ve dinleyiciyi sonlandırmak ve çıkmak için bunu ayarlamak için bir şeye dayanan bir "bitti" kontrol bayrağı içerir.

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




