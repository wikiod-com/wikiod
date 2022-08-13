---
title: "Seri bağlantı girişleri"
slug: "seri-baglant-girisleri"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Temel operasyon
    var serialPort = new SerialPort("COM1", 9600, Parity.Even, 8, StopBits.One);
    serialPort.Open();
    serialPort.WriteLine("Test data");
    string response = serialPort.ReadLine();
    Console.WriteLine(response);
    serialPort.Close();


## Kullanılabilir bağlantı noktası adlarını listeleyin
    string[] portNames = SerialPort.GetPortNames();

## Eşzamansız okuma
    void SetupAsyncRead(SerialPort serialPort)
    {
        serialPort.DataReceived += (sender, e) => {
            byte[] buffer = new byte[4096];
            switch (e.EventType)
            {
                case SerialData.Chars:
                    var port = (SerialPort)sender;
                    int bytesToRead = port.BytesToRead;
                    if (bytesToRead > buffer.Length)
                        Array.Resize(ref buffer, bytesToRead);
                    int bytesRead = port.Read(buffer, 0, bytesToRead);
                    // Process the read buffer here
                    // ...
                    break;
                case SerialData.Eof:
                    // Terminate the service here
                    // ...
                    break;
            }
        };


## Senkronize metin yankı hizmeti
    using System.IO.Ports;
    
    namespace TextEchoService
    {
        class Program
        {
            static void Main(string[] args)
            {
                var serialPort = new SerialPort("COM1", 9600, Parity.Even, 8, StopBits.One);
                serialPort.Open();
                string message = "";
                while (message != "quit")
                {
                    message = serialPort.ReadLine();
                    serialPort.WriteLine(message);
                }
                serialPort.Close();
            }
        }
    }
    

## Asenkron mesaj alıcısı
    using System;
    using System.Collections.Generic;
    using System.IO.Ports;
    using System.Text;
    using System.Threading;
    
    namespace AsyncReceiver
    {
        class Program
        {
            const byte STX = 0x02;
            const byte ETX = 0x03;
            const byte ACK = 0x06;
            const byte NAK = 0x15;
            static ManualResetEvent terminateService = new ManualResetEvent(false);
            static readonly object eventLock = new object();
            static List<byte> unprocessedBuffer = null;
    
            static void Main(string[] args)
            {
                try
                {
                    var serialPort = new SerialPort("COM11", 9600, Parity.Even, 8, StopBits.One);
                    serialPort.DataReceived += DataReceivedHandler;
                    serialPort.ErrorReceived += ErrorReceivedHandler;
                    serialPort.Open();
                    terminateService.WaitOne();
                    serialPort.Close();
                }
                catch (Exception e)
                {
                    Console.WriteLine("Exception occurred: {0}", e.Message);
                }
                Console.ReadKey();
            }
    
            static void DataReceivedHandler(object sender, SerialDataReceivedEventArgs e)
            {
                lock (eventLock)
                {
                    byte[] buffer = new byte[4096];
                    switch (e.EventType)
                    {
                        case SerialData.Chars:
                            var port = (SerialPort)sender;
                            int bytesToRead = port.BytesToRead;
                            if (bytesToRead > buffer.Length)
                                Array.Resize(ref buffer, bytesToRead);
                            int bytesRead = port.Read(buffer, 0, bytesToRead);
                            ProcessBuffer(buffer, bytesRead);
                            break;
                        case SerialData.Eof:
                            terminateService.Set();
                            break;
                    }
                }
            }
            static void ErrorReceivedHandler(object sender, SerialErrorReceivedEventArgs e)
            {
                lock (eventLock)
                    if (e.EventType == SerialError.TXFull)
                    {
                        Console.WriteLine("Error: TXFull. Can't handle this!");
                        terminateService.Set();
                    }
                    else
                    {
                        Console.WriteLine("Error: {0}. Resetting everything", e.EventType);
                        var port = (SerialPort)sender;
                        port.DiscardInBuffer();
                        port.DiscardOutBuffer();
                        unprocessedBuffer = null;
                        port.Write(new byte[] { NAK }, 0, 1);
                    }
            }
    
            static void ProcessBuffer(byte[] buffer, int length)
            {
                List<byte> message = unprocessedBuffer;
                for (int i = 0; i < length; i++)
                    if (buffer[i] == ETX)
                    {
                        if (message != null)
                        {
                            Console.WriteLine("MessageReceived: {0}", 
                                Encoding.ASCII.GetString(message.ToArray()));
                            message = null;
                        }
                    }
                    else if (buffer[i] == STX)
                        message = null;
                    else if (message != null)
                        message.Add(buffer[i]);
                unprocessedBuffer = message;
            }
        }
    }

Bu program `STX` ve `ETX` baytları içine alınmış mesajları bekler ve aralarında gelen metni çıkarır. Diğer her şey atılır. Yazma arabelleği taşması durumunda durur. Diğer hatalarda giriş ve çıkış arabelleklerini sıfırlar ve sonraki mesajları bekler.

Kod şunları gösterir:
- Asenkron seri port okuması (bkz. `SerialPort.DataReceived` kullanımı).
- Seri port hatası işleme (bkz. `SerialPort.ErrorReceived` kullanımı).
- Metin mesajı tabanlı olmayan protokol uygulaması.
- Kısmi mesaj okuma.
- 'SerialPort.DataReceived' olayı, mesajın tamamı ('ETX'e kadar) gelmeden önce gerçekleşebilir. İletinin tamamı giriş arabelleğinde de mevcut olmayabilir (SerialPort.Read(..., ..., port.BytesToRead) iletinin yalnızca bir bölümünü okur). Bu durumda alınan kısmı ('işlenmemiş Buffer') saklarız ve daha fazla veri beklemeye devam ederiz.
- Tek seferde gelen birkaç mesajla ilgilenmek.
- `SerialPort.DataReceived` olayı ancak diğer uç tarafından birkaç mesaj gönderildikten sonra gerçekleşebilir.

