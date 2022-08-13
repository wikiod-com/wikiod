---
title: "Ports série"
slug: "ports-serie"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Opération de base
    var serialPort = new SerialPort("COM1", 9600, Parity.Even, 8, StopBits.One);
    serialPort.Open();
    serialPort.WriteLine("Test data");
    string response = serialPort.ReadLine();
    Console.WriteLine(response);
    serialPort.Close();


## Liste les noms de ports disponibles
    string[] portNames = SerialPort.GetPortNames();

## Lecture asynchrone
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


## Service d'écho de texte synchrone
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
    

## Récepteur de message asynchrone
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

Ce programme attend les messages contenus dans les octets `STX` et `ETX` et affiche le texte venant entre eux. Tout le reste est jeté. En cas de débordement du tampon d'écriture, il s'arrête. Sur d'autres erreurs, il réinitialise les tampons d'entrée et de sortie et attend d'autres messages.

Le code illustre :
- Lecture de port série asynchrone (voir utilisation de `SerialPort.DataReceived`).
- Traitement des erreurs de port série (voir l'utilisation de `SerialPort.ErrorReceived`).
- Implémentation d'un protocole basé sur des messages non textuels.
- Lecture partielle des messages.
- L'événement `SerialPort.DataReceived` peut se produire avant que le message entier (jusqu'à `ETX`) n'arrive. Le message entier peut également ne pas être disponible dans le tampon d'entrée (SerialPort.Read(..., ..., port.BytesToRead) ne lit qu'une partie du message). Dans ce cas, nous stockons la partie reçue (`unprocessedBuffer`) et continuons d'attendre d'autres données.
- Traiter plusieurs messages entrant en une seule fois.
- L'événement `SerialPort.DataReceived` ne peut se produire qu'après l'envoi de plusieurs messages par l'autre extrémité.

