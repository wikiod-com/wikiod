---
title: "Portas seriais"
slug: "portas-seriais"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Operação basica
    var serialPort = new SerialPort("COM1", 9600, Parity.Even, 8, StopBits.One);
    serialPort.Open();
    serialPort.WriteLine("Test data");
    string response = serialPort.ReadLine();
    Console.WriteLine(response);
    serialPort.Close();


## Listar nomes de portas disponíveis
    string[] portNames = SerialPort.GetPortNames();

## Leitura assíncrona
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


## Serviço de eco de texto síncrono
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
    

## Receptor de mensagens assíncronas
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

Este programa espera por mensagens contidas em bytes `STX` e `ETX` e gera o texto que vem entre eles. Todo o resto é descartado. No estouro do buffer de gravação, ele para. Em outros erros, ele redefine os buffers de entrada e saída e aguarda outras mensagens.

O código ilustra:
- Leitura de porta serial assíncrona (veja uso de `SerialPort.DataReceived`).
- Processamento de erro de porta serial (veja o uso de `SerialPort.ErrorReceived`).
- Implementação de protocolo baseado em mensagens não-texto.
- Leitura parcial da mensagem.
- O evento `SerialPort.DataReceived` pode ocorrer antes que a mensagem inteira (até `ETX`) chegue. A mensagem inteira também pode não estar disponível no buffer de entrada (SerialPort.Read(..., ..., port.BytesToRead) lê apenas uma parte da mensagem). Neste caso nós armazenamos a parte recebida (`unprocessedBuffer`) e continuamos esperando por mais dados.
- Lidar com várias mensagens que chegam de uma só vez.
- O evento `SerialPort.DataReceived` pode ocorrer somente após várias mensagens terem sido enviadas pela outra ponta.

