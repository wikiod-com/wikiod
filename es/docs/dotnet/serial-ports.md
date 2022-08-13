---
title: "Puertos seriales"
slug: "puertos-seriales"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Operación básica
    var serialPort = new SerialPort("COM1", 9600, Parity.Even, 8, StopBits.One);
    serialPort.Open();
    serialPort.WriteLine("Test data");
    string response = serialPort.ReadLine();
    Console.WriteLine(response);
    serialPort.Close();


## Lista de nombres de puertos disponibles
    string[] portNames = SerialPort.GetPortNames();

## Lectura asíncrona
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


## Servicio de eco de texto síncrono
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
    

## Receptor de mensajes asíncronos
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

Este programa espera los mensajes incluidos en los bytes `STX` y `ETX` y genera el texto que se encuentra entre ellos. Todo lo demás se descarta. En el desbordamiento del búfer de escritura, se detiene. En otros errores, restablece los búferes de entrada y salida y espera más mensajes.

El código ilustra:
- Lectura asíncrona del puerto serie (consulte el uso de `SerialPort.DataReceived`).
- Procesamiento de errores del puerto serie (consulte el uso de `SerialPort.ErrorReceived`).
- Implementación de protocolo basado en mensajes no de texto.
- Lectura parcial de mensajes.
- El evento `SerialPort.DataReceived` puede ocurrir antes de que llegue el mensaje completo (hasta `ETX`). Es posible que el mensaje completo no esté disponible en el búfer de entrada (SerialPort.Read(..., ..., port.BytesToRead) lee solo una parte del mensaje). En este caso guardamos la parte recibida (`unprocessedBuffer`) y seguimos esperando más datos.
- Tratar con varios mensajes que llegan de una sola vez.
- El evento `SerialPort.DataReceived` puede ocurrir solo después de que el otro extremo haya enviado varios mensajes.

