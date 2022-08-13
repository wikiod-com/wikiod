---
title: "Xamarin.Android - Bluetooth communication"
slug: "xamarinandroid---bluetooth-communication"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

In **Xamarin.Android** the **BluetoothSocket.InputStream** and **BluetoothSocket.OutputStream** properties are by design automatically converted to **System.IO.Stream**.
In case of so called interactive communication protocol, when server responds only when client talks to it, System.IO.Stream is not good because it has no method or property to get the number of available response bytes before reading the response.

## Parameters
| Parameter | Details |
| ------ | ------ |
| socket   | An instance of BluetoothSocket object. Socket must be opened before call this method.   |
| cmd   | Command as a byte array to send to BT device.   |
| _mx   | Since this method uses a hardware resource, it is better to call it from a separate worker thread. This parameter is an instance of System.Threading.Mutex object and is used to synchronize the thread with other threads optionally calling this method.   |
| timeOut   | Wait time in milliseconds between Write and Read operations.   |


## Send and receive data from and to bluetooth device using socket
The below example uses [Android.Runtime.InputStreamInvoker][1] and [Android.Runtime.OutputStreamInvoker][2] types obtain [Java.IO.InputStream][3] and [Java.IO.OutputStream][4].
Once we have a **Java.IO.InputStream** instance, we can use its **.Available()** method to get the number of available response bytes which we can use in **.Read()** method:

<!-- language: c# -->
    byte[] Talk2BTsocket(BluetoothSocket socket, byte[] cmd, Mutex _mx, int timeOut = 150)
    {
        var buf = new byte[0x20];
    
        _mx.WaitOne();
        try
        {
            using (var ost = socket.OutputStream)
            {
                var _ost = (ost as OutputStreamInvoker).BaseOutputStream;
                _ost.Write(cmd, 0, cmd.Length);
            }
    
            // needed because when skipped, it can cause no or invalid data on input stream
            Thread.Sleep(timeOut);
    
            using (var ist = socket.InputStream)
            {
                var _ist = (ist as InputStreamInvoker).BaseInputStream;
                var aa = 0;
                if ((aa = _ist.Available()) > 0)
                {
                    var nn = _ist.Read(buf, 0, aa);
                    System.Array.Resize(ref buf, nn);
                }
            }
        }
        catch (System.Exception ex)
        {
            DisplayAlert(ex.Message);
        }
        finally
        {
            _mx.ReleaseMutex();     // must be called here !!!
        }
    
        return buf;
    }


  [1]: https://developer.xamarin.com/api/type/Android.Runtime.InputStreamInvoker/
  [2]: https://developer.xamarin.com/api/type/Android.Runtime.OutputStreamInvoker/
  [3]: https://developer.xamarin.com/api/type/Java.IO.InputStream/
  [4]: https://developer.xamarin.com/api/type/Java.IO.OutputStream/

