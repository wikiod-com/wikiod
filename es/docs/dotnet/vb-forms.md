---
title: "Formularios VB"
slug: "formularios-vb"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Hola mundo en formularios VB.NET
Para mostrar un cuadro de mensaje cuando se ha mostrado el formulario:

    Public Class Form1
        Private Sub Form1_Shown(sender As Object, e As EventArgs) Handles MyBase.Shown
            MessageBox.Show("Hello, World!")
        End Sub
    End Class
    To show a message box before the form has been shown:
    
    Public Class Form1
        Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
            MessageBox.Show("Hello, World!")
        End Sub
    End Class

Load() se llamará primero, y solo una vez, cuando se cargue el formulario por primera vez. Se llamará a Show() cada vez que el usuario inicie el formulario. Se llamará a Activate() cada vez que el usuario active el formulario.

Load() se ejecutará antes de que se llame a Show(), pero tenga cuidado: llamar a msgBox() en show puede hacer que msgBox() se ejecute antes de que finalice Load(). **Por lo general, es una mala idea depender del orden de los eventos entre Load(), Show() y similares.**

## Para principiantes
Algunas cosas que todos los principiantes deben saber / hacer que les ayudarán a tener un buen comienzo con VB .Net:

Configure las siguientes opciones:

    'can be permanently set
    ' Tools / Options / Projects and Soluntions / VB Defaults
    Option Strict On
    Option Explicit On
    Option Infer Off
    
    Public Class Form1
    
    End Class

[Use &, no + para la concatenación de cadenas.][1] [Strings][2] debe estudiarse con cierto detalle, ya que se usan ampliamente.

Dedique algo de tiempo a entender [Tipos de valor y referencia][3].

Nunca use [Application.DoEvents][4]. Preste atención a la 'Precaución'. Cuando llegue a un punto en el que esto parezca algo que debe usar, pregunte.

La [documentación][5] es tu amiga.


[1]: https://msdn.microsoft.com/en-us/library/te2585xw.aspx?f=255&MSPPError=-2147217396
[2]: https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/t63sy5hs.aspx
[4]: https://msdn.microsoft.com/en-us/library/system.windows.forms.application.doevents%28v=vs.110%29.aspx?f=255&MSPPError=-2147217396
[5]: https://social.msdn.microsoft.com/Search/en-US?query=vb%20.net&emptyWatermark=true&searchButtonTooltip=Search%20MSDN&ac=2

## Temporizador de formularios
El componente [Windows.Forms.Timer][1] se puede usar para proporcionar al usuario información que **no** es crítica en cuanto al tiempo. Cree un formulario con un botón, una etiqueta y un componente de temporizador.

Por ejemplo, podría usarse para mostrar al usuario la hora del día periódicamente.

    'can be permanently set
    ' Tools / Options / Projects and Soluntions / VB Defaults
    Option Strict On
    Option Explicit On
    Option Infer Off
    
    Public Class Form1
    
        Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
            Button1.Enabled = False
            Timer1.Interval = 60 * 1000 'one minute intervals
            'start timer
            Timer1.Start()
            Label1.Text = DateTime.Now.ToLongTimeString
        End Sub
    
        Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
            Label1.Text = DateTime.Now.ToLongTimeString
        End Sub
    End Class

Pero este temporizador no es adecuado para el tiempo. Un ejemplo sería usarlo para una cuenta regresiva. En este ejemplo, simularemos una cuenta regresiva de tres minutos. Este puede muy bien ser uno de los ejemplos más aburridamente importantes aquí.

    'can be permanently set
    ' Tools / Options / Projects and Soluntions / VB Defaults
    Option Strict On
    Option Explicit On
    Option Infer Off
    
    Public Class Form1
    
        Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
            Button1.Enabled = False
            ctSecs = 0 'clear count
            Timer1.Interval = 1000 'one second in ms.
            'start timers
            stpw.Reset()
            stpw.Start()
            Timer1.Start()
        End Sub
    
        Dim stpw As New Stopwatch
        Dim ctSecs As Integer
    
        Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
            ctSecs += 1
            If ctSecs = 180 Then 'about 2.5 seconds off on my PC!
                'stop timing
                stpw.Stop()
                Timer1.Stop()
                'show actual elapsed time
                'Is it near 180?
                Label1.Text = stpw.Elapsed.TotalSeconds.ToString("n1")
            End If
        End Sub
    End Class

Después de hacer clic en el botón 1, pasan unos tres minutos y la etiqueta 1 muestra los resultados. ¿Label1 muestra 180? Probablemente no. ¡En mi máquina mostró 182.5!

El motivo de la discrepancia está en la documentación, "El componente del temporizador de Windows Forms es de un solo subproceso y está limitado a una precisión de 55 milisegundos". Esta es la razón por la que no debe usarse para medir el tiempo.

Usando el temporizador y el cronómetro de forma un poco diferente podemos obtener mejores resultados.

    'can be permanently set
    ' Tools / Options / Projects and Soluntions / VB Defaults
    Option Strict On
    Option Explicit On
    Option Infer Off
    
    Public Class Form1
    
        Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
            Button1.Enabled = False
            Timer1.Interval = 100 'one tenth of a second in ms.
            'start timers
            stpw.Reset()
            stpw.Start()
            Timer1.Start()
        End Sub
    
        Dim stpw As New Stopwatch
        Dim threeMinutes As TimeSpan = TimeSpan.FromMinutes(3)
    
        Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
            If stpw.Elapsed >= threeMinutes Then '0.1 off on my PC!
                'stop timing
                stpw.Stop()
                Timer1.Stop()
                'show actual elapsed time
                'how close?
                Label1.Text = stpw.Elapsed.TotalSeconds.ToString("n1")
            End If
        End Sub
    End Class

Hay otros temporizadores que se pueden usar según sea necesario. Esta [búsqueda][2] debería ayudar en ese sentido.


[1]: https://msdn.microsoft.com/en-us/library/system.windows.forms.timer(v=vs.110).aspx
[2]: https://social.msdn.microsoft.com/Search/en-US?query=vb%20.net%20windows%20timers&emptyWatermark=true&searchButtonTooltip=Search%20MSDN&ac=5#refinementChanges=117&pageNumber=1&showMore=false

