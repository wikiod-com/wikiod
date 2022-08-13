---
title: "VB Formları"
slug: "vb-formlar"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## VB.NET Formlarında Merhaba Dünya
Form gösterildiğinde bir mesaj kutusu göstermek için:

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

Load(), form ilk yüklendiğinde önce ve yalnızca bir kez çağrılır. Show(), kullanıcı formu her başlattığında çağrılır. Activate(), kullanıcı formu her etkinleştirdiğinde çağrılır.

Load(), Show() çağrılmadan önce yürütülür, ancak uyarılmalıdır: show'da msgBox() öğesinin çağrılması, Load() tamamlanmadan önce msgBox() öğesinin yürütülmesine neden olabilir. **Load(), Show() ve benzerleri arasında olay sıralamasına bağlı olmak genellikle kötü bir fikirdir.**

## Yeni başlayanlar için
Tüm yeni başlayanların bilmesi/yapması gereken, VB .Net ile iyi bir başlangıç ​​yapmalarına yardımcı olacak bazı şeyler:

Aşağıdaki Seçenekleri ayarlayın:

    'can be permanently set
    ' Tools / Options / Projects and Soluntions / VB Defaults
    Option Strict On
    Option Explicit On
    Option Infer Off
    
    Public Class Form1
    
    End Class

[Dize bitiştirme için + değil & kullanın.][1] [Dizeler][2], yaygın olarak kullanıldıkları için biraz ayrıntılı olarak incelenmelidir.

[Değer ve Referans Türlerini][3] anlamak için biraz zaman ayırın.

[Application.DoEvents][4] asla kullanmayın. 'Dikkat' kısmına dikkat edin. Bunun kullanmanız gereken bir şey gibi göründüğü bir noktaya geldiğinizde, sorun.

[belgeler][5] arkadaşınızdır.


[1]: https://msdn.microsoft.com/en-us/library/te2585xw.aspx?f=255&MSPPError=-2147217396
[2]: https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/t63sy5hs.aspx
[4]: https://msdn.microsoft.com/en-us/library/system.windows.forms.application.doevents%28v=vs.110%29.aspx?f=255&MSPPError=-2147217396
[5]: https://social.msdn.microsoft.com/Search/en-US?query=vb%20.net&emptyWatermark=true&searchButtonTooltip=Search%20MSDN&ac=2

## Form Zamanlayıcısı
[Windows.Forms.Timer][1] bileşeni, zaman açısından kritik olmayan** kullanıcı bilgilerini sağlamak için kullanılabilir. Tek düğme, tek etiket ve Zamanlayıcı bileşeni içeren bir form oluşturun.

Örneğin, kullanıcıya periyodik olarak günün saatini göstermek için kullanılabilir.

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

Ancak bu zamanlayıcı zamanlama için uygun değildir. Bir örnek, bir geri sayım için kullanmak olabilir. Bu örnekte, üç dakikaya kadar bir geri sayımı simüle edeceğiz. Bu, buradaki en sıkıcı derecede önemli örneklerden biri olabilir.

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

Düğme1 tıklandıktan sonra yaklaşık üç dakika geçer ve etiket1 sonuçları gösterir. etiket1 180 gösteriyor mu? Muhtemelen değil. Makinemde 182.5 gösterdi!

Tutarsızlığın nedeni, "Windows Forms Zamanlayıcı bileşeni tek iş parçacıklıdır ve 55 milisaniyelik bir doğrulukla sınırlıdır." Bu yüzden zamanlama için kullanılmamalıdır.

Zamanlayıcı ve kronometreyi biraz farklı kullanarak daha iyi sonuçlar elde edebiliriz.

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

Gerektiğinde kullanılabilecek başka zamanlayıcılar da vardır. Bu [arama][2] bu konuda yardımcı olmalıdır.


[1]: https://msdn.microsoft.com/en-us/library/system.windows.forms.timer(v=vs.110).aspx
[2]: https://social.msdn.microsoft.com/Search/en-US?query=vb%20.net%20windows%20timers&emptyWatermark=true&searchButtonTooltip=Search%20MSDN&ac=5#refinementChanges=117&pageNumber=1&showMore=false

