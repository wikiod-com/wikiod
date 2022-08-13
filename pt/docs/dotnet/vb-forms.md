---
title: "Formulários VB"
slug: "formularios-vb"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Olá Mundo em Formulários VB.NET
Para mostrar uma caixa de mensagem quando o formulário foi exibido:

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

Load() será chamado primeiro, e apenas uma vez, quando o formulário for carregado pela primeira vez. Show() será chamado toda vez que o usuário iniciar o formulário. Activate() será chamado toda vez que o usuário tornar o formulário ativo.

Load() será executado antes que Show() seja chamado, mas esteja avisado: chamar msgBox() em show pode fazer com que msgBox() seja executado antes que Load() seja concluído. **Geralmente é uma má ideia depender da ordenação de eventos entre Load(), Show() e similares.**

## Para iniciantes
Algumas coisas que todos os iniciantes devem saber/fazer que os ajudarão a ter um bom começo com VB .Net:

Defina as seguintes opções:

    'can be permanently set
    ' Tools / Options / Projects and Soluntions / VB Defaults
    Option Strict On
    Option Explicit On
    Option Infer Off
    
    Public Class Form1
    
    End Class

[Use &, não + para concatenação de strings.][1] [Strings][2] devem ser estudados com algum detalhe, pois são amplamente usados.

Passe algum tempo entendendo [Tipos de valor e referência][3].

Nunca use [Application.DoEvents][4]. Preste atenção ao 'Cuidado'. Quando você chegar a um ponto em que isso parece ser algo que você deve usar, pergunte.

A [documentação][5] é sua amiga.


[1]: https://msdn.microsoft.com/en-us/library/te2585xw.aspx?f=255&MSPPError=-2147217396
[2]: https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/t63sy5hs.aspx
[4]: https://msdn.microsoft.com/en-us/library/system.windows.forms.application.doevents%28v=vs.110%29.aspx?f=255&MSPPError=-2147217396
[5]: https://social.msdn.microsoft.com/Search/en-US?query=vb%20.net&emptyWatermark=true&searchButtonTooltip=Search%20MSDN&ac=2

## Temporizador de formulários
O componente [Windows.Forms.Timer][1] pode ser usado para fornecer as informações do usuário que **não** são críticas em termos de tempo. Crie um formulário com um botão, um rótulo e um componente Timer.

Por exemplo, pode ser usado para mostrar ao usuário a hora do dia periodicamente.

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

Mas este temporizador não é adequado para temporização. Um exemplo seria usá-lo para uma contagem regressiva. Neste exemplo vamos simular uma contagem regressiva para três minutos. Este pode muito bem ser um dos exemplos mais chatos e importantes aqui.

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

Depois que o botão1 é clicado, cerca de três minutos se passam e o rótulo1 mostra os resultados. O label1 mostra 180? Provavelmente não. Na minha máquina mostrou 182,5!

O motivo da discrepância está na documentação, "O componente Windows Forms Timer é de thread único e está limitado a uma precisão de 55 milissegundos". É por isso que não deve ser usado para cronometragem.

Usando o cronômetro e o cronômetro de maneira um pouco diferente, podemos obter melhores resultados.

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

Existem outros temporizadores que podem ser usados ​​conforme necessário. Esta [pesquisa][2] deve ajudar nesse sentido.


[1]: https://msdn.microsoft.com/en-us/library/system.windows.forms.timer(v=vs.110).aspx
[2]: https://social.msdn.microsoft.com/Search/en-US?query=vb%20.net%20windows%20timers&emptyWatermark=true&searchButtonTooltip=Search%20MSDN&ac=5#refinementChanges=117&pageNumber=1&showMore=false

