---
title: "Formulaires VB"
slug: "formulaires-vb"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Hello World dans les formulaires VB.NET
Pour afficher une boîte de message lorsque le formulaire a été affiché :

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

Load() sera appelé en premier, et une seule fois, lors du premier chargement du formulaire. Show() sera appelé chaque fois que l'utilisateur lancera le formulaire. Activate() sera appelée chaque fois que l'utilisateur rendra le formulaire actif.

Load() s'exécutera avant que Show() ne soit appelé, mais soyez prévenu : l'appel de msgBox() dans show peut entraîner l'exécution de msgBox() avant que Load() ne soit terminé. **C'est généralement une mauvaise idée de dépendre de l'ordre des événements entre Load(), Show() et similaire.**

## Pour les débutants
Certaines choses que tous les débutants devraient savoir / faire qui les aideront à bien démarrer avec VB .Net :

Définissez les options suivantes :

    'can be permanently set
    ' Tools / Options / Projects and Soluntions / VB Defaults
    Option Strict On
    Option Explicit On
    Option Infer Off
    
    Public Class Form1
    
    End Class

[Utilisez &, pas + pour la concaténation de chaînes.][1] Les [chaînes][2] doivent être étudiées en détail car elles sont largement utilisées.

Passez un peu de temps à comprendre [Types de valeur et de référence][3].

N'utilisez jamais [Application.DoEvents][4]. Faites attention à la 'Attention'. Lorsque vous atteignez un point où cela semble être quelque chose que vous devez utiliser, demandez.

La [documentation][5] est votre amie.


[1] : https://msdn.microsoft.com/en-us/library/te2585xw.aspx?f=255&MSPPError=-2147217396
[2] : https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx
[3] : https://msdn.microsoft.com/en-us/library/t63sy5hs.aspx
[4] : https://msdn.microsoft.com/en-us/library/system.windows.forms.application.doevents%28v=vs.110%29.aspx?f=255&MSPPError=-2147217396
[5] : https://social.msdn.microsoft.com/Search/en-US?query=vb%20.net&emptyWatermark=true&searchButtonTooltip=Search%20MSDN&ac=2

## Minuteur de formulaires
Le composant [Windows.Forms.Timer][1] peut être utilisé pour fournir les informations utilisateur qui ne sont **pas** critiques dans le temps. Créez un formulaire avec un bouton, une étiquette et un composant Minuterie.

Par exemple, il pourrait être utilisé pour montrer à l'utilisateur l'heure de la journée périodiquement.

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

Mais cette minuterie n'est pas adaptée au chronométrage. Un exemple serait de l'utiliser pour un compte à rebours. Dans cet exemple, nous allons simuler un compte à rebours jusqu'à trois minutes. Cela pourrait très bien être l'un des exemples les plus ennuyeux ici.

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

Après avoir cliqué sur le bouton 1, environ trois minutes s'écoulent et l'étiquette 1 affiche les résultats. Label1 affiche-t-il 180 ? Probablement pas. Sur ma machine, il affichait 182,5 !

La raison de l'écart se trouve dans la documentation, "Le composant Windows Forms Timer est monothread et est limité à une précision de 55 millisecondes." C'est pourquoi il ne devrait pas être utilisé pour le chronométrage.

En utilisant la minuterie et le chronomètre un peu différemment, nous pouvons obtenir de meilleurs résultats.

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

Il existe d'autres minuteries qui peuvent être utilisées au besoin. Cette [recherche][2] devrait aider à cet égard.


[1] : https://msdn.microsoft.com/en-us/library/system.windows.forms.timer(v=vs.110).aspx
[2] : https://social.msdn.microsoft.com/Search/en-US?query=vb%20.net%20windows%20timers&emptyWatermark=true&searchButtonTooltip=Search%20MSDN&ac=5#refinementChanges=117&pageNumber=1&showMore=false

