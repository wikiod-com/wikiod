---
title: "Minuteries"
slug: "minuteries"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Syntaxe
- `myTimer.Interval` - définit la fréquence à laquelle l'événement "Tick" est appelé (en millisecondes)
- `myTimer.Enabled` - valeur booléenne qui définit la minuterie à activer/désactiver
- `myTimer.Start()` - Démarre le minuteur.
- `myTimer.Stop()` - Arrête le chronomètre.

Si vous utilisez Visual Studio, les minuteurs peuvent être ajoutés en tant que contrôle directement à votre formulaire à partir de la boîte à outils.

## Minuteries multithreads
`System.Threading.Timer` - Minuteur multithread le plus simple. Contient deux méthodes et un constructeur.

Exemple:
Une minuterie appelle la méthode DataWrite, qui écrit "multithread exécuté..." après cinq
secondes se sont écoulées, puis toutes les secondes jusqu'à ce que l'utilisateur appuie sur Entrée :

    using System;
    using System.Threading;
    class Program
    {
      static void Main()
      {
        // First interval = 5000ms; subsequent intervals = 1000ms
        Timer timer = new Timer (DataWrite, "multithread executed...", 5000, 1000);
        Console.ReadLine();
        timer.Dispose(); // This both stops the timer and cleans up.
      }

      static void DataWrite (object data)
      {
        // This runs on a pooled thread
        Console.WriteLine (data); // Writes "multithread executed..."
      }
    }

Remarque : publiera une section distincte pour la suppression des minuteurs multithreads.

`Change` - Cette méthode peut être appelée lorsque vous souhaitez modifier l'intervalle de la minuterie.

`Timeout.Infinite` - Si vous ne voulez tirer qu'une seule fois. Spécifiez ceci dans le dernier argument du constructeur.

`System.Timers` - Une autre classe de minuterie fournie par .NET Framework. Il encapsule le `System.Threading.Timer`.

Traits:
---------

- `IComponent` - lui permettant d'être placé dans la barre d'état des composants du concepteur de Visual Studio
- Propriété `Interval` au lieu d'une méthode `Change`
- `Elapsed` `event` au lieu d'un rappel `delegate`
- Propriété `Enabled` pour démarrer et arrêter le minuteur (`default value = false`)
- Méthodes `Start` et `Stop` au cas où vous seriez confus par la propriété `Enabled` (au-dessus du point)
- `AutoReset` - pour indiquer un événement récurrent (`default value = true`)
- Propriété `SynchronizingObject` avec les méthodes `Invoke` et `BeginInvoke` pour
appeler en toute sécurité des méthodes sur des éléments WPF et des contrôles Windows Forms


Exemple représentant toutes les fonctionnalités ci-dessus :

    using System;
    using System.Timers; // Timers namespace rather than Threading
    class SystemTimer
    {
      static void Main()
      {
        Timer timer = new Timer(); // Doesn't require any args
        timer.Interval = 500;
        timer.Elapsed += timer_Elapsed; // Uses an event instead of a delegate
        timer.Start(); // Start the timer
        Console.ReadLine();
        timer.Stop(); // Stop the timer
        Console.ReadLine();
        timer.Start(); // Restart the timer
        Console.ReadLine();
        timer.Dispose(); // Permanently stop the timer
     }

     static void timer_Elapsed(object sender, EventArgs e)
     {
       Console.WriteLine ("Tick");
     }
    }

`Minuteurs multithreads` - utilisez le pool de threads pour permettre à quelques threads d'en servir plusieurs
minuteries. Cela signifie que la méthode de rappel ou l'événement "Elapsed" peut se déclencher sur un autre
thread à chaque fois qu'il est appelé.

`Elapsed` - cet événement se déclenche toujours à l'heure, que l'événement `Elapsed` précédent ait fini de s'exécuter ou non. Pour cette raison, les rappels ou les gestionnaires d'événements doivent être thread-safe.
La précision des temporisateurs multithread dépend du système d'exploitation et est généralement
dans les 10 à 20 ms.

`interop` - chaque fois que vous avez besoin d'une plus grande précision, utilisez-le et appelez le minuteur multimédia Windows. Cela a une précision jusqu'à 1 ms et est défini dans `winmm.dll`.

`timeBeginPeriod` - Appelez-le d'abord pour informer le système d'exploitation que vous avez besoin d'une précision de synchronisation élevée

`timeSetEvent` - appelez ceci après `timeBeginPeriod` pour démarrer une minuterie multimédia.

`timeKillEvent` - appelez ceci lorsque vous avez terminé, cela arrête le chronomètre

`timeEndPeriod` - Appelez ceci pour informer le système d'exploitation que vous n'avez plus besoin d'une précision de synchronisation élevée.

Vous pouvez trouver sur Internet des exemples complets d'utilisation de la minuterie multimédia en recherchant les mots-clés `dllimport` `winmm.dll` `timesetevent`.

## Création d'une instance d'une minuterie
Les minuteries sont utilisées pour effectuer des tâches à des intervalles de temps spécifiques (faire X toutes les Y secondes)
Vous trouverez ci-dessous un exemple de création d'une nouvelle instance d'un Timer.

**REMARQUE** : Cela s'applique aux minuteries utilisant WinForms. Si vous utilisez WPF, vous pouvez consulter `DispatcherTimer`


        using System.Windows.Forms; //Timers use the Windows.Forms namespace

        public partial class Form1 : Form
        {

            Timer myTimer = new Timer(); //create an instance of Timer named myTimer
   
        
            public Form1()
            {
                InitializeComponent();
            }

        }

## Affectation du gestionnaire d'événements "Tick" à un Timer
Toutes les actions effectuées dans une minuterie sont gérées dans l'événement "Tick".

    public partial class Form1 : Form
    {

        Timer myTimer = new Timer();
   
        
        public Form1()
        {
            InitializeComponent();

            myTimer.Tick += myTimer_Tick; //assign the event handler named "myTimer_Tick"
        }

        private void myTimer_Tick(object sender, EventArgs e)
        {
            // Perform your actions here.
        }
    }

## Exemple : Utilisation d'un minuteur pour effectuer un compte à rebours simple.
        public partial class Form1 : Form
        {

        Timer myTimer = new Timer();
        int timeLeft = 10;
        
            public Form1()
            {
                InitializeComponent();
    
                //set properties for the Timer
                myTimer.Interval = 1000;
                myTimer.Enabled = true;
    
                //Set the event handler for the timer, named "myTimer_Tick"
                myTimer.Tick += myTimer_Tick;
    
                //Start the timer as soon as the form is loaded
                myTimer.Start();
    
                //Show the time set in the "timeLeft" variable
                lblCountDown.Text = timeLeft.ToString();
    
            }

            private void myTimer_Tick(object sender, EventArgs e)
            {
                //perform these actions at the interval set in the properties.
                lblCountDown.Text = timeLeft.ToString();
                timeLeft -= 1;
    
                if (timeLeft < 0)
                {
                    myTimer.Stop();
                }
            }
        }

Résulte en...

[![entrez la description de l'image ici][1]][1][![entrez la description de l'image ici][2]][2]

Etc...


[1] : http://i.stack.imgur.com/VZlnr.png
[2] : http://i.stack.imgur.com/30t8F.png

