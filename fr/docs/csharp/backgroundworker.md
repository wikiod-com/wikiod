---
title: "ContexteTravailleur"
slug: "contextetravailleur"
draft: false
images: []
weight: 9869
type: docs
toc: true
---

## Syntaxe

- `bgWorker.CancellationPending //retourne si le bgWorker a été annulé pendant son opération `

- `bgWorker.IsBusy //retourne vrai si le bgWorker est au milieu d'une opération`

- `bgWorker.ReportProgress(int x) //Rapporte un changement en cours. Déclenche l'événement "ProgressChanged"`

- `bgWorker.RunWorkerAsync() //Démarre le BackgroundWorker en déclenchant l'événement "DoWork"`

- `bgWorker.CancelAsync() //ordonne à BackgroundWorker de s'arrêter après l'achèvement d'une tâche.`

L'exécution d'opérations de longue durée dans le thread d'interface utilisateur peut empêcher votre application de répondre, ce qui donne l'impression à l'utilisateur qu'elle a cessé de fonctionner. Il est préférable que ces tâches soient exécutées sur un thread d'arrière-plan. Une fois terminée, l'interface utilisateur peut être mise à jour.

Apporter des modifications à l'interface utilisateur pendant le fonctionnement de BackgroundWorker nécessite d'invoquer les modifications apportées au thread d'interface utilisateur, généralement à l'aide de [Control.Invoke](https://msdn.microsoft.com/en-us/library/system.windows.forms. control.invoke(v=vs.110).aspx) sur le contrôle que vous mettez à jour. Si vous ne le faites pas, votre programme lancera une exception.

Le BackgroundWorker est généralement utilisé uniquement dans les applications Windows Forms. Dans les applications WPF, [Tasks](https://msdn.microsoft.com/en-us/library/system.threading.tasks.task(v=vs.110).aspx) sont utilisées pour décharger le travail sur les threads d'arrière-plan ( éventuellement en combinaison avec [async/await](https://msdn.microsoft.com/en-us/library/mt674882.aspx)). Le regroupement des mises à jour sur le thread d'interface utilisateur est généralement effectué <!-- si la version .NET [gte 3.5] --> automatiquement, lorsque la propriété mise à jour implémente [INotifyPropertyChanged](https://msdn.microsoft.com/en-us/ library/windows/apps/windows.ui.xaml.data.inotifypropertychanged.aspx), ou manuellement <!-- end version if -->en utilisant le [Dispatcher] du fil d'interface utilisateur (https://msdn.microsoft.com/ en-us/library/system.windows.threading.dispatcher(v=vs.110).aspx).

## Utilisation d'un BackgroundWorker pour terminer une tâche.
L'exemple suivant illustre l'utilisation d'un BackgroundWorker pour mettre à jour une WinForms ProgressBar. Le backgroundWorker mettra à jour la valeur de la barre de progression sans bloquer le thread de l'interface utilisateur, affichant ainsi une interface utilisateur réactive pendant que le travail est effectué en arrière-plan.


    namespace BgWorkerExample
    {
        public partial class Form1 : Form
    {

        //a new instance of a backgroundWorker is created.
        BackgroundWorker bgWorker = new BackgroundWorker();
        
        public Form1()
        {
            InitializeComponent();

            prgProgressBar.Step = 1;

            //this assigns event handlers for the backgroundWorker
            bgWorker.DoWork += bgWorker_DoWork;
            bgWorker.RunWorkerCompleted += bgWorker_WorkComplete;

            //tell the backgroundWorker to raise the "DoWork" event, thus starting it.
            //Check to make sure the background worker is not already running.
            if(!bgWorker.IsBusy)
                bgWorker.RunWorkerAsync();
            
        }

        private void bgWorker_DoWork(object sender, DoWorkEventArgs e)
        {
            //this is the method that the backgroundworker will perform on in the background thread.
            /* One thing to note! A try catch is not necessary as any exceptions will terminate the backgroundWorker and report 
              the error to the "RunWorkerCompleted" event */
            CountToY();    
        }

        private void bgWorker_WorkComplete(object sender, RunWorkerCompletedEventArgs e)
        {
            //e.Error will contain any exceptions caught by the backgroundWorker
            if (e.Error != null)
            {
                MessageBox.Show(e.Error.Message);
            }
            else
            {
                MessageBox.Show("Task Complete!");
                prgProgressBar.Value = 0;
            }
        }

        // example method to perform a "long" running task.
        private void CountToY()
        {
            int x = 0;

            int maxProgress = 100;
            prgProgressBar.Maximum = maxProgress;
            

            while (x < maxProgress)
            {
                System.Threading.Thread.Sleep(50);
                Invoke(new Action(() => { prgProgressBar.PerformStep(); }));
                x += 1;
            }
        }


    }



**<h2>Le résultat est le suivant...</h2>**

[![entrez la description de l'image ici][1]][1]
[![entrez la description de l'image ici][2]][2]


[1] : http://i.stack.imgur.com/xGryX.png
[2] : http://i.stack.imgur.com/CRarn.png

## Affectation de gestionnaires d'événements à un BackgroundWorker
Une fois que l'instance de BackgroundWorker a été déclarée, il faut lui attribuer des propriétés et des gestionnaires d'événements pour les tâches qu'elle exécute.

        


        /* This is the backgroundworker's "DoWork" event handler. This 
           method is what will contain all the work you 
           wish to have your program perform without blocking the UI. */

        bgWorker.DoWork += bgWorker_DoWork;


        /*This is how the DoWork event method signature looks like:*/
        private void bgWorker_DoWork(object sender, DoWorkEventArgs e)
        {
            // Work to be done here   
            // ...
            // To get a reference to the current Backgroundworker:
            BackgroundWorker worker = sender as BackgroundWorker;
            // The reference to the BackgroundWorker is often used to report progress
            worker.ReportProgress(...);
        }

        /*This is the method that will be run once the BackgroundWorker has completed its tasks */

        bgWorker.RunWorkerCompleted += bgWorker_CompletedWork;

        /*This is how the RunWorkerCompletedEvent event method signature looks like:*/
        private void bgWorker_CompletedWork(object sender, RunWorkerCompletedEventArgs e)
        {
            // Things to be done after the backgroundworker has finished
        }

       /* When you wish to have something occur when a change in progress 
         occurs, (like the completion of a specific task) the "ProgressChanged" 
         event handler is used. Note that ProgressChanged events may be invoked
         by calls to bgWorker.ReportProgress(...) only if bgWorker.WorkerReportsProgress
         is set to true.  */

         bgWorker.ProgressChanged += bgWorker_ProgressChanged;

        /*This is how the ProgressChanged event method signature looks like:*/
        private void bgWorker_ProgressChanged(object sender, ProgressChangedEventArgs e)
        {
            // Things to be done when a progress change has been reported

            /* The ProgressChangedEventArgs gives access to a percentage,
             allowing for easy reporting of how far along a process is*/
            int progress = e.ProgressPercentage;
        }

## Création d'une nouvelle instance de BackgroundWorker
Un BackgroundWorker est couramment utilisé pour effectuer des tâches, parfois chronophages, sans bloquer le thread de l'interface utilisateur.


    // BackgroundWorker is part of the ComponentModel namespace.
    using System.ComponentModel;

    namespace BGWorkerExample 
    {
         public partial class ExampleForm : Form 
         {

          // the following creates an instance of the BackgroundWorker named "bgWorker"
          BackgroundWorker bgWorker = new BackgroundWorker();

          public ExampleForm() { ...

## Affectation de propriétés à un BackgroundWorker
Cela permet au BackgroundWorker d'être annulé entre les tâches

    bgWorker.WorkerSupportsCancellation = true;

Cela permet au travailleur de signaler les progrès entre l'achèvement des tâches...

    bgWorker.WorkerReportsProgress = true;
    
    //this must also be used in conjunction with the ProgressChanged event






