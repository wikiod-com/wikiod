---
title: "Trabalhador em segundo plano"
slug: "trabalhador-em-segundo-plano"
draft: false
images: []
weight: 9869
type: docs
toc: true
---

## Sintaxe

- `bgWorker.CancellationPending //retorna se o bgWorker foi cancelado durante sua operação `

- `bgWorker.IsBusy //retorna true se o bgWorker estiver no meio de uma operação`

- `bgWorker.ReportProgress(int x) //Relata uma mudança em andamento. Gera o evento "ProgressChanged"`

- `bgWorker.RunWorkerAsync() //Inicia o BackgroundWorker acionando o evento "DoWork"`

- `bgWorker.CancelAsync() //instrui o BackgroundWorker a parar após a conclusão de uma tarefa.`

A execução de operações de longa duração no thread de interface do usuário pode fazer com que seu aplicativo pare de responder, parecendo ao usuário que ele parou de funcionar. É preferível que essas tarefas sejam executadas em um thread em segundo plano. Depois de concluído, a interface do usuário pode ser atualizada.

Fazer alterações na interface do usuário durante a operação do BackgroundWorker requer invocar as alterações no thread da interface do usuário, normalmente usando o [Control.Invoke](https://msdn.microsoft.com/en-us/library/system.windows.forms. control.invoke(v=vs.110).aspx) no controle que você está atualizando. Negligenciar isso fará com que seu programa lance uma exceção.

O BackgroundWorker normalmente é usado apenas em aplicativos Windows Forms. Em aplicativos WPF, [Tasks](https://msdn.microsoft.com/en-us/library/system.threading.tasks.task(v=vs.110).aspx) são usados ​​para descarregar o trabalho em threads em segundo plano ( possivelmente em combinação com [async/await](https://msdn.microsoft.com/en-us/library/mt674882.aspx)). A distribuição de atualizações no thread da interface do usuário geralmente é feita <!-- if version .NET [gte 3.5] --> automaticamente, quando a propriedade que está sendo atualizada implementa [INotifyPropertyChanged](https://msdn.microsoft.com/en-us/ library/windows/apps/windows.ui.xaml.data.inotifypropertychanged.aspx) ou manualmente <!-- end version if --> usando o [Dispatcher] do thread da interface do usuário (https://msdn.microsoft.com/ pt-br/library/system.windows.threading.dispatcher(v=vs.110).aspx).

## Usando um BackgroundWorker para concluir uma tarefa.
O exemplo a seguir demonstra o uso de um BackgroundWorker para atualizar um WinForms ProgressBar. O backgroundWorker atualizará o valor da barra de progresso sem bloquear o thread da interface do usuário, mostrando assim uma interface do usuário reativa enquanto o trabalho é feito em segundo plano.


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



***<h2>O resultado é o seguinte...</h2>**

[![digite a descrição da imagem aqui][1]][1]
[![digite a descrição da imagem aqui][2]][2]


[1]: http://i.stack.imgur.com/xGryX.png
[2]: http://i.stack.imgur.com/CRarn.png

## Atribuindo manipuladores de eventos a um BackgroundWorker
Uma vez que a instância do BackgroundWorker tenha sido declarada, ela deve receber propriedades e manipuladores de eventos para as tarefas que executa.

        


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

## Criando uma nova instância BackgroundWorker
Um BackgroundWorker é comumente usado para executar tarefas, às vezes demoradas, sem bloquear o thread da interface do usuário.


    // BackgroundWorker is part of the ComponentModel namespace.
    using System.ComponentModel;

    namespace BGWorkerExample 
    {
         public partial class ExampleForm : Form 
         {

          // the following creates an instance of the BackgroundWorker named "bgWorker"
          BackgroundWorker bgWorker = new BackgroundWorker();

          public ExampleForm() { ...

## Atribuindo propriedades a um BackgroundWorker
Isso permite que o BackgroundWorker seja cancelado entre as tarefas

    bgWorker.WorkerSupportsCancellation = true;

Isso permite que o trabalhador relate o progresso entre a conclusão das tarefas...

    bgWorker.WorkerReportsProgress = true;
    
    //this must also be used in conjunction with the ProgressChanged event






