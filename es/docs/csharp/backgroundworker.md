---
title: "FondoTrabajador"
slug: "fondotrabajador"
draft: false
images: []
weight: 9869
type: docs
toc: true
---

## Sintaxis

- `bgWorker.CancellationPending //devuelve si el bgWorker fue cancelado durante su operación`

- `bgWorker.IsBusy //devuelve verdadero si bgWorker está en medio de una operación`

- `bgWorker.ReportProgress(int x) //Informa de un cambio en curso. Genera el evento "ProgressChanged"

- `bgWorker.RunWorkerAsync() //Inicia el BackgroundWorker generando el evento "DoWork"`

- `bgWorker.CancelAsync() //instruye a BackgroundWorker que se detenga después de completar una tarea.`

La realización de operaciones de ejecución prolongada dentro del subproceso de la interfaz de usuario puede hacer que su aplicación deje de responder y el usuario le parezca que ha dejado de funcionar. Se prefiere que estas tareas se ejecuten en un subproceso en segundo plano. Una vez completada, la interfaz de usuario se puede actualizar.

Hacer cambios en la interfaz de usuario durante la operación de BackgroundWorker requiere invocar los cambios en el subproceso de la interfaz de usuario, normalmente mediante el uso de [Control.Invoke](https://msdn.microsoft.com/en-us/library/system.windows.forms. control.invoke(v=vs.110).aspx) en el control que está actualizando. Si no lo hace, su programa generará una excepción.

El BackgroundWorker normalmente solo se usa en aplicaciones de Windows Forms. En las aplicaciones WPF, las [Tareas](https://msdn.microsoft.com/en-us/library/system.threading.tasks.task(v=vs.110).aspx) se utilizan para descargar trabajo en subprocesos en segundo plano ( posiblemente en combinación con [async/await](https://msdn.microsoft.com/en-us/library/mt674882.aspx)). La ordenación de las actualizaciones en el subproceso de la interfaz de usuario generalmente se realiza <!-- si la versión .NET [gte 3.5] --> automáticamente, cuando la propiedad que se actualiza implementa [INotifyPropertyChanged](https://msdn.microsoft.com/en-us/ library/windows/apps/windows.ui.xaml.data.inotifypropertychanged.aspx), o manualmente <!-- end version if -->mediante el [Dispatcher] del subproceso de la interfaz de usuario (https://msdn.microsoft.com/ en-us/library/system.windows.threading.dispatcher(v=vs.110).aspx).

## Usando un BackgroundWorker para completar una tarea.
El siguiente ejemplo demuestra el uso de un BackgroundWorker para actualizar un ProgressBar de WinForms. El backgroundWorker actualizará el valor de la barra de progreso sin bloquear el subproceso de la interfaz de usuario, mostrando así una interfaz de usuario reactiva mientras se realiza el trabajo en segundo plano.


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



**<h2>El resultado es el siguiente...</h2>**

[![ingrese la descripción de la imagen aquí][1]][1]
[![ingrese la descripción de la imagen aquí][2]][2]


[1]: http://i.stack.imgur.com/xGryX.png
[2]: http://i.stack.imgur.com/CRarn.png

## Asignación de controladores de eventos a un BackgroundWorker
Una vez que se ha declarado la instancia de BackgroundWorker, se le deben dar propiedades y controladores de eventos para las tareas que realiza.

        


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

## Creando una nueva instancia de BackgroundWorker
Un BackgroundWorker se usa comúnmente para realizar tareas, que a veces consumen mucho tiempo, sin bloquear el subproceso de la interfaz de usuario.


    // BackgroundWorker is part of the ComponentModel namespace.
    using System.ComponentModel;

    namespace BGWorkerExample 
    {
         public partial class ExampleForm : Form 
         {

          // the following creates an instance of the BackgroundWorker named "bgWorker"
          BackgroundWorker bgWorker = new BackgroundWorker();

          public ExampleForm() { ...

## Asignación de propiedades a un BackgroundWorker
Esto permite que BackgroundWorker se cancele entre tareas

    bgWorker.WorkerSupportsCancellation = true;

Esto le permite al trabajador informar el progreso entre la finalización de las tareas...

    bgWorker.WorkerReportsProgress = true;
    
    //this must also be used in conjunction with the ProgressChanged event






