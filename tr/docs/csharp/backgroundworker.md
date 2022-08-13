---
title: "Arka Planİşçi"
slug: "arka-planisci"
draft: false
images: []
weight: 9869
type: docs
toc: true
---

## Sözdizimi

- `bgWorker.CancellationPending //bgWorker'ın çalışması sırasında iptal edilip edilmediğini döndürür `

- `bgWorker.IsBusy //bgWorker bir işlemin ortasındaysa true döndürür`

- `bgWorker.ReportProgress(int x) //Devam eden bir değişikliği bildirir. "ProgressChanged" olayını yükseltir`

- `bgworker.runworkerasync () //" Dowork "etkinliğini yükselterek arka plan çalışanı başlatır"

- `bgWorker.CancelAsync() // BackgroundWorker'a bir görev tamamlandıktan sonra durmasını söyler.`

UI iş parçacığı içinde uzun süre çalışan işlemler gerçekleştirmek, uygulamanızın yanıt vermemesine ve kullanıcıya çalışmayı durdurmuş gibi görünmesine neden olabilir. Bu görevlerin bir arka plan iş parçacığında çalıştırılması tercih edilir. Tamamlandığında, kullanıcı arayüzü güncellenebilir.

BackgroundWorker'ın işlemi sırasında kullanıcı arayüzünde değişiklik yapmak, tipik olarak [Control.invoke] (https://msdn.microsoft.com/en-us/library/system.windows.forms kullanılarak UI iş parçacığında değişikliklerin çağrılmasını gerektirir. Control.Invoke (V = vs.110) .aspx) Güncellediğiniz denetim üzerinde. Bunu ihmal etmek, programınızın bir istisna atmasına neden olacaktır.

BackgroundWorker, genellikle yalnızca Windows Forms uygulamalarında kullanılır. WPF uygulamalarında, işi arka plan iş parçacıklarına ( muhtemelen [async/await](https://msdn.microsoft.com/en-us/library/mt674882.aspx) ile birlikte). UI iş parçacığında sıralama güncellemeleri genellikle <!-- if version .NET [gte 3.5] --> otomatik olarak yapılır, güncellenen özellik [INotifyPropertyChanged](https://msdn.microsoft.com/en-us/) uygulandığında library/windows/apps/windows.ui.xaml.data.inotifypropertychanged.aspx) veya UI iş parçacığının [Dispatcher](https://msdn.microsoft.com/) kullanılarak manuel olarak <!-- end version if --> en-us/library/system.windows.threading.dispatcher(v=vs.110).aspx).

## Bir görevi tamamlamak için BackgroundWorker kullanma.
Aşağıdaki örnek, bir WinForms ProgressBar'ı güncellemek için BackgroundWorker kullanımını gösterir. BackgroundWorker, UI iş parçacığını engellemeden ilerleme çubuğunun değerini günceller, böylece arka planda çalışma yapılırken reaktif bir UI gösterir.


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



** <H2> Sonuç aşağıdadır ... </h2> **

[![buraya resim açıklamasını girin][1]][1]
[![buraya resim açıklamasını girin][2]][2]


[1]: http://i.stack.imgur.com/xgryx.png
[2]: http://i.stack.imgur.com/CRarn.png

## Bir BackgroundWorker'a Olay İşleyicileri Atama
BackgroundWorker örneği bildirildiğinde, gerçekleştirdiği görevler için ona özellikler ve olay işleyicileri verilmelidir.

        


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

## Yeni bir BackgroundWorker örneği oluşturma
Bir BackgroundWorker, UI iş parçacığını engellemeden bazen zaman alan görevleri gerçekleştirmek için yaygın olarak kullanılır.


    // BackgroundWorker is part of the ComponentModel namespace.
    using System.ComponentModel;

    namespace BGWorkerExample 
    {
         public partial class ExampleForm : Form 
         {

          // the following creates an instance of the BackgroundWorker named "bgWorker"
          BackgroundWorker bgWorker = new BackgroundWorker();

          public ExampleForm() { ...

## Bir BackgroundWorker'a Özellikler Atama
Bu, BackgroundWorker'ın görevler arasında iptal edilmesini sağlar.

    bgWorker.WorkerSupportsCancellation = true;

Bu, çalışanın görevlerin tamamlanması arasındaki ilerlemeyi rapor etmesine olanak tanır...

    bgWorker.WorkerReportsProgress = true;
    
    //this must also be used in conjunction with the ProgressChanged event






