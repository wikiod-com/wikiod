---
title: "Enfilage"
slug: "enfilage"
draft: false
images: []
weight: 9822
type: docs
toc: true
---

Un **thread** est une partie d'un programme qui peut s'exécuter indépendamment des autres parties. Il peut effectuer des tâches simultanément avec d'autres threads. Le **multithreading** est une fonctionnalité qui permet aux programmes d'effectuer un traitement simultané afin que plusieurs opérations puissent être effectuées à la fois.

Par exemple, vous pouvez utiliser le threading pour mettre à jour une minuterie ou un compteur en arrière-plan tout en effectuant simultanément d'autres tâches au premier plan.

Les applications multithreads sont plus réactives aux entrées de l'utilisateur et sont également facilement évolutives, car le développeur peut ajouter des threads au fur et à mesure que la charge de travail augmente.

Par défaut, un programme C# a un thread - le thread principal du programme. Cependant, des threads secondaires peuvent être créés et utilisés pour exécuter du code en parallèle avec le thread principal. Ces threads sont appelés threads de travail.

Pour contrôler le fonctionnement d'un thread, le CLR délègue une fonction au système d'exploitation appelée Thread Scheduler. Un planificateur de threads garantit que tous les threads se voient allouer un temps d'exécution approprié. Il vérifie également que les threads bloqués ou verrouillés ne consomment pas beaucoup de temps CPU.

L'espace de noms "System.Threading" du .NET Framework facilite l'utilisation des threads. System.Threading permet le multithreading en fournissant un certain nombre de classes et d'interfaces. En plus de fournir des types et des classes pour un thread particulier, il définit également des types pour contenir une collection de threads, une classe de minuterie, etc. Il fournit également son support en permettant un accès synchronisé aux données partagées.

`Thread` est la classe principale de l'espace de noms `System.Threading`. Les autres classes incluent `AutoResetEvent`, `Interlocked`, `Monitor`, `Mutex` et `ThreadPool`.

Certains des délégués présents dans l'espace de noms "System.Threading" incluent
`ThreadStart`, `TimerCallback` et `WaitCallback`.

Les énumérations dans l'espace de noms `System.Threading` incluent `ThreadPriority`, `ThreadState`,
et `EventResetMode`.

Dans .NET Framework 4 et les versions ultérieures, la programmation multithread est facilitée et simplifiée grâce aux classes `System.Threading.Tasks.Parallel` et `System.Threading.Tasks.Task`, Parallel LINQ (PLINQ), nouvelles classes de collection simultanées dans l'espace de noms "System.Collections.Concurrent" et un nouveau modèle de programmation basé sur les tâches.

## Éviter de lire et d'écrire des données simultanément
Parfois, vous souhaitez que vos threads partagent simultanément des données. Lorsque cela se produit, il est important de connaître le code et de verrouiller toutes les pièces qui pourraient mal tourner. Un exemple simple de comptage de deux threads est présenté ci-dessous.

Voici un code dangereux (incorrect):

    using System.Threading;
    
    class MainClass 
    {    
        static int count { get; set; }
    
        static void Main() 
        {
            for (int i = 1; i <= 2; i++)
            {
                var thread = new Thread(ThreadMethod);
                thread.Start(i);
                Thread.Sleep(500);
            }
        }
    
        static void ThreadMethod(object threadNumber) 
        {
            while (true)
            {
                var temp = count;
                System.Console.WriteLine("Thread " + threadNumber + ": Reading the value of count.");
                Thread.Sleep(1000);
                count = temp + 1;
                System.Console.WriteLine("Thread " + threadNumber + ": Incrementing the value of count to:" + count);
                Thread.Sleep(1000);
            }
        }
    }

Vous remarquerez, au lieu de compter 1,2,3,4,5... on compte 1,1,2,2,3...

Pour résoudre ce problème, nous devons **verrouiller** la valeur de count, afin que plusieurs threads différents ne puissent pas lire et écrire dessus en même temps. Avec l'ajout d'un verrou et d'une clé, nous pouvons empêcher les threads d'accéder simultanément aux données.

    using System.Threading;
    
    class MainClass
    {
    
        static int count { get; set; } 
        static readonly object key = new object();
    
        static void Main()
        {
            for (int i = 1; i <= 2; i++)
            {
                var thread = new Thread(ThreadMethod);
                thread.Start(i);
                Thread.Sleep(500);
            }
        }
    
        static void ThreadMethod(object threadNumber)
        {
            while (true)
            {
                lock (key) 
                {
                    var temp = count;
                    System.Console.WriteLine("Thread " + threadNumber + ": Reading the value of count.");
                    Thread.Sleep(1000);
                    count = temp + 1;
                    System.Console.WriteLine("Thread " + threadNumber + ": Incrementing the value of count to:" + count);
                }
                Thread.Sleep(1000);
            }
        }
    }

## Création et démarrage d'un deuxième thread
Si vous effectuez plusieurs longs calculs, vous pouvez les exécuter en même temps sur différents threads de votre ordinateur. Pour ce faire, nous créons un nouveau **Thread** et le faisons pointer vers une méthode différente.

    using System.Threading;
    
    class MainClass {
        static void Main() {
            var thread = new Thread(Secondary);
            thread.Start();
        }
    
        static void Secondary() {
            System.Console.WriteLine("Hello World!");
        }
    }

## Parallèle.ForEach Boucle
Si vous souhaitez accélérer une boucle foreach et que l'ordre dans lequel se trouve la sortie ne vous dérange pas, vous pouvez la convertir en une boucle foreach parallèle en procédant comme suit :

    using System;
    using System.Threading;
    using System.Threading.Tasks;
    
    public class MainClass {
    
        public static void Main() {
            int[] Numbers = new int[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
            // Single-threaded
            Console.WriteLine("Normal foreach loop: ");
            foreach (var number in Numbers) {
                Console.WriteLine(longCalculation(number));
            }
            // This is the Parallel (Multi-threaded solution)
            Console.WriteLine("Parallel foreach loop: ");
            Parallel.ForEach(Numbers, number => {
                Console.WriteLine(longCalculation(number));
            });
        }
    
        private static int longCalculation(int number) {
            Thread.Sleep(1000); // Sleep to simulate a long calculation
            return number * number;
        }
    }

## Démo de threading simple et complète
    class Program
    {
        static void Main(string[] args)
        {
            // Create 2 thread objects.  We're using delegates because we need to pass 
            // parameters to the threads.  
            var thread1 = new Thread(new ThreadStart(() => PerformAction(1)));
            var thread2 = new Thread(new ThreadStart(() => PerformAction(2)));

            // Start the threads running 
            thread1.Start();
            // NB: as soon as the above line kicks off the thread, the next line starts; 
            // even if thread1 is still processing.
            thread2.Start();

            // Wait for thread1 to complete before continuing
            thread1.Join();
            // Wait for thread2 to complete before continuing
            thread2.Join();

            Console.WriteLine("Done");
            Console.ReadKey();
        }

        // Simple method to help demonstrate the threads running in parallel.
        static void PerformAction(int id)
        {
            var rnd = new Random(id);
            for (int i = 0; i < 100; i++)
            {
                Console.WriteLine("Thread: {0}: {1}", id, i);
                Thread.Sleep(rnd.Next(0, 1000));
            }
        }
    }

## Interblocages (maintenir la ressource et attendre)
Un blocage est ce qui se produit lorsque deux threads ou plus attendent l'un de l'autre pour se terminer ou pour libérer une ressource de telle manière qu'ils attendent indéfiniment.

Si thread1 détient un verrou sur la ressource A et attend que la ressource B soit libérée tandis que thread2 détient la ressource B et attend que la ressource A soit libérée, ils sont bloqués.

En cliquant sur le bouton 1 pour l'exemple de code suivant, votre application entrera dans l'état de blocage susmentionné et se bloquera

    private void button_Click(object sender, EventArgs e)
    {
        DeadlockWorkers workers = new DeadlockWorkers();
        workers.StartThreads();
        textBox.Text = workers.GetResult();
    }

    private class DeadlockWorkers
    {
        Thread thread1, thread2;

        object resourceA = new object();
        object resourceB = new object();

        string output;

        public void StartThreads()
        {
            thread1 = new Thread(Thread1DoWork);
            thread2 = new Thread(Thread2DoWork);
            thread1.Start();
            thread2.Start();
        }

        public string GetResult()
        {
            thread1.Join();
            thread2.Join();
            return output;
        }

        public void Thread1DoWork()
        {
            Thread.Sleep(100);
            lock (resourceA)
            {
                Thread.Sleep(100);
                lock (resourceB)
                {
                    output += "T1#";
                }
            }
        }

        public void Thread2DoWork()
        {
            Thread.Sleep(100);
            lock (resourceB)
            {
                Thread.Sleep(100);
                lock (resourceA)
                {
                    output += "T2#";
                }
            }
        }
    }

Pour éviter d'être bloqué de cette façon, on peut utiliser Monitor.TryEnter(lock_object, timeout_in_milliseconds) pour vérifier si un verrou est déjà détenu sur un objet. Si Monitor.TryEnter ne réussit pas à acquérir un verrou sur lock_object avant timeout_in_milliseconds, il renvoie false, donnant au thread une chance de libérer d'autres ressources détenues et de céder, donnant ainsi aux autres threads une chance de se terminer comme dans cette version légèrement modifiée de ce qui précède :

    private void button_Click(object sender, EventArgs e)
    {
        MonitorWorkers workers = new MonitorWorkers();
        workers.StartThreads();
        textBox.Text = workers.GetResult();
    }

    private class MonitorWorkers
    {
        Thread thread1, thread2;

        object resourceA = new object();
        object resourceB = new object();

        string output;

        public void StartThreads()
        {
            thread1 = new Thread(Thread1DoWork);
            thread2 = new Thread(Thread2DoWork);
            thread1.Start();
            thread2.Start();
        }

        public string GetResult()
        {
            thread1.Join();
            thread2.Join();
            return output;
        }

        public void Thread1DoWork()
        {
            bool mustDoWork = true;
            Thread.Sleep(100);
            while (mustDoWork)
            {
                lock (resourceA)
                {
                    Thread.Sleep(100);
                    if (Monitor.TryEnter(resourceB, 0))
                    {
                        output += "T1#";
                        mustDoWork = false;
                        Monitor.Exit(resourceB);
                    }
                }
                if (mustDoWork) Thread.Yield();
            }
        }

        public void Thread2DoWork()
        {
            Thread.Sleep(100);
            lock (resourceB)
            {
                Thread.Sleep(100);
                lock (resourceA)
                {
                    output += "T2#";
                }
            }
        }
    }

Notez que cette solution de contournement repose sur le fait que thread2 est têtu sur ses verrous et que thread1 est prêt à céder, de sorte que thread2 a toujours la priorité. Notez également que thread1 doit refaire le travail qu'il a effectué après avoir verrouillé la ressource A, lorsqu'il cède. Par conséquent, soyez prudent lors de la mise en œuvre de cette approche avec plus d'un thread de rendement, car vous courrez alors le risque d'entrer dans un soi-disant livelock - un état qui se produirait si deux threads continuaient à faire le premier morceau de leur travail et ensuite céder mutuellement , en recommençant à plusieurs reprises.

## Création d'un thread par processeur
> `Environment.ProcessorCount` Obtient le nombre de processeurs **logiques** sur la machine actuelle.

Le CLR planifiera ensuite chaque thread sur un processeur logique, cela pourrait théoriquement signifier chaque thread sur un processeur logique différent, tous les threads sur un seul processeur logique ou une autre combinaison.

    using System;
    using System.Threading;
    
    class MainClass {
        static void Main() {
            for (int i = 0; i < Environment.ProcessorCount; i++) {
                var thread = new Thread(Secondary);
                thread.Start(i);
            }
            
        }
    
        static void Secondary(object threadNumber) {
            System.Console.WriteLine("Hello World from thread: " + threadNumber);
        }
    }

## Démo de threading simple et complète à l'aide de tâches
    class Program
    {
        static void Main(string[] args)
        {
            // Run 2 Tasks.  
            var task1 = Task.Run(() => PerformAction(1)));
            var task2 = Task.Run(() => PerformAction(2)));
    
            // Wait (i.e. block this thread) until both Tasks are complete.
            Task.WaitAll(new [] { task1, task2 });
            
            Console.WriteLine("Done");
            Console.ReadKey();
        }
    
        // Simple method to help demonstrate the threads running in parallel.
        static void PerformAction(int id)
        {
            var rnd = new Random(id);
            for (int i = 0; i < 100; i++)
            {
                Console.WriteLine("Task: {0}: {1}", id, i);
                Thread.Sleep(rnd.Next(0, 1000));
            }
        }
    }

## Parallélisme de tâche explicite
        private static void explicitTaskParallism()
        {
            Thread.CurrentThread.Name = "Main";

            // Create a task and supply a user delegate by using a lambda expression. 
            Task taskA = new Task(() => Console.WriteLine($"Hello from task {nameof(taskA)}."));
            Task taskB = new Task(() => Console.WriteLine($"Hello from task {nameof(taskB)}."));

            // Start the task.
            taskA.Start();
            taskB.Start();

            // Output a message from the calling thread.
            Console.WriteLine("Hello from thread '{0}'.",
                              Thread.CurrentThread.Name);
            taskA.Wait();
            taskB.Wait();
            Console.Read();
        }

## Parallélisme implicite des tâches
        private static void Main(string[] args)
        {
            var a = new A();
            var b = new B();
            //implicit task parallelism
            Parallel.Invoke(
                () => a.DoSomeWork(),
                () => b.DoSomeOtherWork()
                );

          }

## Démarrer un thread avec des paramètres
en utilisant System.Threading ;
    
    class MainClass {
        static void Main() {
            var thread = new Thread(Secondary);
            thread.Start("SecondThread");
        }
    
        static void Secondary(object threadName) {
            System.Console.WriteLine("Hello World from thread: " + threadName);
        }
    }

## Interblocages (deux threads en attente l'un de l'autre)
Un blocage est ce qui se produit lorsque deux threads ou plus attendent l'un de l'autre pour se terminer ou pour libérer une ressource de telle manière qu'ils attendent indéfiniment.

Un scénario typique de deux threads attendant l'un l'autre pour se terminer est lorsqu'un thread d'interface graphique Windows Forms attend un thread de travail et que le thread de travail tente d'appeler un objet géré par le thread d'interface graphique.
Observez qu'avec cet exemple de code, cliquer sur le bouton 1 entraînera le blocage du programme.

    private void button1_Click(object sender, EventArgs e)
    {
        Thread workerthread= new Thread(dowork);
        workerthread.Start();
        workerthread.Join();
        // Do something after
    }

    private void dowork()
    {
        // Do something before
        textBox1.Invoke(new Action(() => textBox1.Text = "Some Text"));
        // Do something after
    }

`workerthread.Join()` est un appel qui bloque le thread appelant jusqu'à ce que workerthread se termine.
`textBox1.Invoke(invoke_delegate)` est un appel qui bloque le thread appelant jusqu'à ce que le thread GUI ait traité invoke_delegate, mais cet appel provoque des blocages si le thread GUI attend déjà que le thread appelant se termine.

Pour contourner ce problème, on peut utiliser un moyen non bloquant d'invoquer la zone de texte à la place :

    private void dowork()
    {
        // Do work
        textBox1.BeginInvoke(new Action(() => textBox1.Text = "Some Text"));
        // Do work that is not dependent on textBox1 being updated first
    }

Cependant, cela causera des problèmes si vous devez exécuter du code qui dépend de la zone de texte mise à jour en premier. Dans ce cas, exécutez-le dans le cadre de l'appel, mais sachez que cela le fera fonctionner sur le thread de l'interface graphique.

    private void dowork()
    {
        // Do work
        textBox1.BeginInvoke(new Action(() => {
            textBox1.Text = "Some Text";
            // Do work dependent on textBox1 being updated first, 
            // start another worker thread or raise an event
        }));
        // Do work that is not dependent on textBox1 being updated first
    }

Vous pouvez également démarrer un tout nouveau thread et laisser celui-ci faire l'attente sur le thread de l'interface graphique, afin que le workerthread puisse se terminer.

    private void dowork()
    {
        // Do work
        Thread workerthread2 = new Thread(() =>
        {
            textBox1.Invoke(new Action(() => textBox1.Text = "Some Text"));
            // Do work dependent on textBox1 being updated first, 
            // start another worker thread or raise an event
        });
        workerthread2.Start();
        // Do work that is not dependent on textBox1 being updated first
    }

Pour minimiser le risque de tomber dans une impasse d'attente mutuelle, évitez toujours les références circulaires entre les threads lorsque cela est possible. Une hiérarchie de threads où les threads de rang inférieur ne laissent que des messages pour les threads de rang supérieur et ne les attendent jamais ne se heurtera pas à ce type de problème. Cependant, il serait toujours vulnérable aux interblocages basés sur le verrouillage des ressources.

