---
title: "enhebrar"
slug: "enhebrar"
draft: false
images: []
weight: 9822
type: docs
toc: true
---

Un **hilo** es una parte de un programa que puede ejecutarse independientemente de otras partes. Puede realizar tareas simultáneamente con otros subprocesos. **Multithreading** es una función que permite que los programas realicen un procesamiento simultáneo para que se pueda realizar más de una operación a la vez.

Por ejemplo, puede usar subprocesos para actualizar un temporizador o contador en segundo plano mientras realiza otras tareas simultáneamente en primer plano.

Las aplicaciones de subprocesos múltiples responden mejor a la entrada del usuario y también son fácilmente escalables, porque el desarrollador puede agregar subprocesos a medida que aumenta la carga de trabajo.

De forma predeterminada, un programa C# tiene un subproceso: el subproceso principal del programa. Sin embargo, los subprocesos secundarios se pueden crear y utilizar para ejecutar código en paralelo con el subproceso principal. Tales subprocesos se denominan subprocesos de trabajo.

Para controlar el funcionamiento de un subproceso, el CLR delega una función al sistema operativo conocida como Programador de subprocesos. Un programador de subprocesos asegura que a todos los subprocesos se les asigne el tiempo de ejecución adecuado. También verifica que los hilos que están bloqueados o bloqueados no consuman mucho tiempo de CPU.

El espacio de nombres `System.Threading` de .NET Framework facilita el uso de subprocesos. System.Threading permite subprocesos múltiples al proporcionar una serie de clases e interfaces. Además de proporcionar tipos y clases para un subproceso en particular, también define tipos para contener una colección de subprocesos, clases de temporizador, etc. También proporciona su apoyo al permitir el acceso sincronizado a los datos compartidos.

`Thread` es la clase principal en el espacio de nombres `System.Threading`. Otras clases incluyen `AutoResetEvent`, `Interlocked`, `Monitor`, `Mutex` y `ThreadPool`.

Algunos de los delegados que están presentes en el espacio de nombres `System.Threading` incluyen
`ThreadStart`, `TimerCallback` y `WaitCallback`.

Las enumeraciones en el espacio de nombres `System.Threading` incluyen `ThreadPriority`, `ThreadState`,
y `EventResetMode`.

En .NET Framework 4 y versiones posteriores, la programación multiproceso se hace más fácil y simple a través de las clases `System.Threading.Tasks.Parallel` y `System.Threading.Tasks.Task`, Parallel LINQ (PLINQ), nuevas clases de colección concurrentes en el espacio de nombres `System.Collections.Concurrent` y un nuevo modelo de programación basado en tareas.

## Evitar leer y escribir datos simultáneamente
A veces, desea que sus subprocesos compartan datos simultáneamente. Cuando esto sucede, es importante conocer el código y bloquear cualquier parte que pueda fallar. A continuación se muestra un ejemplo simple de conteo de dos hilos.

Aquí hay un código peligroso (incorrecto):

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

Notarás que en lugar de contar 1,2,3,4,5... contamos 1,1,2,2,3...

Para solucionar este problema, necesitamos **bloquear** el valor de count, de modo que varios subprocesos diferentes no puedan leerlo y escribirlo al mismo tiempo. Con la adición de un candado y una llave, podemos evitar que los subprocesos accedan a los datos simultáneamente.

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

## Crear e iniciar un segundo subproceso
Si está haciendo varios cálculos largos, puede ejecutarlos al mismo tiempo en diferentes subprocesos en su computadora. Para hacer esto, creamos un nuevo **Subproceso** y hacemos que apunte a un método diferente.

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

## Paralelo.Para cada bucle
Si tiene un bucle foreach que desea acelerar y no le importa en qué orden está la salida, puede convertirlo en un bucle foreach paralelo haciendo lo siguiente:

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

## Demostración de enhebrado completo simple
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

## Interbloqueos (mantener recurso y esperar)
Un interbloqueo es lo que ocurre cuando dos o más subprocesos esperan entre sí para completar o liberar un recurso de tal manera que esperan para siempre.

Si el subproceso1 mantiene un bloqueo en el recurso A y está esperando que se libere el recurso B mientras que el subproceso2 retiene el recurso B y está esperando que se libere el recurso A, están bloqueados.

Al hacer clic en el botón 1 para el siguiente código de ejemplo, su aplicación entrará en el estado de interbloqueo antes mencionado y se bloqueará.

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

Para evitar estar bloqueado de esta manera, se puede usar Monitor.TryEnter(lock_object, timeout_in_milliseconds) para comprobar si ya se ha mantenido un bloqueo en un objeto. Si Monitor.TryEnter no logra adquirir un bloqueo en lock_object antes del tiempo de espera_en_milisegundos, devuelve falso, lo que le da al subproceso la oportunidad de liberar otros recursos retenidos y cede, dando así a otros subprocesos la oportunidad de completarse como en esta versión ligeramente modificada de la anterior. :

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

Tenga en cuenta que esta solución depende de que thread2 sea obstinado con sus bloqueos y de que thread1 esté dispuesto a ceder, de modo que thread2 siempre tenga prioridad. También tenga en cuenta que thread1 tiene que rehacer el trabajo que hizo después de bloquear el recurso A, cuando cede. Por lo tanto, tenga cuidado al implementar este enfoque con más de un subproceso que cede, ya que correrá el riesgo de ingresar en el llamado bloqueo en vivo, un estado que ocurriría si dos subprocesos siguieran haciendo la primera parte de su trabajo y luego ceder mutuamente , comenzando de nuevo repetidamente.

## Creación de un subproceso por procesador
> `Environment.ProcessorCount` Obtiene el número de procesadores **lógicos** en la máquina actual.

Luego, CLR programará cada subproceso en un procesador lógico; esto, en teoría, podría significar cada subproceso en un procesador lógico diferente, todos los subprocesos en un solo procesador lógico o alguna otra combinación.

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

## Demostración de subprocesos completa simple usando tareas
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

## Paralelismo de tareas explícito
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

## Paralelismo implícito de tareas
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

## Comenzando un hilo con parámetros
utilizando System.Threading;
    
    class MainClass {
        static void Main() {
            var thread = new Thread(Secondary);
            thread.Start("SecondThread");
        }
    
        static void Secondary(object threadName) {
            System.Console.WriteLine("Hello World from thread: " + threadName);
        }
    }

## Interbloqueos (dos subprocesos esperando el uno al otro)
Un interbloqueo es lo que ocurre cuando dos o más subprocesos esperan entre sí para completar o liberar un recurso de tal manera que esperan para siempre.

Un escenario típico de dos subprocesos que esperan el uno al otro para completarse es cuando un subproceso de GUI de Windows Forms espera un subproceso de trabajo y el subproceso de trabajo intenta invocar un objeto administrado por el subproceso de GUI.
Observe que con este ejemplo de código, hacer clic en el botón 1 hará que el programa se cuelgue.

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

`workerthread.Join()` es una llamada que bloquea el subproceso de llamada hasta que se completa el subproceso de trabajo.
`textBox1.Invoke(invoke_delegate)` es una llamada que bloquea el subproceso de llamada hasta que el subproceso de GUI haya procesado invoque_delegate, pero esta llamada provoca interbloqueos si el subproceso de GUI ya está esperando que se complete el subproceso de llamada.

Para evitar esto, se puede usar una forma sin bloqueo de invocar el cuadro de texto en su lugar:

    private void dowork()
    {
        // Do work
        textBox1.BeginInvoke(new Action(() => textBox1.Text = "Some Text"));
        // Do work that is not dependent on textBox1 being updated first
    }

Sin embargo, esto causará problemas si necesita ejecutar un código que depende de que el cuadro de texto se actualice primero. En ese caso, ejecútelo como parte de la invocación, pero tenga en cuenta que esto hará que se ejecute en el subproceso de la GUI.

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

Alternativamente, inicie un subproceso completamente nuevo y deje que ese haga la espera en el subproceso de GUI, para que el subproceso de trabajo pueda completarse.

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

Para minimizar el riesgo de caer en un punto muerto de espera mutua, evite siempre las referencias circulares entre subprocesos cuando sea posible. Una jerarquía de subprocesos donde los subprocesos de rango inferior solo dejan mensajes para subprocesos de rango superior y nunca los esperan no se encontrará con este tipo de problema. Sin embargo, seguiría siendo vulnerable a interbloqueos basados ​​en el bloqueo de recursos.

