---
title: "Rosqueamento"
slug: "rosqueamento"
draft: false
images: []
weight: 9822
type: docs
toc: true
---

Um **thread** é uma parte de um programa que pode ser executado independentemente de outras partes. Ele pode executar tarefas simultaneamente com outros threads. **Multithreading** é um recurso que permite que os programas executem processamento simultâneo para que mais de uma operação possa ser feita por vez.

Por exemplo, você pode usar o encadeamento para atualizar um cronômetro ou contador em segundo plano enquanto executa simultaneamente outras tarefas em primeiro plano.

Os aplicativos multithread são mais responsivos à entrada do usuário e também são facilmente escaláveis, porque o desenvolvedor pode adicionar threads à medida que a carga de trabalho aumenta.

Por padrão, um programa C# tem um thread - o thread do programa principal. No entanto, threads secundários podem ser criados e usados ​​para executar código em paralelo com o thread primário. Esses threads são chamados de threads de trabalho.

Para controlar a operação de um thread, o CLR delega uma função ao sistema operacional conhecida como Thread Scheduler. Um agendador de threads garante que todos os threads tenham tempo de execução adequado. Ele também verifica se os encadeamentos que estão bloqueados ou bloqueados não consomem muito do tempo da CPU.

O namespace `System.Threading` do .NET Framework facilita o uso de threads. System.Threading permite multithreading fornecendo várias classes e interfaces. Além de fornecer tipos e classes para um thread específico, ele também define tipos para armazenar uma coleção de threads, classe de timer e assim por diante. Ele também fornece seu suporte ao permitir o acesso sincronizado a dados compartilhados.

`Thread` é a classe principal no namespace `System.Threading`. Outras classes incluem `AutoResetEvent`, `Interlocked`, `Monitor`, `Mutex` e `ThreadPool`.

Alguns dos delegados presentes no namespace `System.Threading` incluem
`ThreadStart`, `TimerCallback` e `WaitCallback`.

Enumerações no namespace `System.Threading` incluem `ThreadPriority`, `ThreadState`,
e `EventResetMode`.

No .NET Framework 4 e versões posteriores, a programação multithread é facilitada e simples através das classes `System.Threading.Tasks.Parallel` e `System.Threading.Tasks.Task`, Parallel LINQ (PLINQ), novas classes de coleção simultâneas em o namespace `System.Collections.Concurrent` e um novo modelo de programação baseado em tarefas.

## Evitando Ler e Gravar Dados Simultaneamente
Às vezes, você deseja que seus threads compartilhem dados simultaneamente. Quando isso acontece, é importante estar atento ao código e bloquear todas as peças que possam dar errado. Um exemplo simples de contagem de dois threads é mostrado abaixo.

Aqui está algum código perigoso (incorreto):

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

Você notará, em vez de contar 1,2,3,4,5... contamos 1,1,2,2,3...

Para corrigir esse problema, precisamos **bloquear** o valor de count, para que vários threads diferentes não possam ler e gravar nele ao mesmo tempo. Com a adição de uma fechadura e uma chave, podemos impedir que as threads acessem os dados simultaneamente.

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

## Criando e iniciando um segundo thread
Se você estiver fazendo vários cálculos longos, poderá executá-los ao mesmo tempo em diferentes threads em seu computador. Para fazer isso, criamos um novo **Thread** e fazemos com que ele aponte para um método diferente.

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

## Parallel.ForEach Loop
Se você tem um loop foreach que deseja acelerar e não se importa com a ordem em que a saída está, você pode convertê-lo em um loop foreach paralelo fazendo o seguinte:

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

## Demonstração de encadeamento completo simples
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

## Deadlocks (segurar recurso e esperar)
Um deadlock é o que ocorre quando dois ou mais threads estão esperando um pelo outro para completar ou liberar um recurso de tal forma que eles esperam para sempre.

Se o thread1 retém um bloqueio no recurso A e está aguardando o recurso B ser liberado enquanto o thread2 retém o recurso B e está aguardando o recurso A ser liberado, eles estão em deadlock.

Clicar no botão1 para o código de exemplo a seguir fará com que seu aplicativo entre no estado de impasse mencionado acima e trave

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

Para evitar um impasse dessa forma, pode-se usar Monitor.TryEnter(lock_object, timeout_in_milliseconds) para verificar se um bloqueio já está em um objeto. Se Monitor.TryEnter não obtiver sucesso em adquirir um bloqueio em lock_object antes de timeout_in_milliseconds, ele retornará false, dando ao thread a chance de liberar outros recursos retidos e cedendo, dando assim a outros threads a chance de concluir como nesta versão ligeiramente modificada do acima :

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

Observe que essa solução alternativa depende de thread2 ser teimoso sobre seus bloqueios e thread1 estar disposto a ceder, de modo que thread2 sempre tenha precedência. Observe também que o thread1 precisa refazer o trabalho que fez após bloquear o recurso A, quando ele rende. Portanto, tenha cuidado ao implementar essa abordagem com mais de um thread de rendimento, pois você correrá o risco de entrar no chamado livelock - um estado que ocorreria se dois threads continuassem fazendo o primeiro bit de seu trabalho e, em seguida, produzissem mutuamente , recomeçando repetidamente.

## Criando um thread por processador
> `Environment.ProcessorCount` Obtém o número de processadores **lógicos** na máquina atual.

O CLR então agendará cada thread para um processador lógico, isso teoricamente poderia significar cada thread em um processador lógico diferente, todos os threads em um único processador lógico ou alguma outra combinação.

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

## Demonstração de Threading Completa Simples usando Tarefas
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

## Paralismo Explícito de Tarefas
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

## Paralelismo de Tarefa Implícito
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

## Iniciando um thread com parâmetros
usando System.Threading;
    
    class MainClass {
        static void Main() {
            var thread = new Thread(Secondary);
            thread.Start("SecondThread");
        }
    
        static void Secondary(object threadName) {
            System.Console.WriteLine("Hello World from thread: " + threadName);
        }
    }

## Deadlocks (duas threads esperando uma pela outra)
Um deadlock é o que ocorre quando dois ou mais threads estão esperando um pelo outro para completar ou liberar um recurso de tal forma que eles esperam para sempre.

Um cenário típico de dois threads aguardando a conclusão um do outro é quando um thread da GUI do Windows Forms aguarda um thread de trabalho e o thread de trabalho tenta invocar um objeto gerenciado pelo thread da GUI.
Observe que com este exemplo de código, clicar no botão1 fará com que o programa trave.

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

`workerthread.Join()` é uma chamada que bloqueia o thread de chamada até que o workerthread seja concluído.
`textBox1.Invoke(invoke_delegate)` é uma chamada que bloqueia o thread de chamada até que o thread da GUI tenha processado invoke_delegate, mas esta chamada causa deadlocks se o thread da GUI já estiver aguardando a conclusão do thread de chamada.

Para contornar isso, pode-se usar uma maneira sem bloqueio de invocar a caixa de texto:

    private void dowork()
    {
        // Do work
        textBox1.BeginInvoke(new Action(() => textBox1.Text = "Some Text"));
        // Do work that is not dependent on textBox1 being updated first
    }

No entanto, isso causará problemas se você precisar executar um código que dependa da atualização da caixa de texto primeiro. Nesse caso, execute isso como parte da invocação, mas esteja ciente de que isso fará com que seja executado no thread da GUI.

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

Alternativamente, inicie um novo encadeamento inteiro e deixe-o fazer a espera no encadeamento da GUI, para que o encadeamento de trabalho possa ser concluído.

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

Para minimizar o risco de entrar em um impasse de espera mútua, sempre evite referências circulares entre threads quando possível. Uma hierarquia de threads em que os threads de classificação mais baixa apenas deixam mensagens para threads de classificação mais alta e nunca os aguardam não terá esse tipo de problema. No entanto, ainda estaria vulnerável a deadlocks com base no bloqueio de recursos.

