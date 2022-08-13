---
title: "Temporizadores"
slug: "temporizadores"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Sintaxe
- `myTimer.Interval` - define com que frequência o evento "Tick" é chamado (em milissegundos)
- `myTimer.Enabled` - valor booleano que define o timer para ser habilitado/desabilitado
- `myTimer.Start()` - Inicia o cronômetro.
- `myTimer.Stop()` - Pára o cronômetro.

Se estiver usando o Visual Studio, os temporizadores podem ser adicionados como um controle diretamente ao seu formulário a partir da caixa de ferramentas.

## Temporizadores multithread
`System.Threading.Timer` - Temporizador multithread mais simples. Contém dois métodos e um construtor.

Exemplo:
Um temporizador chama o método DataWrite, que escreve "multithread executado..." após cinco
segundos se passaram e, em seguida, a cada segundo até que o usuário pressione Enter:

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

Nota: Publicará uma seção separada para descartar temporizadores multithread.

`Change` - Este método pode ser chamado quando você deseja alterar o intervalo do temporizador.

`Timeout.Infinite` - Se você quiser disparar apenas uma vez. Especifique isso no último argumento do construtor.

`System.Timers` - Outra classe de timer fornecida pelo .NET Framework. Ele envolve o `System.Threading.Timer`.

Características:
---------

- `IComponent` - Permitindo que seja localizado na bandeja de componentes do Visual Studio Designer
- Propriedade `Interval` em vez de um método `Change`
- `Elapsed` `event` em vez de um callback `delegate`
- Propriedade `Enabled` para iniciar e parar o temporizador (`default value = false`)
- Métodos `Start` e `Stop` caso você fique confuso com a propriedade `Enabled` (acima do ponto)
- `AutoReset` - para indicar um evento recorrente (`default value = true`)
- Propriedade `SynchronizingObject` com métodos `Invoke` e `BeginInvoke` para
chamando métodos com segurança em elementos WPF e controles Windows Forms


Exemplo representando todos os recursos acima:

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

`Timers multithreaded` - use o pool de threads para permitir que alguns threads atendam a muitos
temporizadores. Isso significa que o método de retorno de chamada ou evento `Elapsed` pode ser acionado em um
thread cada vez que é chamado.

`Elapsed` - este evento sempre é acionado na hora certa, independentemente de o evento `Elapsed` anterior ter terminado de ser executado. Por isso, retornos de chamada ou manipuladores de eventos devem ser thread-safe.
A precisão dos temporizadores multithread depende do sistema operacional e normalmente é
nos 10-20 ms.

`interop` - sempre que precisar de maior precisão, use isso e chame o temporizador multimídia do Windows. Isso tem precisão de até 1 ms e é definido em `winmm.dll`.

`timeBeginPeriod` - Chame isso primeiro para informar o sistema operacional que você precisa de alta precisão de tempo

`timeSetEvent` - chame isto após `timeBeginPeriod` para iniciar um temporizador multimídia.

`timeKillEvent` - chame isso quando terminar, isso interrompe o cronômetro

`timeEndPeriod` - Chame isso para informar ao sistema operacional que você não precisa mais de alta precisão de tempo.

Você pode encontrar exemplos completos na Internet que usam o temporizador multimídia pesquisando as palavras-chave `dllimport` `winmm.dll` `timesetevent`.

## Criando uma instância de um timer
Os temporizadores são usados ​​para executar tarefas em intervalos de tempo específicos (Faça X a cada Y segundos)
Abaixo está um exemplo de criação de uma nova instância de um Timer.

**NOTA**: Isso se aplica a Timers usando WinForms. Se estiver usando WPF, você pode querer olhar para `DispatcherTimer`


        using System.Windows.Forms; //Timers use the Windows.Forms namespace

        public partial class Form1 : Form
        {

            Timer myTimer = new Timer(); //create an instance of Timer named myTimer
   
        
            public Form1()
            {
                InitializeComponent();
            }

        }

## Atribuindo o manipulador de eventos "Tick" a um Timer
Todas as ações executadas em um timer são tratadas no evento "Tick".

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

## Exemplo: Usando um Timer para realizar uma contagem regressiva simples.
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

Resulta em...

[![digite a descrição da imagem aqui][1]][1][![digite a descrição da imagem aqui][2]][2]

E assim por diante...


[1]: http://i.stack.imgur.com/VZlnr.png
[2]: http://i.stack.imgur.com/30t8F.png

