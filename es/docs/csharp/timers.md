---
title: "Temporizadores"
slug: "temporizadores"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Sintaxis
- `myTimer.Interval` - establece la frecuencia con la que se llama al evento "Tick" (en milisegundos)
- `myTimer.Enabled` - valor booleano que establece que el temporizador esté habilitado/deshabilitado
- `myTimer.Start()` - Inicia el temporizador.
- `myTimer.Stop()` - Detiene el temporizador.

Si usa Visual Studio, los temporizadores se pueden agregar como un control directamente a su formulario desde la caja de herramientas.

## Temporizadores de subprocesos múltiples
`System.Threading.Timer`: el temporizador multiproceso más simple. Contiene dos métodos y un constructor.

Ejemplo:
Un temporizador llama al método DataWrite, que escribe "multiproceso ejecutado..." después de cinco
han transcurrido segundos, y luego cada segundo hasta que el usuario presiona Enter:

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

Nota: publicará una sección separada para desechar temporizadores de subprocesos múltiples.

`Cambiar`: se puede llamar a este método cuando desee cambiar el intervalo del temporizador.

`Timeout.Infinite`: si desea disparar solo una vez. Especifique esto en el último argumento del constructor.

`System.Timers`: otra clase de temporizador proporcionada por .NET Framework. Envuelve `System.Threading.Timer`.

Características:
---------

- `IComponent` - Permitir que se ubique en la bandeja de componentes del Diseñador de Visual Studio
- Propiedad `Interval` en lugar de un método `Change`
- `Evento` `transcurrido` en lugar de `delegado` de devolución de llamada
- Propiedad `Habilitada` para iniciar y detener el temporizador (`valor predeterminado = falso`)
- Métodos `Start` y `Stop` en caso de que se confunda con la propiedad `Habilitado` (punto anterior)
- `AutoReset` - para indicar un evento recurrente (`valor predeterminado = verdadero`)
- Propiedad `SynchronizingObject` con los métodos `Invoke` y `BeginInvoke` para
llamar de forma segura a métodos en elementos WPF y controles de Windows Forms


Ejemplo que representa todas las características anteriores:

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

`Temporizadores de subprocesos múltiples`: use el grupo de subprocesos para permitir que algunos subprocesos sirvan a muchos
temporizadores Significa que el método de devolución de llamada o el evento 'Elapsed' pueden activarse en un diferente
subproceso cada vez que se llama.

`Elapsed`: este evento siempre se activa a tiempo, independientemente de si el evento `Elapsed` anterior terminó de ejecutarse. Debido a esto, las devoluciones de llamada o los controladores de eventos deben ser seguros para subprocesos.
La precisión de los temporizadores de subprocesos múltiples depende del sistema operativo y, por lo general, es
en los 10-20 ms.

`interop`: siempre que necesite una mayor precisión, use esto y llame al temporizador multimedia de Windows. Esto tiene una precisión de hasta 1 ms y está definido en `winmm.dll`.

`timeBeginPeriod`: llame a este primero para informar al sistema operativo que necesita una alta precisión de tiempo

`timeSetEvent`: llama a esto después de `timeBeginPeriod` para iniciar un temporizador multimedia.

`timeKillEvent`: llame a esto cuando haya terminado, esto detiene el temporizador

`timeEndPeriod`: llámelo para informar al sistema operativo que ya no necesita una alta precisión de tiempo.

Puede encontrar ejemplos completos en Internet que usan el temporizador multimedia buscando las palabras clave `dllimport` `winmm.dll` `timesetevent`.

## Creación de una instancia de un temporizador
Los temporizadores se utilizan para realizar tareas en intervalos de tiempo específicos (Hacer X cada Y segundos)
A continuación se muestra un ejemplo de creación de una nueva instancia de un temporizador.

**NOTA**: Esto se aplica a los temporizadores que utilizan WinForms. Si usa WPF, es posible que desee buscar en `DispatcherTimer`


        using System.Windows.Forms; //Timers use the Windows.Forms namespace

        public partial class Form1 : Form
        {

            Timer myTimer = new Timer(); //create an instance of Timer named myTimer
   
        
            public Form1()
            {
                InitializeComponent();
            }

        }

## Asignar el controlador de eventos "Tick" a un temporizador
Todas las acciones realizadas en un temporizador se manejan en el evento "Tick".

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

## Ejemplo: usar un temporizador para realizar una cuenta regresiva simple.
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

Resultados en...

[![ingrese la descripción de la imagen aquí][1]][1][![ingrese la descripción de la imagen aquí][2]][2]

Y así...


[1]: http://i.stack.imgur.com/VZlnr.png
[2]: http://i.stack.imgur.com/30t8F.png

