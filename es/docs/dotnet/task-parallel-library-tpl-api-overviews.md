---
title: "Descripción general de la API de la biblioteca paralela de tareas (TPL)"
slug: "descripcion-general-de-la-api-de-la-biblioteca-paralela-de-tareas-tpl"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

Task Parallel Library es un conjunto de tipos públicos y API que simplifican drásticamente el proceso de agregar paralelismo y concurrencia a una aplicación. .Red. TPL se introdujo en .Net 4 y es la forma recomendada de escribir código multiproceso y paralelo.

TPL se ocupa de la programación del trabajo, la afinidad de subprocesos, el soporte de cancelación, la gestión de estado y el equilibrio de carga para que el programador pueda concentrarse en resolver problemas en lugar de perder tiempo en detalles comunes de bajo nivel.

## Realice el trabajo en respuesta a un clic de botón y actualice la interfaz de usuario
Este ejemplo demuestra cómo puede responder a un clic de botón al realizar algún trabajo en un subproceso de trabajo y luego actualizar la interfaz de usuario para indicar la finalización.

    void MyButton_OnClick(object sender, EventArgs args)
    {
        Task.Run(() => // Schedule work using the thread pool
            {
                System.Threading.Thread.Sleep(5000); // Sleep for 5 seconds to simulate work.
            })
        .ContinueWith(p => // this continuation contains the 'update' code to run on the UI thread
        {
            this.TextBlock_ResultText.Text = "The work completed at " + DateTime.Now.ToString()
        },
        TaskScheduler.FromCurrentSynchronizationContext()); // make sure the update is run on the UI thread.
    
    }


