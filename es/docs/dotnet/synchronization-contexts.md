---
title: "Contextos de sincronización"
slug: "contextos-de-sincronizacion"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Un contexto de sincronización es una abstracción que permite consumir código para pasar unidades de trabajo a un planificador, sin necesidad de saber cómo se planificará el trabajo.

Los contextos de sincronización se usan tradicionalmente para garantizar que el código se ejecute en un subproceso específico. En las aplicaciones WPF y Winforms, el marco de presentación proporciona un `SynchronizationContext` que representa el subproceso de la interfaz de usuario. De esta forma, `SynchronizationContext` se puede considerar como un patrón productor-consumidor para los delegados. Un subproceso de trabajo _producirá_ código ejecutable (el delegado) y lo pondrá en cola o _consumirá_ mediante el bucle de mensajes de la interfaz de usuario.

La biblioteca paralela de tareas proporciona funciones para capturar y usar automáticamente contextos de sincronización.

## Ejecutar código en el subproceso de la interfaz de usuario después de realizar el trabajo en segundo plano
Este ejemplo muestra cómo actualizar un componente de la interfaz de usuario desde un subproceso en segundo plano mediante un `SynchronizationContext`


    void Button_Click(object sender, EventArgs args)
    {
        SynchronizationContext context = SynchronizationContext.Current;
        Task.Run(() =>
        {
            for(int i = 0; i < 10; i++) 
            {
                Thread.Sleep(500); //simulate work being done
                context.Post(ShowProgress, "Work complete on item " + i);
            }
        }
    }

    void UpdateCallback(object state)
    {
        // UI can be safely updated as this method is only called from the UI thread
        this.MyTextBox.Text = state as string;
    }

En este ejemplo, si intentara actualizar directamente `MyTextBox.Text` dentro del bucle `for`, obtendría un error de subprocesamiento. Al publicar la acción `UpdateCallback` en `SynchronizationContext`, el cuadro de texto se actualiza en el mismo hilo que el resto de la interfaz de usuario.

En la práctica, las actualizaciones de progreso deben realizarse mediante una instancia de `System.IProgress<T>`. La implementación predeterminada `System.Progress<T>` captura automáticamente el contexto de sincronización en el que se crea.

