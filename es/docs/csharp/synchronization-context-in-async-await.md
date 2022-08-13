---
title: "Contexto de sincronización en Async-Await"
slug: "contexto-de-sincronizacion-en-async-await"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Pseudocódigo para palabras clave async/await
Considere un método asíncrono simple:

    async Task Foo()
    {
        Bar();
        await Baz();
        Qux();
    }

Simplificando, podemos decir que este código en realidad significa lo siguiente:

    Task Foo()
    {
        Bar();
        Task t = Baz();
        var context = SynchronizationContext.Current;
        t.ContinueWith(task) =>
        {
            if (context == null)
                Qux();
            else
                context.Post((obj) => Qux(), null);
        }, TaskScheduler.Current);

        return t;
    }

Significa que las palabras clave `async`/`await` utilizan el contexto de sincronización actual, si existe. Es decir. puede escribir código de biblioteca que funcione correctamente en aplicaciones de interfaz de usuario, web y consola.

[Artículo de origen](https://blogs.msdn.microsoft.com/pfxteam/2012/01/20/await-synchronizationcontext-and-console-apps/).

## Deshabilitar el contexto de sincronización
Para deshabilitar el contexto de sincronización, debe llamar al método [`ConfigureAwait`](https://msdn.microsoft.com/en-us/library/system.threading.tasks.task.configureawait(v=vs.110).aspx) :

    async Task() Foo()
    {
        await Task.Run(() => Console.WriteLine("Test"));
    }

    . . .

    Foo().ConfigureAwait(false);

> ConfigureAwait proporciona un medio para evitar el comportamiento predeterminado de captura de SynchronizationContext; pasar false para el parámetro flowContext evita que SynchronizationContext se use para reanudar la ejecución después de await.

Cita de [It's All About the SynchronizationContext](https://msdn.microsoft.com/en-us/magazine/gg598924.aspx).

## ¿Por qué SynchronizationContext es tan importante?
Considere este ejemplo:

    private void button1_Click(object sender, EventArgs e)
    {
        label1.Text = RunTooLong();
    }

Este método congelará la aplicación de interfaz de usuario hasta que se complete `RunTooLong`. La aplicación no responderá.

Puede intentar ejecutar el código interno de forma asíncrona:

    private void button1_Click(object sender, EventArgs e)
    {
        Task.Run(() => label1.Text = RunTooLong());
    }

Pero este código no se ejecutará porque el cuerpo interno puede ejecutarse en un subproceso que no sea de la interfaz de usuario y [no debería cambiar las propiedades de la interfaz de usuario directamente] (https://nnish.com/2010/03/14/accessing-wpf-controls- en un subproceso no ui/):

    private void button1_Click(object sender, EventArgs e)
    {
        Task.Run(() =>
        {
            var label1Text = RunTooLong();

            if (label1.InvokeRequired)
                lable1.BeginInvoke((Action) delegate() { label1.Text = label1Text; });
            else
                label1.Text = label1Text;
        });
    }

Ahora no olvides usar siempre este patrón. O pruebe [`SynchronizationContext.Post`](https://lostechies.com/gabrielschenker/2009/01/23/synchronizing-calls-to-the-ui-in-a-multi-threaded-application/) que hazlo por ti:

    private void button1_Click(object sender, EventArgs e)
    {
        Task.Run(() =>
        {
            var label1Text = RunTooLong();
            SynchronizationContext.Current.Post((obj) =>
            {
                label1.Text = label1    Text);
            }, null);
        });
    }




