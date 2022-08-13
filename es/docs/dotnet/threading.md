---
title: "enhebrar"
slug: "enhebrar"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## Acceso a controles de formulario desde otros subprocesos
Si desea cambiar un atributo de un control, como un cuadro de texto o una etiqueta de otro subproceso que no sea el subproceso de GUI que creó el control, deberá invocarlo o, de lo contrario, podría recibir un mensaje de error que indique:

> "La operación de subprocesos cruzados no es válida: se accedió al control 'control_name' desde un subproceso que no es el subproceso en el que se creó".

El uso de este código de ejemplo en un formulario system.windows.forms emitirá una excepción con ese mensaje:

    private void button4_Click(object sender, EventArgs e)
    {
        Thread thread = new Thread(updatetextbox);
        thread.Start();
    }

    private void updatetextbox()
    {
        textBox1.Text = "updated"; // Throws exception
    }

En cambio, cuando desee cambiar el texto de un cuadro de texto desde dentro de un hilo que no lo posee, use Control.Invoke o Control.BeginInvoke. También puede usar Control.InvokeRequired para verificar si es necesario invocar el control.

    private void updatetextbox()
    {
        if (textBox1.InvokeRequired)
            textBox1.BeginInvoke((Action)(() => textBox1.Text = "updated"));
        else
            textBox1.Text = "updated";
    }

Si necesita hacer esto con frecuencia, puede escribir una extensión para objetos invocables para reducir la cantidad de código necesario para realizar esta verificación:

    public static class Extensions
    {
        public static void BeginInvokeIfRequired(this ISynchronizeInvoke obj, Action action)
        {
            if (obj.InvokeRequired)
                obj.BeginInvoke(action, new object[0]);
            else
                action();
        }
    }

Y actualizar el cuadro de texto desde cualquier hilo se vuelve un poco más simple:

    private void updatetextbox()
    {
        textBox1.BeginInvokeIfRequired(() => textBox1.Text = "updated");
    }

Tenga en cuenta que Control.BeginInvoke, como se usa en este ejemplo, es asíncrono, lo que significa que el código que viene después de una llamada a Control.BeginInvoke se puede ejecutar inmediatamente después, ya sea que el delegado pasado se haya ejecutado o no.

Si necesita asegurarse de que textBox1 esté actualizado antes de continuar, use Control.Invoke en su lugar, lo que bloqueará el hilo de llamada hasta que se haya ejecutado su delegado. Tenga en cuenta que este enfoque puede ralentizar significativamente su código si realiza muchas llamadas de invocación y tenga en cuenta que bloqueará su aplicación si su subproceso de GUI está esperando que el subproceso de llamada se complete o libere un recurso retenido.

