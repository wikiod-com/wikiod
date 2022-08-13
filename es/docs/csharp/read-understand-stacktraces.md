---
title: "Leer y comprender Stacktraces"
slug: "leer-y-comprender-stacktraces"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

Un seguimiento de pila es una gran ayuda al depurar un programa. Obtendrá un seguimiento de la pila cuando su programa arroje una excepción y, a veces, cuando el programa finalice de manera anormal.

## Rastreo de pila para una NullReferenceException simple en Windows Forms
Vamos a crear un pequeño fragmento de código que arroje una excepción:

    private void button1_Click(object sender, EventArgs e)
    {
        string msg = null;
        msg.ToCharArray();
    }

Si ejecutamos esto, obtenemos la siguiente Excepción y seguimiento de pila:

    System.NullReferenceException: "Object reference not set to an instance of an object."
       at WindowsFormsApplication1.Form1.button1_Click(Object sender, EventArgs e) in F:\WindowsFormsApplication1\WindowsFormsApplication1\Form1.cs:line 29
       at System.Windows.Forms.Control.OnClick(EventArgs e)
       at System.Windows.Forms.Button.OnClick(EventArgs e)
       at System.Windows.Forms.Button.OnMouseUp(MouseEventArgs mevent)

El seguimiento de la pila continúa así, pero esta parte será suficiente para nuestros propósitos.

En la parte superior del seguimiento de la pila, vemos la línea:

> en WindowsFormsApplication1.Form1.button1_Click(Object sender, EventArgs e) en F:\WindowsFormsApplication1\WindowsFormsApplication1\Form1.cs:línea 29

Esta es la parte más importante. Nos dice la línea _exacta_ donde ocurrió la Excepción: línea 29 en Form1.cs .
Entonces, aquí es donde comienza su búsqueda.

La segunda línea es

> en System.Windows.Forms.Control.OnClick(EventArgs e)

Este es el método que llamó `button1_Click`. Así que ahora sabemos que `button1_Click`, donde ocurrió el error, fue llamado desde `System.Windows.Forms.Control.OnClick`.

Podemos continuar así; la tercera linea es

> en System.Windows.Forms.Button.OnClick(EventArgs e)

Este es, a su vez, el código que llamó a `System.windows.Forms.Control.OnClick`.

El seguimiento de la pila es la lista de funciones que se llamaron hasta que su código encontró la excepción.
¡Y al seguir esto, puede averiguar qué ruta de ejecución siguió su código hasta que tuvo problemas!

Tenga en cuenta que el seguimiento de la pila incluye llamadas del sistema .Net; normalmente no necesita seguir todo el código `System.Windows.Forms` de Microsoft para averiguar qué salió mal, solo el código que pertenece a su propia aplicación.


Entonces, ¿por qué se llama esto un "rastreo de pila"?
Porque, cada vez que un programa llama a un método, realiza un seguimiento de dónde estaba. Tiene una estructura de datos llamada "pila", donde vuelca su última ubicación.
Si ha terminado de ejecutar el método, mira en la pila para ver dónde estaba antes de llamar al método, y continúa desde allí.

Entonces, la pila le permite a la computadora saber dónde se quedó, antes de llamar a un nuevo método.

Pero también sirve como ayuda para la depuración. Al igual que un detective que rastrea los pasos que tomó un criminal al cometer su crimen, un programador puede usar la pila para rastrear los pasos que tomó un programa antes de fallar.





