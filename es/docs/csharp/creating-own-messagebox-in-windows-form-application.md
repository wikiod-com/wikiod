---
title: "Creación de un cuadro de mensaje propio en la aplicación de formulario de Windows"
slug: "creacion-de-un-cuadro-de-mensaje-propio-en-la-aplicacion-de-formulario-de-windows"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Primero necesitamos saber qué es un MessageBox...

El control MessageBox muestra un mensaje con un texto específico y se puede personalizar especificando una imagen personalizada, un título y conjuntos de botones (estos conjuntos de botones permiten al usuario elegir más que una respuesta básica sí/no).

Al crear nuestro propio MessageBox, podemos reutilizar ese MessageBox Control en cualquier aplicación nueva simplemente usando el dll generado o copiando el archivo que contiene la clase.

## Sintaxis
- 'resultado de DialogResult estático = DialogResult.No; //DialogResult es devuelto por diálogos después del cierre.'

## Creación de un control MessageBox propio.
Para crear nuestro propio control MessageBox, simplemente siga la guía a continuación...

1. Abra su instancia de Visual Studio (VS 2008/2010/2012/2015/2017)

2. Vaya a la barra de herramientas en la parte superior y haga clic en Archivo -> Nuevo proyecto --> Aplicación de Windows Forms --> Asigne un nombre al proyecto y luego haga clic en Aceptar.
3. Una vez cargado, arrastre y suelte un control de botón desde la caja de herramientas (que se encuentra a la izquierda) en el formulario (como se muestra a continuación).

[![ingrese la descripción de la imagen aquí][1]][1]


4. Haga doble clic en el botón y el entorno de desarrollo integrado generará automáticamente el controlador de eventos de clic para usted.

5. Edite el código del formulario para que tenga el siguiente aspecto (puede hacer clic con el botón derecho en el formulario y hacer clic en Editar código):


    namespace MsgBoxExample {
        public partial class MsgBoxExampleForm : Form {
            //Constructor, called when the class is initialised.
            public MsgBoxExampleForm() {
                InitializeComponent();
            }

            //Called whenever the button is clicked.
            private void btnShowMessageBox_Click(object sender, EventArgs e) {
               CustomMsgBox.Show($"I'm a {nameof(CustomMsgBox)}!", "MSG", "OK");
            }
        }
    }

6. Explorador de soluciones -> Haga clic derecho en su proyecto -> Agregar -> Formulario de Windows y establezca el nombre como "CustomMsgBox.cs"

7. Arrastre un control de botón y etiqueta desde la caja de herramientas hasta el formulario (luego de hacerlo, tendrá un aspecto similar al siguiente formulario):


[![ingrese la descripción de la imagen aquí][2]][2]

8. Ahora escribe el siguiente código en el formulario recién creado:


    private DialogResult result = DialogResult.No;
    public static DialogResult Show(string text, string caption, string btnOkText) {
        var msgBox = new CustomMsgBox();
        msgBox.lblText.Text = text; //The text for the label...
        msgBox.Text = caption; //Title of form
        msgBox.btnOk.Text = btnOkText; //Text on the button
        //This method is blocking, and will only return once the user
        //clicks ok or closes the form.
        msgBox.ShowDialog(); 
        return result;
    }

    private void btnOk_Click(object sender, EventArgs e) {
        result = DialogResult.Yes;
        MsgBox.Close();
    }

9. Ahora ejecute el programa simplemente presionando la tecla F5.
Enhorabuena, ha creado un control reutilizable.

[1]: https://i.stack.imgur.com/aW1q1.jpg
[2]: https://i.stack.imgur.com/73c1M.jpg









## Cómo usar el control MessageBox creado en otra aplicación de Windows Forms.
Para encontrar sus archivos .cs existentes, haga clic con el botón derecho en el proyecto en su instancia de Visual Studio y haga clic en Abrir carpeta en el Explorador de archivos.

1. Visual Studio --> Su proyecto actual (Windows Form) --> Explorador de soluciones --> Nombre del proyecto --> Haga clic con el botón derecho --> Agregar --> Elemento existente --> Luego ubique su archivo .cs existente.

2. Ahora hay una última cosa que hacer para usar el control. Agregue una declaración de uso a su código, para que su ensamblaje conozca sus dependencias.

       using System;
       using System.Collections.Generic;
       using System.ComponentModel;
       using System.Data;
       using System.Drawing;
       .
       .
       .
       using CustomMsgBox; //Here's the using statement for our dependency.

3. Para mostrar el cuadro de mensaje, simplemente use lo siguiente...

    CustomMsgBox.Show("Your Message for Message Box...","MSG","OK");

