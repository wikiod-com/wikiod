---
title: "Criando o próprio MessageBox no aplicativo Windows Form"
slug: "criando-o-proprio-messagebox-no-aplicativo-windows-form"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Primeiro precisamos saber o que é um MessageBox...

O controle MessageBox exibe uma mensagem com texto especificado e pode ser personalizado especificando uma imagem personalizada, um título e conjuntos de botões (esses conjuntos de botões permitem que o usuário escolha mais do que uma resposta básica sim/não).

Ao criar nosso próprio MessageBox, podemos reutilizar esse MessageBox Control em qualquer novo aplicativo apenas usando a dll gerada ou copiando o arquivo que contém a classe.

## Sintaxe
- 'Resultado estático do DialogResult = DialogResult.No; //DialogResult é retornado por diálogos após a dispensa.'

## Criando o próprio controle MessageBox.
Para criar nosso próprio controle MessageBox basta seguir o guia abaixo...

1. Abra sua instância do Visual Studio (VS 2008/2010/2012/2015/2017)

2. Vá para a barra de ferramentas na parte superior e clique em Arquivo -> Novo Projeto -> Aplicativo Windows Forms -> Dê um nome ao projeto e clique em ok.
3. Uma vez carregado, arraste e solte um controle de botão da Caixa de ferramentas (encontrada à esquerda) no formulário (como mostrado abaixo).

[![digite a descrição da imagem aqui][1]][1]


4. Clique duas vezes no botão e o Ambiente de Desenvolvimento Integrado gerará automaticamente o manipulador de eventos de clique para você.

5. Edite o código do formulário para que fique parecido com o seguinte (você pode clicar com o botão direito do mouse no formulário e clicar em Editar código):


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

6. Gerenciador de Soluções -> Clique com o botão direito do mouse em seu projeto -> Adicionar -> Windows Form e defina o nome como "CustomMsgBox.cs"

7. Arraste um controle de botão e rótulo da caixa de ferramentas para o formulário (será parecido com o formulário abaixo depois de fazer isso):


[![digite a descrição da imagem aqui][2]][2]

8. Agora escreva o código abaixo no formulário recém-criado:


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

9. Agora execute o programa apenas pressionando a tecla F5.
Parabéns, você fez um controle reutilizável.

[1]: https://i.stack.imgur.com/aW1q1.jpg
[2]: https://i.stack.imgur.com/73c1M.jpg









## Como usar o próprio controle MessageBox criado em outro aplicativo Windows Form.
Para localizar seus arquivos .cs existentes, clique com o botão direito do mouse no projeto em sua instância do Visual Studio e clique em Abrir Pasta no Explorador de Arquivos.

1. Visual Studio --> Seu projeto atual (Windows Form) --> Solution Explorer --> Project Name --> Right Click --> Add --> Existing Item --> Em seguida, localize seu arquivo .cs existente.

2. Agora há uma última coisa a fazer para usar o controle. Adicione uma instrução using ao seu código, para que seu assembly saiba sobre suas dependências.

       using System;
       using System.Collections.Generic;
       using System.ComponentModel;
       using System.Data;
       using System.Drawing;
       .
       .
       .
       using CustomMsgBox; //Here's the using statement for our dependency.

3. Para exibir a caixa de mensagem, basta usar o seguinte...

    CustomMsgBox.Show("Your Message for Message Box...","MSG","OK");

