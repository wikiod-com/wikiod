---
title: "Ler e entender Stacktraces"
slug: "ler-e-entender-stacktraces"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

Um rastreamento de pilha é uma grande ajuda ao depurar um programa. Você obterá um rastreamento de pilha quando seu programa lançar uma exceção e, às vezes, quando o programa terminar de forma anormal.

## Stack trace para uma simples NullReferenceException no Windows Forms
Vamos criar um pequeno pedaço de código que lança uma exceção:

    private void button1_Click(object sender, EventArgs e)
    {
        string msg = null;
        msg.ToCharArray();
    }

Se executarmos isso, obteremos a seguinte exceção e rastreamento de pilha:

    System.NullReferenceException: "Object reference not set to an instance of an object."
       at WindowsFormsApplication1.Form1.button1_Click(Object sender, EventArgs e) in F:\WindowsFormsApplication1\WindowsFormsApplication1\Form1.cs:line 29
       at System.Windows.Forms.Control.OnClick(EventArgs e)
       at System.Windows.Forms.Button.OnClick(EventArgs e)
       at System.Windows.Forms.Button.OnMouseUp(MouseEventArgs mevent)

O rastreamento de pilha continua assim, mas esta parte será suficiente para nossos propósitos.

No topo do rastreamento de pilha, vemos a linha:

> em WindowsFormsApplication1.Form1.button1_Click(Object sender, EventArgs e) em F:\WindowsFormsApplication1\WindowsFormsApplication1\Form1.cs:line 29

Esta é a parte mais importante. Ele nos informa a linha _exact_ onde ocorreu a exceção: linha 29 em Form1.cs .
Então, é aqui que você começa sua busca.

A segunda linha é

> em System.Windows.Forms.Control.OnClick(EventArgs e)

Este é o método que chamou `button1_Click`. Então agora sabemos que `button1_Click`, onde ocorreu o erro, foi chamado de `System.Windows.Forms.Control.OnClick`.

Podemos continuar assim; a terceira linha é

> em System.Windows.Forms.Button.OnClick(EventArgs e)

Este é, por sua vez, o código que chamou `System.windows.Forms.Control.OnClick`.

O rastreamento de pilha é a lista de funções que foram chamadas até que seu código encontrou a exceção.
E seguindo isso, você pode descobrir qual caminho de execução seu código seguiu até ter problemas!

Observe que o rastreamento de pilha inclui chamadas do sistema .Net; você normalmente não precisa seguir todo o código `System.Windows.Forms` da Microsoft para descobrir o que deu errado, apenas o código que pertence ao seu próprio aplicativo.


Então, por que isso é chamado de "rastreamento de pilha"?
Porque, toda vez que um programa chama um método, ele mantém o controle de onde ele estava. Ele possui uma estrutura de dados chamada "stack", onde despeja seu último local.
Se terminar de executar o método, ele procura na pilha para ver onde estava antes de chamar o método - e continua a partir daí.

Assim, a pilha permite que o computador saiba onde parou, antes de chamar um novo método.

Mas também serve como uma ajuda de depuração. Como um detetive rastreando os passos que um criminoso deu ao cometer seu crime, um programador pode usar a pilha para rastrear os passos que um programa deu antes de travar.





