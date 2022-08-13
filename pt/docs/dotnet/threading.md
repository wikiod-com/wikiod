---
title: "Rosqueamento"
slug: "rosqueamento"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## Acessando controles de formulário de outros threads
Se você quiser alterar um atributo de um controle, como uma caixa de texto ou rótulo de outro thread que não o thread da GUI que criou o controle, você terá que invocá-lo ou poderá receber uma mensagem de erro informando:

> "Operação entre threads inválida: Controle 'control_name' acessado de um thread diferente do thread em que foi criado."

Usar este código de exemplo em um formulário system.windows.forms lançará uma exceção com essa mensagem:

    private void button4_Click(object sender, EventArgs e)
    {
        Thread thread = new Thread(updatetextbox);
        thread.Start();
    }

    private void updatetextbox()
    {
        textBox1.Text = "updated"; // Throws exception
    }

Em vez disso, quando você deseja alterar o texto de uma caixa de texto de dentro de um thread que não o possui, use Control.Invoke ou Control.BeginInvoke. Você também pode usar Control.InvokeRequired para verificar se é necessário invocar o controle.

    private void updatetextbox()
    {
        if (textBox1.InvokeRequired)
            textBox1.BeginInvoke((Action)(() => textBox1.Text = "updated"));
        else
            textBox1.Text = "updated";
    }

Se você precisar fazer isso com frequência, poderá escrever uma extensão para objetos invocáveis ​​para reduzir a quantidade de código necessária para fazer essa verificação:

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

E atualizar a caixa de texto de qualquer thread fica um pouco mais simples:

    private void updatetextbox()
    {
        textBox1.BeginInvokeIfRequired(() => textBox1.Text = "updated");
    }

Esteja ciente de que Control.BeginInvoke, conforme usado neste exemplo, é assíncrono, o que significa que o código que vem após uma chamada para Control.BeginInvoke pode ser executado imediatamente depois, independentemente de o delegado passado já ter sido executado.

Se você precisar ter certeza de que textBox1 foi atualizado antes de continuar, use Control.Invoke, que bloqueará o thread de chamada até que seu delegado seja executado. Observe que essa abordagem pode desacelerar seu código significativamente se você fizer muitas chamadas de invocação e observe que ela travará seu aplicativo se o thread da GUI estiver aguardando o thread de chamada concluir ou liberar um recurso retido.

