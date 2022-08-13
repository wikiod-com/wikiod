---
title: "Contexto de sincronização em Async-Await"
slug: "contexto-de-sincronizacao-em-async-await"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Pseudocódigo para palavras-chave async/await
Considere um método assíncrono simples:

    async Task Foo()
    {
        Bar();
        await Baz();
        Qux();
    }

Simplificando, podemos dizer que esse código realmente significa o seguinte:

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

Isso significa que as palavras-chave `async`/`await` usam o contexto de sincronização atual, se ele existir. Ou seja você pode escrever código de biblioteca que funcionaria corretamente em aplicativos de interface do usuário, Web e console.

[Artigo de origem](https://blogs.msdn.microsoft.com/pfxteam/2012/01/20/await-synchronizationcontext-and-console-apps/).

## Desativando o contexto de sincronização
Para desabilitar o contexto de sincronização, você deve chamar o método [`ConfigureAwait`](https://msdn.microsoft.com/en-us/library/system.threading.tasks.task.configureawait(v=vs.110).aspx) :

    async Task() Foo()
    {
        await Task.Run(() => Console.WriteLine("Test"));
    }

    . . .

    Foo().ConfigureAwait(false);

> ConfigureAwait fornece um meio para evitar o comportamento de captura padrão do SynchronizationContext; passar false para o parâmetro flowContext impede que o SynchronizationContext seja usado para retomar a execução após a espera.

Citação de [It's All About the SynchronizationContext](https://msdn.microsoft.com/en-us/magazine/gg598924.aspx).

## Por que SynchronizationContext é tão importante?
Considere este exemplo:

    private void button1_Click(object sender, EventArgs e)
    {
        label1.Text = RunTooLong();
    }

Este método irá congelar o aplicativo de interface do usuário até que o `RunTooLong` seja concluído. O aplicativo não responderá.

Você pode tentar executar o código interno de forma assíncrona:

    private void button1_Click(object sender, EventArgs e)
    {
        Task.Run(() => label1.Text = RunTooLong());
    }

Mas esse código não será executado porque o corpo interno pode ser executado em threads que não são da interface do usuário e [não deve alterar as propriedades da interface do usuário diretamente](https://nnish.com/2010/03/14/accessing-wpf-controls- em um thread não-ui/):

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

Agora não se esqueça de sempre usar este padrão. Ou tente [`SynchronizationContext.Post`](https://lostechies.com/gabrielschenker/2009/01/23/synchronizing-calls-to-the-ui-in-a-multi-threaded-application/) que faça para você:

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




