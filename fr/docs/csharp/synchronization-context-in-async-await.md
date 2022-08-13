---
title: "Contexte de synchronisation dans Async-Await"
slug: "contexte-de-synchronisation-dans-async-await"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Pseudocode pour les mots-clés async/wait
Prenons une méthode asynchrone simple :

    async Task Foo()
    {
        Bar();
        await Baz();
        Qux();
    }

En simplifiant, on peut dire que ce code signifie en fait ce qui suit :

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

Cela signifie que les mots-clés `async`/`wait` utilisent le contexte de synchronisation actuel s'il existe. C'est à dire. vous pouvez écrire du code de bibliothèque qui fonctionnerait correctement dans les applications d'interface utilisateur, Web et de console.

[Article source] (https://blogs.msdn.microsoft.com/pfxteam/2012/01/20/await-synchronizationcontext-and-console-apps/).

## Désactiver le contexte de synchronisation
Pour désactiver le contexte de synchronisation, vous devez appeler la méthode [`ConfigureAwait`](https://msdn.microsoft.com/en-us/library/system.threading.tasks.task.configureawait(v=vs.110).aspx) :

    async Task() Foo()
    {
        await Task.Run(() => Console.WriteLine("Test"));
    }

    . . .

    Foo().ConfigureAwait(false);

> ConfigureAwait fournit un moyen d'éviter le comportement de capture par défaut de SynchronizationContext ; passer false pour le paramètre flowContext empêche l'utilisation de SynchronizationContext pour reprendre l'exécution après l'attente.

Citation de [It's All About the SynchronizationContext](https://msdn.microsoft.com/en-us/magazine/gg598924.aspx).

## Pourquoi SynchronizationContext est-il si important ?
Considérez cet exemple :

    private void button1_Click(object sender, EventArgs e)
    {
        label1.Text = RunTooLong();
    }

Cette méthode gèlera l'application de l'interface utilisateur jusqu'à ce que le "RunTooLong" soit terminé. L'application ne répondra pas.

Vous pouvez essayer d'exécuter le code interne de manière asynchrone :

    private void button1_Click(object sender, EventArgs e)
    {
        Task.Run(() => label1.Text = RunTooLong());
    }

Mais ce code ne s'exécutera pas car le corps interne peut être exécuté sur un thread non-UI et [il ne devrait pas modifier directement les propriétés de l'interface utilisateur] (https://nnish.com/2010/03/14/accessing-wpf-controls- on-a-non-ui-thread/):

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

Maintenant, n'oubliez pas de toujours utiliser ce modèle. Ou essayez [`SynchronizationContext.Post`](https://lostechies.com/gabrielschenker/2009/01/23/synchronizing-calls-to-the-ui-in-a-multi-threaded-application/) qui fais-le pour toi :

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




