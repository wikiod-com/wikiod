---
title: "Contextos de Sincronização"
slug: "contextos-de-sincronizacao"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Um Contexto de Sincronização é uma abstração que permite consumir código para passar unidades de trabalho para um agendador, sem exigir conhecimento de como o trabalho será agendado.

Os contextos de sincronização são tradicionalmente usados ​​para garantir que o código seja executado em um thread específico. Em aplicativos WPF e Winforms, um `SynchronizationContext` representando o thread da interface do usuário é fornecido pela estrutura de apresentação. Desta forma, o `SynchronizationContext` pode ser pensado como um padrão produtor-consumidor para delegados. Um thread de trabalho _produz_ código executável (o delegado) e o enfileira ou _consumo_ pelo loop de mensagens da interface do usuário.

A Biblioteca Paralela de Tarefas fornece recursos para capturar e usar contextos de sincronização automaticamente.

## Execute o código no thread da interface do usuário após realizar o trabalho em segundo plano
Este exemplo mostra como atualizar um componente de interface do usuário de um thread em segundo plano usando um `SynchronizationContext`


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

Neste exemplo, se você tentasse atualizar diretamente `MyTextBox.Text` dentro do loop `for`, você obteria um erro de encadeamento. Ao postar a ação `UpdateCallback` no `SynchronizationContext`, a caixa de texto é atualizada no mesmo encadeamento que o restante da interface do usuário.

Na prática, as atualizações de progresso devem ser executadas usando uma instância de `System.IProgress<T>`. A implementação padrão `System.Progress<T>` captura automaticamente o contexto de sincronização em que é criado.

