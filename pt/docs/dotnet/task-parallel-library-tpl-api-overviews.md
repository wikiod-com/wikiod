---
title: "Visão geral da API da biblioteca paralela de tarefas (TPL)"
slug: "visao-geral-da-api-da-biblioteca-paralela-de-tarefas-tpl"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

A Biblioteca Paralela de Tarefas é um conjunto de tipos públicos e APIs que simplificam drasticamente o processo de adição de paralelismo e simultaneidade a um aplicativo. .Internet. A TPL foi introduzida no .Net 4 e é a maneira recomendada de escrever código multi-thread e paralelo.

A TPL cuida do agendamento de trabalho, afinidade de thread, suporte a cancelamento, gerenciamento de estado e balanceamento de carga para que o programador possa se concentrar na solução de problemas em vez de gastar tempo em detalhes comuns de baixo nível.

## Execute o trabalho em resposta a um clique de botão e atualize a interface do usuário
Este exemplo demonstra como você pode responder a um clique de botão realizando algum trabalho em um thread de trabalho e, em seguida, atualizar a interface do usuário para indicar a conclusão

    void MyButton_OnClick(object sender, EventArgs args)
    {
        Task.Run(() => // Schedule work using the thread pool
            {
                System.Threading.Thread.Sleep(5000); // Sleep for 5 seconds to simulate work.
            })
        .ContinueWith(p => // this continuation contains the 'update' code to run on the UI thread
        {
            this.TextBlock_ResultText.Text = "The work completed at " + DateTime.Now.ToString()
        },
        TaskScheduler.FromCurrentSynchronizationContext()); // make sure the update is run on the UI thread.
    
    }


