---
title: "Aperçus de l'API de la bibliothèque parallèle de tâches (TPL)"
slug: "apercus-de-lapi-de-la-bibliotheque-parallele-de-taches-tpl"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

La bibliothèque parallèle de tâches est un ensemble de types publics et d'API qui simplifient considérablement le processus d'ajout de parallélisme et de concurrence à une application. .Rapporter. TPL a été introduit dans .Net 4 et est la méthode recommandée pour écrire du code multithread et parallèle.

TPL s'occupe de la planification du travail, de l'affinité des threads, de la prise en charge de l'annulation, de la gestion de l'état et de l'équilibrage de charge afin que le programmeur puisse se concentrer sur la résolution des problèmes plutôt que de passer du temps sur des détails communs de bas niveau.

## Effectuez un travail en réponse à un clic sur un bouton et mettez à jour l'interface utilisateur
Cet exemple montre comment vous pouvez répondre à un clic sur un bouton en effectuant un travail sur un thread de travail, puis mettre à jour l'interface utilisateur pour indiquer l'achèvement

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


