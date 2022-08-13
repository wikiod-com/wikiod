---
title: "Contextes de synchronisation"
slug: "contextes-de-synchronisation"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Un contexte de synchronisation est une abstraction qui permet de consommer du code pour transmettre des unités de travail à un planificateur, sans nécessiter de savoir comment le travail sera planifié.

Les contextes de synchronisation sont traditionnellement utilisés pour s'assurer que le code est exécuté sur un thread spécifique. Dans les applications WPF et Winforms, un `SynchronizationContext` représentant le thread d'interface utilisateur est fourni par le framework de présentation. De cette façon, `SynchronizationContext` peut être considéré comme un modèle producteur-consommateur pour les délégués. Un thread de travail _produira_ du code exécutable (le délégué) et le mettra en file d'attente ou _consommation_ par la boucle de messages de l'interface utilisateur.

La bibliothèque parallèle de tâches fournit des fonctionnalités permettant de capturer et d'utiliser automatiquement des contextes de synchronisation.

## Exécuter le code sur le thread de l'interface utilisateur après avoir effectué un travail en arrière-plan
Cet exemple montre comment mettre à jour un composant d'interface utilisateur à partir d'un thread d'arrière-plan à l'aide d'un `SynchronizationContext`


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

Dans cet exemple, si vous essayez de mettre à jour directement `MyTextBox.Text` dans la boucle `for`, vous obtiendrez une erreur de thread. En publiant l'action `UpdateCallback` dans `SynchronizationContext`, la zone de texte est mise à jour sur le même fil que le reste de l'interface utilisateur.

En pratique, les mises à jour de progression doivent être effectuées à l'aide d'une instance de `System.IProgress<T>`. L'implémentation par défaut `System.Progress<T>` capture automatiquement le contexte de synchronisation sur lequel elle est créée.

