---
title: "Lire et comprendre Stacktraces"
slug: "lire-et-comprendre-stacktraces"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

Une trace de pile est une aide précieuse lors du débogage d'un programme. Vous obtiendrez une trace de la pile lorsque votre programme lève une exception, et parfois lorsque le programme se termine anormalement.

## Stack trace pour une simple NullReferenceException dans Windows Forms
Créons un petit morceau de code qui lève une exception :

    private void button1_Click(object sender, EventArgs e)
    {
        string msg = null;
        msg.ToCharArray();
    }

Si nous exécutons ceci, nous obtenons l'exception et la trace de pile suivantes :

    System.NullReferenceException: "Object reference not set to an instance of an object."
       at WindowsFormsApplication1.Form1.button1_Click(Object sender, EventArgs e) in F:\WindowsFormsApplication1\WindowsFormsApplication1\Form1.cs:line 29
       at System.Windows.Forms.Control.OnClick(EventArgs e)
       at System.Windows.Forms.Button.OnClick(EventArgs e)
       at System.Windows.Forms.Button.OnMouseUp(MouseEventArgs mevent)

La trace de la pile continue comme ça, mais cette partie suffira pour nos besoins.

En haut de la trace de la pile, nous voyons la ligne :

> à WindowsFormsApplication1.Form1.button1_Click(Object sender, EventArgs e) dans F:\WindowsFormsApplication1\WindowsFormsApplication1\Form1.cs:line 29

C'est la partie la plus importante. Il nous indique la ligne _exact_ où l'exception s'est produite : ligne 29 dans Form1.cs .
Donc, c'est là que vous commencez votre recherche.

La deuxième ligne est

> à System.Windows.Forms.Control.OnClick(EventArgs e)

Il s'agit de la méthode appelée `button1_Click`. Nous savons donc maintenant que `button1_Click`, où l'erreur s'est produite, a été appelé à partir de `System.Windows.Forms.Control.OnClick`.

Nous pouvons continuer ainsi; la troisième ligne est

> sur System.Windows.Forms.Button.OnClick(EventArgs e)

C'est, à son tour, le code qui a appelé `System.windows.Forms.Control.OnClick`.

La trace de la pile est la liste des fonctions qui ont été appelées jusqu'à ce que votre code rencontre l'exception.
Et en suivant cela, vous pouvez déterminer quel chemin d'exécution votre code a suivi jusqu'à ce qu'il rencontre des problèmes !

Notez que la trace de la pile inclut les appels du système .Net ; vous n'avez normalement pas besoin de suivre tout le code `System.Windows.Forms` de Microsoft pour découvrir ce qui ne va pas, seulement le code qui appartient à votre propre application.


Alors, pourquoi cela s'appelle-t-il une "trace de pile" ?
Parce que chaque fois qu'un programme appelle une méthode, il garde une trace de l'endroit où elle se trouvait. Il a une structure de données appelée "pile", où il vide son dernier emplacement.
S'il a fini d'exécuter la méthode, il regarde sur la pile pour voir où il se trouvait avant d'appeler la méthode - et continue à partir de là.

Ainsi, la pile indique à l'ordinateur où elle s'est arrêtée, avant d'appeler une nouvelle méthode.

Mais il sert également d'aide au débogage. Comme un détective retraçant les étapes suivies par un criminel lors de la perpétration de son crime, un programmeur peut utiliser la pile pour retracer les étapes suivies par un programme avant qu'il ne tombe en panne.





