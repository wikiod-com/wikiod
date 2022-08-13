---
title: "Enfilage"
slug: "enfilage"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## Accéder aux contrôles de formulaire à partir d'autres threads
Si vous souhaitez modifier un attribut d'un contrôle tel qu'une zone de texte ou une étiquette à partir d'un autre thread que le thread GUI qui a créé le contrôle, vous devrez l'invoquer, sinon vous pourriez recevoir un message d'erreur indiquant :

> "Opération inter-thread non valide : contrôle 'nom_contrôle' accédé à partir d'un thread autre que le thread sur lequel il a été créé."

L'utilisation de cet exemple de code sur un formulaire system.windows.forms lancera une exception avec ce message :

    private void button4_Click(object sender, EventArgs e)
    {
        Thread thread = new Thread(updatetextbox);
        thread.Start();
    }

    private void updatetextbox()
    {
        textBox1.Text = "updated"; // Throws exception
    }

Au lieu de cela, lorsque vous souhaitez modifier le texte d'une zone de texte à partir d'un thread qui ne le possède pas, utilisez Control.Invoke ou Control.BeginInvoke. Vous pouvez également utiliser Control.InvokeRequired pour vérifier si l'appel du contrôle est nécessaire.

    private void updatetextbox()
    {
        if (textBox1.InvokeRequired)
            textBox1.BeginInvoke((Action)(() => textBox1.Text = "updated"));
        else
            textBox1.Text = "updated";
    }

Si vous devez le faire souvent, vous pouvez écrire une extension pour les objets invocables afin de réduire la quantité de code nécessaire pour effectuer cette vérification :

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

Et la mise à jour de la zone de texte à partir de n'importe quel fil devient un peu plus simple :

    private void updatetextbox()
    {
        textBox1.BeginInvokeIfRequired(() => textBox1.Text = "updated");
    }

Sachez que Control.BeginInvoke tel qu'utilisé dans cet exemple est asynchrone, ce qui signifie que le code venant après un appel à Control.BeginInvoke peut être exécuté immédiatement après, que le délégué passé ait été exécuté ou non.

Si vous devez être sûr que textBox1 est mis à jour avant de continuer, utilisez plutôt Control.Invoke, ce qui bloquera le thread appelant jusqu'à ce que votre délégué soit exécuté. Notez que cette approche peut ralentir considérablement votre code si vous effectuez de nombreux appels d'invocation et notez qu'elle bloquera votre application si votre thread GUI attend que le thread appelant termine ou libère une ressource bloquée.

