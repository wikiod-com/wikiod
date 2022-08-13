---
title: "Créer sa propre MessageBox dans l'application Windows Form"
slug: "creer-sa-propre-messagebox-dans-lapplication-windows-form"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Nous devons d'abord savoir ce qu'est une MessageBox...

Le contrôle MessageBox affiche un message avec le texte spécifié et peut être personnalisé en spécifiant une image, un titre et des ensembles de boutons personnalisés (ces ensembles de boutons permettent à l'utilisateur de choisir plus qu'une simple réponse oui/non).

En créant notre propre MessageBox, nous pouvons réutiliser ce contrôle MessageBox dans toutes les nouvelles applications simplement en utilisant la dll générée ou en copiant le fichier contenant la classe.

## Syntaxe
- 'résultat DialogResult statique = DialogResult.No ; //DialogResult est renvoyé par les dialogues après le rejet.'

## Création de votre propre contrôle MessageBox.
Pour créer notre propre contrôle MessageBox, suivez simplement le guide ci-dessous...

1. Ouvrez votre instance de Visual Studio (VS 2008/2010/2012/2015/2017)

2. Accédez à la barre d'outils en haut et cliquez sur Fichier -> Nouveau projet --> Application Windows Forms --> Donnez un nom au projet, puis cliquez sur OK.
3. Une fois chargé, faites glisser et déposez un contrôle de bouton de la boîte à outils (située à gauche) sur le formulaire (comme illustré ci-dessous).

[![entrez la description de l'image ici][1]][1]


4. Double-cliquez sur le bouton et l'environnement de développement intégré générera automatiquement le gestionnaire d'événements de clic pour vous.

5. Modifiez le code du formulaire afin qu'il ressemble à ce qui suit (vous pouvez cliquer avec le bouton droit sur le formulaire et cliquer sur Modifier le code) :


    namespace MsgBoxExample {
        public partial class MsgBoxExampleForm : Form {
            //Constructor, called when the class is initialised.
            public MsgBoxExampleForm() {
                InitializeComponent();
            }

            //Called whenever the button is clicked.
            private void btnShowMessageBox_Click(object sender, EventArgs e) {
               CustomMsgBox.Show($"I'm a {nameof(CustomMsgBox)}!", "MSG", "OK");
            }
        }
    }

6. Explorateur de solutions -> Cliquez avec le bouton droit sur votre projet -> Ajouter -> Formulaire Windows et définissez le nom sur "CustomMsgBox.cs"

7. Faites glisser un contrôle de bouton et d'étiquette de la boîte à outils vers le formulaire (il ressemblera à quelque chose comme le formulaire ci-dessous après l'avoir fait) :


[![entrez la description de l'image ici][2]][2]

8. Écrivez maintenant le code ci-dessous dans le formulaire nouvellement créé :


    private DialogResult result = DialogResult.No;
    public static DialogResult Show(string text, string caption, string btnOkText) {
        var msgBox = new CustomMsgBox();
        msgBox.lblText.Text = text; //The text for the label...
        msgBox.Text = caption; //Title of form
        msgBox.btnOk.Text = btnOkText; //Text on the button
        //This method is blocking, and will only return once the user
        //clicks ok or closes the form.
        msgBox.ShowDialog(); 
        return result;
    }

    private void btnOk_Click(object sender, EventArgs e) {
        result = DialogResult.Yes;
        MsgBox.Close();
    }

9. Exécutez maintenant le programme en appuyant simplement sur la touche F5.
Félicitations, vous avez créé un contrôle réutilisable.

[1] : https://i.stack.imgur.com/aW1q1.jpg
[2] : https://i.stack.imgur.com/73c1M.jpg









## Comment utiliser son propre contrôle MessageBox créé dans une autre application Windows Form.
Pour trouver vos fichiers .cs existants, cliquez avec le bouton droit sur le projet dans votre instance de Visual Studio, puis cliquez sur Ouvrir le dossier dans l'Explorateur de fichiers.

1. Visual Studio --> Votre projet actuel (Windows Form) --> Explorateur de solutions --> Nom du projet --> Clic droit --> Ajouter --> Élément existant --> Localisez ensuite votre fichier .cs existant.

2. Il reste maintenant une dernière chose à faire pour utiliser la commande. Ajoutez une instruction using à votre code, afin que votre assembly connaisse ses dépendances.

       using System;
       using System.Collections.Generic;
       using System.ComponentModel;
       using System.Data;
       using System.Drawing;
       .
       .
       .
       using CustomMsgBox; //Here's the using statement for our dependency.

3. Pour afficher la boîte de message, utilisez simplement ce qui suit...

    CustomMsgBox.Show("Your Message for Message Box...","MSG","OK");

