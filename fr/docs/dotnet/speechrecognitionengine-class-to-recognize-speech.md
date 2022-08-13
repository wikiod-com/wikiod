---
title: "Classe SpeechRecognitionEngine pour reconnaître la parole"
slug: "classe-speechrecognitionengine-pour-reconnaitre-la-parole"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

## Syntaxe
- Moteur de reconnaissance vocale()
- SpeechRecognitionEngine.LoadGrammar (grammaire grammaticale)
- SpeechRecognitionEngine.SetInputToDefaultAudioDevice()
- SpeechRecognitionEngine.RecognizeAsync (mode RecognizeMode)
- Constructeur de grammaire()
- GrammarBuilder.Append (choix de choix)
- Choix (params string[] choix)
- Grammaire (constructeur GrammarBuilder)

## Paramètres
| `LoadGrammar` : Paramètres | Détails |
| --------- | ------- |  
| grammaire | La grammaire à charger. Par exemple, un objet `DictationGrammar` pour permettre la dictée de texte libre. |
| **`RecognizeAsync` : Paramètres** | **Détails** |
| mode | Le `RecognizeMode` pour la reconnaissance actuelle : `Single` pour une seule reconnaissance, `Multiple` pour en autoriser plusieurs.
| **`GrammarBuilder.Append` : Paramètres** | **Détails** |
| choix | Ajoute quelques choix au générateur de grammaire. Cela signifie que, lorsque l'utilisateur saisit la parole, le module de reconnaissance peut suivre différentes "branches" d'une grammaire. |
| **Constructeur `Choices` : Paramètres** | **Détails** |
| choix | Un tableau de choix pour le constructeur de grammaire. Voir `GrammarBuilder.Append`. |
| **Constructeur `Grammar` : Paramètre** | **Détails** |
| constructeur | Le `GrammarBuilder` à partir duquel construire une `Grammar`.

Pour utiliser `SpeechRecognitionEngine`, votre version de Windows doit avoir la reconnaissance vocale activée.

Vous devez ajouter une référence à `System.Speech.dll` avant de pouvoir utiliser les classes vocales.

## Reconnaissance asynchrone de la parole basée sur un ensemble restreint de phrases
    SpeechRecognitionEngine recognitionEngine = new SpeechRecognitionEngine();
    GrammarBuilder builder = new GrammarBuilder();
    builder.Append(new Choices("I am", "You are", "He is", "She is", "We are", "They are"));
    builder.Append(new Choices("friendly", "unfriendly"));
    recognitionEngine.LoadGrammar(new Grammar(builder));
    recognitionEngine.SpeechRecognized += delegate(object sender, SpeechRecognizedEventArgs e)
    {
        Console.WriteLine("You said: {0}", e.Result.Text);
    };
    recognitionEngine.SetInputToDefaultAudioDevice();
    recognitionEngine.RecognizeAsync(RecognizeMode.Multiple);

## Reconnaissance asynchrone de la parole pour la dictée de texte libre
    using System.Speech.Recognition;

    // ...

    SpeechRecognitionEngine recognitionEngine = new SpeechRecognitionEngine();
    recognitionEngine.LoadGrammar(new DictationGrammar());
    recognitionEngine.SpeechRecognized += delegate(object sender, SpeechRecognizedEventArgs e)
    {
        Console.WriteLine("You said: {0}", e.Result.Text);
    };
    recognitionEngine.SetInputToDefaultAudioDevice();
    recognitionEngine.RecognizeAsync(RecognizeMode.Multiple);

