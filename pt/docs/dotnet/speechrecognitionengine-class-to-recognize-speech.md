---
title: "Classe SpeechRecognitionEngine para reconhecer a fala"
slug: "classe-speechrecognitionengine-para-reconhecer-a-fala"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

## Sintaxe
- SpeechRecognitionEngine()
- SpeechRecognitionEngine.LoadGrammar(Gramática gramatical)
- SpeechRecognitionEngine.SetInputToDefaultAudioDevice()
- SpeechRecognitionEngine.RecognizeAsync (modo RecognizeMode)
- Construtor de gramática()
- GrammarBuilder.Append(Escolhas de opções)
- Choices(params string[] escolhas)
- Gramática (construtor GrammarBuilder)

## Parâmetros
| `LoadGrammar`: Parâmetros | Detalhes |
| --------- | ------- |  
| gramática | A gramática a ser carregada. Por exemplo, um objeto `DictationGrammar` para permitir o ditado de texto livre. |
| **`RecognizeAsync`: Parâmetros** | **Detalhes** |
| modo | O `RecognizeMode` para o reconhecimento atual: `Single` para apenas um reconhecimento, `Multiple` para permitir vários.
| **`GrammarBuilder.Append`: Parâmetros** | **Detalhes** |
| escolhas | Acrescenta algumas opções ao construtor de gramática. Isso significa que, quando o usuário insere a fala, o reconhecedor pode seguir diferentes "ramificações" de uma gramática. |
| **Construtor `Choices`: Parâmetros** | **Detalhes** |
| escolhas | Uma matriz de opções para o construtor de gramática. Veja `GrammarBuilder.Append`. |
| **Construtor `Grammar`: Parâmetro** | **Detalhes** |
| construtor | O `GrammarBuilder` para construir uma `Grammar`.

Para usar o `SpeechRecognitionEngine`, sua versão do Windows precisa ter o reconhecimento de fala ativado.

Você tem que adicionar uma referência a `System.Speech.dll` antes de poder usar as classes de fala.

## Reconhecimento de fala de forma assíncrona com base em um conjunto restrito de frases
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

## Reconhecimento de fala de forma assíncrona para ditado de texto livre
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

