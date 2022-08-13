---
title: "Clase SpeechRecognitionEngine para reconocer el habla"
slug: "clase-speechrecognitionengine-para-reconocer-el-habla"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

## Sintaxis
- Motor de reconocimiento de voz()
- SpeechRecognitionEngine.LoadGrammar (gramática gramatical)
- SpeechRecognitionEngine.SetInputToDefaultAudioDevice()
- SpeechRecognitionEngine.RecognizeAsync (modo RecognizeMode)
- Constructor de gramática()
- GrammarBuilder.Append (Opciones de opciones)
- Opciones (cadena de parámetros [] opciones)
- Gramática (constructor GrammarBuilder)

## Parámetros
| `LoadGrammar`: Parámetros | Detalles |
| --------- | ------- |  
| gramática | La gramática a cargar. Por ejemplo, un objeto `DictationGrammar` para permitir el dictado de texto libre. |
| **`RecognizeAsync`: Parámetros** | **Detalles** |
| modo | El `RecognizeMode` para el reconocimiento actual: `Single` para un solo reconocimiento, `Multiple` para permitir múltiples.
| **`GrammarBuilder.Append`: Parámetros** | **Detalles** |
| elecciones | Agrega algunas opciones al generador de gramática. Esto significa que, cuando el usuario ingresa el habla, el reconocedor puede seguir diferentes "ramas" de una gramática. |
| **Constructor `Choices`: Parámetros** | **Detalles** |
| elecciones | Una variedad de opciones para el constructor de gramática. Ver `GrammarBuilder.Append`. |
| **Constructor `Gramática`: Parámetro** | **Detalles** |
| constructor | El `GrammarBuilder` para construir una `Gramática`.

Para usar `SpeechRecognitionEngine`, su versión de Windows debe tener habilitado el reconocimiento de voz.

Debe agregar una referencia a `System.Speech.dll` antes de poder usar las clases de voz.

## Reconocimiento asincrónico del habla basado en un conjunto restringido de frases
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

## Reconocimiento asíncrono de voz para dictado de texto libre
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

