---
title: "Konuşmayı tanımak için SpeechRecognitionEngine sınıfı"
slug: "konusmay-tanmak-icin-speechrecognitionengine-snf"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

## Sözdizimi
- Konuşma TanımaMotoru()
- SpeechRecognitionEngine.LoadGrammar(Dilbilgisi dilbilgisi)
- SpeechRecognitionEngine.SetInputToDefaultAudioDevice()
- SpeechRecognitionEngine.RecognizeAsync(RecognizeMode modu)
- Dilbilgisi oluşturucu()
- GrammarBuilder.Append(Seçenekler seçenekleri)
- Seçenekler(params string[] seçimler)
- Dilbilgisi (GrammarBuilder oluşturucu)

## Parametreler
| `LoadGrammar`: Parametreler | Ayrıntılar |
| --------- | ------- |  
| gramer | Yüklenecek gramer. Örneğin, serbest metin diktesine izin veren bir "DictationGrammar" nesnesi. |
| **`RecognizeAsync`: Parametreler** | **Ayrıntılar** |
| modu | Geçerli tanıma için "TanımaModu": Yalnızca bir tanıma için "Tek", birden çok tanımaya izin vermek için "Çoklu".
| **`GrammarBuilder.Append`: Parametreler** | **Ayrıntılar** |
| seçimler | Dil bilgisi oluşturucuya bazı seçenekler ekler. Bu, kullanıcı konuşma girdiğinde, tanıyıcının bir dilbilgisinden farklı "dalları" takip edebileceği anlamına gelir. |
| **`Seçimler` yapıcısı: Parametreler** | **Ayrıntılar** |
| seçimler | Dilbilgisi oluşturucu için bir dizi seçenek. Bkz. `GrammarBuilder.Append`. |
| **`Grammar` yapıcısı: Parametre** | **Ayrıntılar** |
| inşaatçı | Bir "Dilbilgisi" oluşturmak için "GrammarBuilder".

`SpeechRecognitionEngine` kullanmak için Windows sürümünüzde konuşma tanımanın etkinleştirilmesi gerekir.

Konuşma sınıflarını kullanmadan önce `System.Speech.dll` dosyasına bir referans eklemelisiniz.

## Kısıtlı bir dizi ifadeye dayalı konuşmayı eşzamansız olarak tanıma
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

## Serbest metin dikte etmek için konuşmayı eşzamansız olarak tanıma
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

