---
title: "Speech to Text"
slug: "speech-to-text"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

IBM Watson Speech to Text offers a variety of options for transcribing audio in various languages and formats:

- **WebSockets** – establish a persistent connection over the WebSocket protocol for continuous transcription

- **Sessionless** – transcribe audio without the overhead of establishing and maintaining a session

- **Sessions** – create long multi-turn exchanges with the service or establish multiple parallel conversations with a particular instance of the service

- **Asynchronous** – provides a non-blocking HTTP interface for transcribing audio. You can register a callback URL to be notified of job status and results, or you can poll the service to learn job status and retrieve results manually.

See the [Getting Started][1] topic to learn how to get started with Speech to Text and other Watson services. For more Speech to Text details and examples, see the [API reference][2] and the [documentation][3].


  [1]: https://www.wikiod.com/ibm-watson-cognitive/getting-started-with-ibm-watson-cognitive
  [2]: https://www.ibm.com/watson/developercloud/speech-to-text/api/v1/#introduction
  [3]: https://www.ibm.com/watson/developercloud/doc/speech-to-text/

## Recognizing an audio file using WebSockets in Java
Using the [`Java-SDK 3.0.1`](https://github.com/watson-developer-cloud/java-sdk/releases/tag/java-sdk-3.0.1)

<!-- language: java -->

    CountDownLatch lock = new CountDownLatch(1);
    
    SpeechToText service = new SpeechToText();
    service.setUsernameAndPassword("<username>", "<password>");
    
    FileInputStream audio = new FileInputStream("filename.wav");
    
    RecognizeOptions options = new RecognizeOptions.Builder()
        .continuous(true)
        .interimResults(true)
        .contentType(HttpMediaType.AUDIO_WAV)
        .build();
    
    service.recognizeUsingWebSocket(audio, options, new BaseRecognizeCallback() {
      @Override
      public void onTranscription(SpeechResults speechResults) {
        System.out.println(speechResults);
        if (speechResults.isFinal())
          lock.countDown();
      }
    });
    
    lock.await(1, TimeUnit.MINUTES);

## Transcribing an audio file using WebSockets (Node.js)
This example shows how to use the IBM Watson Speech to Text service to recognize the type of an audio file and produce a transcription of the spoken text in that file.

>This example requires [Speech to Text service credentials][1] and [Node.js][2] 


 1. Install the npm module for the [Watson Developer Cloud Node.js SDK][3]:

```
$ npm install watson-developer-cloud
```

 2. Create a JavaScript file (for example, *app.js*) and copy the following code into it. Make sure you enter the `username` and `password` for your Speech to Text service instance.

<!-- language: lang-js -->


```
var SpeechToTextV1 = require('watson-developer-cloud/speech-to-text/v1');
var fs = require('fs');

var speech_to_text = new SpeechToTextV1({
  username: 'INSERT YOUR USERNAME FOR THE SERVICE HERE',
  password: 'INSERT YOUR PASSWORD FOR THE SERVICE HERE',
  url: 'https://stream.watsonplatform.net/speech-to-text/api'
});

var params = {
  content_type: 'audio/flac'
};

// Create the stream,
var recognizeStream = speech_to_text.createRecognizeStream(params);

// pipe in some audio,
fs.createReadStream('0001.flac').pipe(recognizeStream);

// and pipe out the transcription.
recognizeStream.pipe(fs.createWriteStream('transcription.txt'));

// To get strings instead of Buffers from received `data` events:
recognizeStream.setEncoding('utf8');

// Listen for 'data' events for just the final text.
// Listen for 'results' events to get the raw JSON with interim results, timings, etc.   
['data', 'results', 'error', 'connection-close'].forEach(function(eventName) {
  recognizeStream.on(eventName, console.log.bind(console, eventName + ' event: '));
});
```

 3. Save the sample audio file *[0001.flac][4]* to the same directory. This example code is set up to process **FLAC** files, but you could modify the `params` section of the sample code to obtain transcriptions from audio files in other formats. Supported formats include **WAV** (type `audio/wav`), **OGG** (type `audio/ogg`) and others. See the [Speech to Text API reference][5] for a complete list.

 5. Run the application (use the name of the file that contains the example code)

```
$ node app.js
```

After running the application, you will find the transcribed text from your audio file in the file *transcription.txt* in the directory from which you ran the application.


  [1]: https://www.wikiod.com/ibm-watson-cognitive/getting-started-with-ibm-watson-cognitive#Getting API credentials
  [2]: https://nodejs.org/en/download/
  [3]: https://github.com/watson-developer-cloud/node-sdk
  [4]: https://github.com/watson-developer-cloud/doc-tutorial-downloads/blob/master/speech-to-text/0001.flac?raw=true
  [5]: http://www.ibm.com/watson/developercloud/speech-to-text/api/v1/#recognize_audio_websockets

