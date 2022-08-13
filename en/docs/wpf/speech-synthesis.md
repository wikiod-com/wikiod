---
title: "Speech Synthesis"
slug: "speech-synthesis"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

In the `System.Speech` assembly, Microsoft has added **Speech Synthesis**, the ability to transform text into spoken words.

## Syntax

 1. SpeechSynthesizer speechSynthesizerObject = new SpeechSynthesizer();
    
    speechSynthesizerObject.Speak("Text to Speak");

## Speech Synthesis Example - Hello World
    using System;
    using System.Speech.Synthesis;
    using System.Windows;
    
    namespace Stackoverflow.SpeechSynthesisExample
    {
            public partial class SpeechSynthesisSample : Window
            {
                    public SpeechSynthesisSample()
                    {
                            InitializeComponent();
                            SpeechSynthesizer speechSynthesizer = new SpeechSynthesizer();
                            speechSynthesizer.Speak("Hello, world!");
                    }
            }
    }

