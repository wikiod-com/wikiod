---
title: "Getting started with nsis"
slug: "getting-started-with-nsis"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
NSIS installer can be downloaded from http://nsis.sourceforge.net/Download.
An exe of 1.6 MB will be downloaded. You can install it using the wizard.
While installing there are options to install

    1. Full: Installs all the components
    2. Lite: Basic and only essential components from the UI
    3. Minimal: The NSIS core files only.
    4. Custom: It's up to us to install whichever components that we need.

## Hello World!
Code, to be saved in „helloworld.nsi“:

    Name "Hello World"
    OutFile "helloworld.exe"
    Section "Hello World"
    MessageBox MB_OK "Hello World!"
    SectionEnd

Compile it with:

    <Path to NSIS>\makensis.exe <Path to script>\helloworld.nsi


