---
title: "Getting started with dart"
slug: "getting-started-with-dart"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
The Dart SDK includes everything you need to write and run Dart code: VM, libraries, analyzer, package manager, doc generator, formatter, debugger, and more. If you are doing web development, you will also need Dartium.

Automated installation and updates
----------------------------------

 - [Installing Dart on Windows][1]
 - [Installing Dart on Mac][2]
 - [Installing Dart on Linux][3]

Manual install
----------------------------------
You can also [manually install any version of the SDK][4].


  [1]: https://www.dartlang.org/install/windows
  [2]: https://www.dartlang.org/install/mac
  [3]: https://www.dartlang.org/install/linux
  [4]: https://www.dartlang.org/install/archive

## Hello, World!
Create a new file named `hello_world.dart` with the following content:

    void main() {
      print('Hello, World!');
    }

In the terminal, navigate to the directory containing the file `hello_world.dart` and type the following:

    dart hello_world.dart

Hit <kbd>enter</kbd> to display `Hello, World!` in the terminal window.

## Http Request
## Html ##

    <img id="cats"></img>

## Dart ##

    import 'dart:html';
    
    /// Stores the image in [blob] in the [ImageElement] of the given [selector].
    void setImage(selector, blob) {
      FileReader reader = new FileReader();
      reader.onLoad.listen((fe) { 
        ImageElement image = document.querySelector(selector);
        image.src = reader.result;
      });
      reader.readAsDataUrl(blob);  
    }
    
    main() async {
      var url = "https://upload.wikimedia.org/wikipedia/commons/2/28/Tortoiseshell_she-cat.JPG";

      // Initiates a request and asynchronously waits for the result.
      var request = await HttpRequest.request(url, responseType: 'blob');
      var blob = request.response;
      setImage("#cats", blob);
    }

## Example ##

see Example on https://dartpad.dartlang.org/a0e092983f63a40b0b716989cac6969a

## Getters and Setters
    void main() {
      var cat = new Cat();
      
      print("Is cat hungry? ${cat.isHungry}");  // Is cat hungry? true
      print("Is cat cuddly? ${cat.isCuddly}");  // Is cat cuddly? false
      print("Feed cat.");
      cat.isHungry = false;                     
      print("Is cat hungry? ${cat.isHungry}");  // Is cat hungry? false
      print("Is cat cuddly? ${cat.isCuddly}");  // Is cat cuddly? true
    }

    class Cat {
      bool _isHungry = true;
      
      bool get isCuddly => !_isHungry;
      
      bool get isHungry => _isHungry;
      bool set isHungry(bool hungry) => this._isHungry = hungry;
    }

[Dart](https://www.dartlang.org) class getters and setters allow APIs to encapsulate object state changes.

See [dartpad](https://dartpad.dartlang.org) example here: https://dartpad.dartlang.org/c25af60ca18a192b84af6990f3313233

