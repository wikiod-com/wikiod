---
title: "Getting started with sfml"
slug: "getting-started-with-sfml"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Basic SFML program
If everything whas been set up correctly, the following snippet will show a window titled "SFML works!" with a green circle:

    #include <SFML/Graphics.hpp>

    int main()
    {
        sf::RenderWindow window(sf::VideoMode(200, 200), "SFML works!");
        sf::CircleShape shape(100.f);
        shape.setFillColor(sf::Color::Green);

        while (window.isOpen())
        {
            sf::Event event;
            while (window.pollEvent(event))
            {
                if (event.type == sf::Event::Closed)
                    window.close();
            }

            window.clear();
            window.draw(shape);
            window.display();
        }
    
        return 0;
    }
    


## Installation - Linux
There are different approaches to the installation of SFML on Linux:

 1. Install it directly from your distribution's package repository
 2. Get the source code, build it and install it
 3. Download the precompiled SDK and manually copy the files

Option 1 is the preferred one; if the version of SFML that you want to install is available in the official repository, then install it using your package manager. For example, on Debian you would do:

    sudo apt-get install libsfml-dev

Option 2 requires more work: you need to ensure all of SFML's dependencies including their development headers are available, make sure CMake is installed, and manually execute some commands. This will result in a package which is tailored to your system.
If you want to go this way, there's a [dedicated tutorial on building SFML yourself][1].

Finally, option 3 is a good choice for quick installation if SFML is not available as an official package. Download the SDK from the download page, unpack it and copy the files to your preferred location: either a separate path in your personal folder (like /home/me/sfml), or a standard path (like /usr/local).

If you already had an older version of SFML installed, make sure that it won't conflict with the new version!


  [1]: https://www.sfml-dev.org/tutorials/2.4/compile-with-cmake.php

## Installation - Windows
The most common way to install SFML on windows is to download the [official SDK][1]

You can then unpack the archive and use it in your environment of choice.

vcpkg
=====

Although it's still heavily in development, if you use Visual studio 2017 or newer, you can also install SFML via [vcpkg][2] which integrates with visual studio, greatly simplifying the installation process:

`vcpkg install sfml`




  [1]: https://www.sfml-dev.org/download.php
  [2]: https://github.com/Microsoft/vcpkg

## Insallation - macOS
First of all you need to download the SFML SDK. Then, in order to start developing SFML applications, you have to install the following items:

Header files and libraries
==========================

SFML is available either as dylibs or as frameworks. Only one type of binary is required although both can be installed simultaneously on the same system. We recommend using the frameworks.

Frameworks
----------

- Copy the content of Frameworks to /Library/Frameworks.

dylib
-----

- Copy the content of lib to /usr/local/lib and copy the content of include to /usr/local/include.

SFML dependencies
=================

SFML depends on a few external libraries on Mac OS X. Copy the content of extlibs to /Library/Frameworks.

Xcode templates
=====
If you use Xcode, installing the templates is strongly recommended. Copy the SFML directory from templates to /Library/Developer/Xcode/Templates (create the folders if they don't exist yet).

## Hello World in a SFML Window
Let's write a small program which will open a window, and write "Hello World" on the screen.

<!-- language: lang-cpp -->

    #include <SFML\Graphics.hpp>
    #include <cassert>
    
    int main() {
        sf::RenderWindow sfmlWin(sf::VideoMode(600, 360), "Hello World SFML Window");
        sf::Font font;
        //You need to pass the font file location
        if (!font.loadFromFile(/*
                               Put the filename that identify the font file you want to load*/"myfont.ttf")) {
            return -1;
        }
        sf::Text message("Hello, World !", font);
    
        while (sfmlWin.isOpen()) {
    
            sf::Event e;
            while (sfmlWin.pollEvent(e)) {
    
                switch (e.type) {
                case sf::Event::EventType::Closed:
                    sfmlWin.close();
                    break;
                }
            }
    
            sfmlWin.clear();
            sfmlWin.draw(message);
            sfmlWin.display();
        }
        return 0;
    }

Let's explain what we did there.

First, we created a `sf::Font` object. We need this object to store the font data that we will use to display the text. After that, we called the `loadFromFile` method, used to load the font in the memory. We should note that SFML don't know about your system fonts, so you need to provide a filename, not a font name

After that, we created a `sf::Text` object. We call a 3 parameter constructor taking :
 - The string you want to display
 - The font the object will use
 - The character size in pixel, which we did not pass here, so il will be set to the default value : 30

Since the `sf::Text` object is ready, we just need to draw it in the main sfml loop, by calling the draw method on the `sfmlWin` render window object that we created before

## Installation or Setup
 - First, download a copy of SFML from the [official page][1].


  [1]: http://www.sfml-dev.org/download/sfml/2.3.2/
 - Save it anywhere in your computer where it can be easily accessed.
 - Open Codeblocks.
 - Go to **Project->Build Options->LinkerSettings tab.**
 - Click on the **Add** button and go to the bin folder of SFML and select all the files present there.
 - Now go to the Search Directories tab and add the 'include' folder of SFML.
 - In the same tab, click the sub-tab Linker Settings and add the 'bin' folder.

