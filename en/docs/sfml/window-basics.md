---
title: "Window Basics"
slug: "window-basics"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

You have to use a `sf::RenderWindow` rather than a `sf::Window`, if you plan on drawing primitives provided by SFML, such as `sf::RectangleShape` or `sf::Sprite`.

## Creating an OpenGL window
Windows in SFML are represented by one of two classes:

* `sf::Window` is a generic window provided by the operating system including an OpenGL render context.
* `sf::RenderWindow` is a specialized version of `sf::Window` that also acts as a `sf::RenderTarget`, allowing SFML's primitives to be rendered to it.

The basic usage is the same in both cases.

<!-- language: lang-cpp -->
    #include <SFML/Window.hpp>

    int main(int argc, char *argv) {
        // Create and initialize a window object
        sf::Window window(sf::VideoMode(640, 480), "My SFML Window");

        // Repeat this as long as the window is open
        while (window.isOpen()) {
            // Handle window events ("event loop")
            sf::Event event;
            while (window.pollEvent(event)) {
                switch(event.type) {
                    case sf::Event::Closed: // User tries to close the window
                        window.close(); // Actually close the window
                        break;
                }
            }

            // Render logic would be placed here
            
            // Swap buffers and update the window
            window.display();
        }
        return 0;
    }

