---
title: "Life-cycle"
slug: "life-cycle"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Create ##
This method is called once when the Application is started. In this method resources should be loaded and variables should be initialized.

---
## Render ##
The method is called every frame, and is used to display whatever needs to be displayed. It is also used to update any variables/classes that may need to be updated, such as a camera.

---
## Dispose ##
This method is called when the application is destroyed, and is used to free any resources, for examples `Texture`s or the `SpriteBatch`. You will know that an object has to be disposed of if it implements the `Disposable` interface.

---
## Pause ##
This method is called when the application is paused. Usually when the application looses focus.

---
## Resume ##
This method is called when the application should be resumed. Usually when the application regains focus.

---
## Resize ##
This method is called when the application is resized. This method is normally used to resize a viewport.

## Main Game file
<!-- language: java -->
    class Tutorial extends Game {
        
        public ScreenNumberOne screenNumberOne;

        public void create(){
            screenNumberOne = new ScreenNumberOne(this);

            this.setScreen(screenNumberOne);
        }

        public void render() {
            super.render();
        }
    }
    

That is the basic file to allow you to make multiple screens. Notice that it extends `Game`.

