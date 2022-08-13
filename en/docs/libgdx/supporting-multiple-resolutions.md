---
title: "Supporting Multiple Resolutions"
slug: "supporting-multiple-resolutions"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Viewports
To support multiple resolutions and aspect ratios Libgdx uses the so called `Viewports`.  
There are a few types of `Viewports` which use different strategies to handle multiple resolutions and aspect ratios.  
A `Viewport` uses a `Camera` under the hood and manages it's `viewportHeight` and `viewportWidth`. You can optionally give the `Viewport` a `Camera` in it's constructor, otherwise it will use an `OrthographicCamera` by default:

    private Viewport viewport;
    private Camera camera;

    public void create() {
       camera = new PerspectiveCamera();
       viewport = new FitViewport(8f, 4.8f, camera);
    }  

Additionally, you must specify the `worldWidth` and `worldHeight` to the viewport's constructor.  This space will represent the virtual coordinate system that you will use for specifying objects' position to be drawn and sizes.
The viewport transformation, that can be applied to a SpriteBatch, for example, will automatically take care of transforming the logical coordinates to actual screen coordinates, with a way that conforms to the actual type of viewport you are using.
For example, when using an orthographic projection (`OrthographicCamera`, which is the default): if your world size is 12.8 by 7.8 meters and your device screen is 2560x1560 pixels, then your world will be dynamically projected with 200 pixels per meter.

When the `Viewport`s size changes (for example if the Screen Orientation of the Smartphone is changing), you need to inform the `Viewport` about that change. it will then automatically update the `Camera`s `viewportHeight` and `viewportWidth`:  

    public void resize(int width, int height) {
        viewport.update(width, height);
    }

The `Viewport`s also manage the `OpenGL Viewport` and modify the drawable area as needed.  

The `Viewport`s also take care about converting screen-coordinates to game-coordinates, which is needed especially for picking:  

    private Vector2 worldPos = new Vector2();
    public boolean touchDown (int x, int y, int pointer, int button) {
        worldPos.set(x, y);
        viewport.unproject(worldPos);
        processPicking(worldPos);
    }

## Builtin Viewports
There are a couple of builtin viewports, that each do different things. Here is a list of the names, and their descriptions:

| Viewport Name | Description |
| ------ | ------ |
| StretchViewport | Will stretch the screen. No black bars, but aspect ratio might be off. |
| FitViewport | Will maximize it's size based on the aspect ratio. Could have black bars. |
| FillViewport | Quite the same as a FitVieport, but always fills the entire screen. |
| ScreenViewport | Always fills the entire screen, but does not resize any children. |
| ExtendedViewport | Keeps the world aspect ratio without black bars by extending the world in one direction |
| CustomViewport | A custom programmed Viewport. Might have black bars, and might keep aspect ratio. |

## Custom Viewports
You can make your very own custom viewport. It may have black bars, and it may or may not keep it's aspect ratio, depending on how you program it. A custom viewport would look something like this: <br />`public class Tutorial extends Viewport`. You would need to override `update(width, height)`, but that's it.

Another attempt would be to extend the generic `ScalingViewport`, and supplying another Scaling which is not already supplied. Suppose that you set it to `Scaling.none`. That would look something like this:
[![Image example][1]][1]
If you want some reference, you can find the builtin viewport classes right over [here][2].


  [1]: https://github.com/libgdx/libgdx/wiki/images/8F697TX.png
  [2]: https://github.com/libgdx/libgdx/tree/master/gdx/src/com/badlogic/gdx/utils/viewport

## StretchViewport
The `StretchViewport` is a `Viewport`type, which supports a virtual screen size.  
This allowes you to define a fixed (resolution independent) width and height.  
The `StretchViewport`, as the name suggests, stretches the virtual screen, if the virtual aspect ratio does not match the real aspect ratio. The OpenGL Viewport won't be modified and there won't appear black bars.

Usage:

    private Viewport viewport;
    private Camera camera;

    public void create() {
        camera = new OrthographicCamera();
        viewport = new StretchViewport(80, 48, camera);
    }

    public void resize(int width, int height) {
        viewport.update(width, height);
    }

## FitViewport
FitViewports are viewports that always maintain the aspect ratio. It does this by creating black bars on the edges where there is space left. This is one of the most commonly used viewports.

Usage:

    private Viewport viewport;
    private Camera camera;
    
    public void create() {
        camera = new OrthographicCamera();
        viewport = new FitViewport(80, 48, camera);
    }
    
    public void resize(int width, int height) {
        viewport.update(width, height);
    }

