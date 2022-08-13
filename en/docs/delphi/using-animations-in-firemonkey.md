---
title: "Using Animations in Firemonkey"
slug: "using-animations-in-firemonkey"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Rotating TRectangle
 1. Create blank Multi-Device (Firemonkey) application.
 2. Drop Rectangle on Form.
 3. In Object inspector window (F11) find RotationAngle click on drop
    down button, and select "Create New TFloatAnimation".
 4. Object inspector window is automatically switched to a newly added
    TFloatAnimation, you can also view it in Structure menu (Shift + Alt
    + F11).
 5. In Object inspector of TFloatAnimation fill duration with any number
    (in seconds). In our case lets take 1. Leave StartValue property as
    it is, and in StopValue type - 360 (Degrees, so it all goes round).
    Also lets turn Loop option on (this loops animation until you stop
    it from code).

 Now we have our animation set up. All is left is to turn it on: Drop two buttons on form, call first one "Start", second one - "Stop". in OnClick event of first button write:

    FloatAnimation1.Start;

OnClick of second button code:

    FloatAnimation1.Stop;

If you changed name of your TFloatAnimation - Also change it when calling Start and Stop.

Now run your project, click Start button and enjoy.

