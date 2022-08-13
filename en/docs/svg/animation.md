---
title: "Animation"
slug: "animation"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

SMIL animation via the `<animate>` element is currently (July 2016) supported in major browsers with the exception of Microsoft browsers. There is a library (fakeSMIL) which can be used as a polyfill for Microsoft compatibility.

Chrome 45 deprecated SMIL in favor of CSS animations and forthcoming Web animations declarative animation syntax, which unfortunately, is only partially implemented in current browsers. But the Chrome developers recently suspended their intent (see [this StackOverflow answer][1])


  [1]: http://stackoverflow.com/questions/39146086/without-smil-is-gif-my-only-option/39180253#39180253

## <animate>
    <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
        <rect x="50" y="50" height="100" width="100" stroke="black" fill="yellow">
            <animate
                attributeType="XML"
                attributeName="height"
                begin="0s"
                dur="10s"
                from="100"
                to="200"
                repeatCount="indefinite"
            />
        </rect>
    </svg>

## <animateTransform>
    <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
        <rect x="50" y="50" height="100" width="100" stroke="black" fill="yellow">
            <animateTransform
                attributeType="XML"
                attributeName="transform"
                type="rotate"
                begin="0s"
                dur="10s"
                from="0"
                to="360"
                repeatCount="indefinite"
            />
        </rect>
    </svg>

