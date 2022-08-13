---
title: "Scripting"
slug: "scripting"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Scripting SVG using the native DOM interfaces is currently (2016) in a state of slight change. The current SVG standard (1.1) is implemented well by most major web browsers. However, as the SVG 2.0 standard is under development, some browsers have begun to remove SVG 1.1 features that will be obsolete in 2.0. You can see a full list of proposed changes from SVG 1.1 to SVG 2.0 in [Appendix L of SVG 2.0](https://www.w3.org/TR/SVG2/changes.html).

## Replacing `pathSegList` and other `SVGPathSeg` usage

In SVG 1.1 `<path>` elements are defined to have a [`pathSegList`](https://www.w3.org/TR/SVG11/paths.html#__svg__SVGAnimatedPathData__pathSegList) property that gives access to a native representation of all [path commands](https://www.w3.org/TR/SVG11/paths.html#PathData). Google Chrome v48 [removed this property](https://bugs.chromium.org/p/chromium/issues/detail?id=539385) at the end of 2015, in preparation for a [proposed replacement in SVG 2.0](https://svgwg.org/specs/paths/#InterfaceSVGPathData). Until SVG 2.0 support is added, you must use a polyfill to either [get the 1.1 functionality back](https://github.com/progers/pathseg), or to [implement the proposed 2.0 API](https://github.com/jarek-foksa/path-data-polyfill.js).

## Replacing `getTransformToElement()`

Chrome v48 also [removed](https://www.chromestatus.com/feature/5736166087196672) the [`SVGGraphicsElement.getTransformToElement()`](https://www.w3.org/TR/SVG/types.html#__svg__SVGLocatable__getTransformToElement) method. A simple polyfill exists to [implement the old method](http://jointjs.com/blog/get-transform-to-element-polyfill.html).

## Creating an Element
The simplest way to understand creating and modifying SVG elements is to operate on the elements using the [DOM Level 2 Core](https://www.w3.org/TR/DOM-Level-2-Core/) interfaces, as you would with HTML or XML.
 
It is imperative that the _elements_ created from JavaScript are created in the same namespace declared in the SVG element - in this example: "http://www.w3.org/2000/svg". However, almost all _attributes_ of SVG elements are not in any namespace. You must **not** place them in the SVG namespace.

Here we demonstrate SVG hosted inside of HTML, as this is a common case:

    <!doctype HTML>
    <html><title>Creating an Element</title>
    <body>
      <svg xmlns="http://www.w3.org/2000/svg"
           width="100%" height="100%"
           viewBox="0 0 400 300"></svg>

      <script>
         var svgNS = "http://www.w3.org/2000/svg";

         // Create a circle element (not part of the DOM yet)
         var circle = document.createElementNS(svgNS,'circle'); // Creates a <circle/>
         circle.setAttribute('fill','red'); // Note: NOT setAttributeNS()
         circle.setAttribute('cx',150);     // setAttribute turns 150 into a string
         circle.setAttribute('cy','80');    // using a string works, too
         circle.setAttribute('r',35);       // give the circle a radius so we can see it

         // Now, add the circle to the SVG document so we can see it
         var svg = document.querySelector('svg'); // the root <svg> element
         svg.appendChild(circle);
      </script>
    </body></html>

There are a few attributes that need to be created in a particular namespace. They are the ones listed with colons in their names in the [SVG Attribute Index](https://www.w3.org/TR/SVG/attindex.html). Specifically, they are: `xlink:actuate`, `xlink:arcrole`, `xlink:href`, `xlink:role`, `xlink:show`, `xlink:title`, `xlink:type`, `xml:base`, `xml:lang`, and `xml:space`. Set these attributes using `setAttributeNS()`:

    var svgNS   = "http://www.w3.org/2000/svg";
    var xlinkNS = "http://www.w3.org/1999/xlink";
    var img = document.createElementNS( svgNS, 'image' );
    img.setAttributeNS( xlinkNS, 'href', 'my.png' );

If you are creating elements often, particularly with many attributes, a helper function like the following can save you typing, avoid mistakes, and make your code easier to read:

    <!doctype HTML>
    <html><title>Creating an Element</title>
    <body>
      <svg xmlns="http://www.w3.org/2000/svg"></svg>
      <script>
         var svg = document.querySelector('svg');
         var circle = createOn( svg, 'circle', {fill:'red', cx:150, cy:80, r:35} );

        // Create an SVG element on another node with a set of attributes.
        // Attributes with colons in the name (e.g. 'xlink:href') will automatically
        // find the appropriate namespace URI from the SVG element.
        // Optionally specify text to create as a child node, for example
        //   createOn(someGroup,'text',{x:100,'text-anchor':'middle'},"Hello World!");
        function createOn(parentEl,name,attrs,text){
          var doc=parentEl.ownerDocument, svg=parentEl;
          while (svg && svg.tagName!='svg') svg=svg.parentNode;
          var el = doc.createElementNS(svg.namespaceURI,name);
          for (var a in attrs){
            if (!attrs.hasOwnProperty(a)) continue;
            var p = a.split(':');
            if (p[1]) el.setAttributeNS(svg.getAttribute('xmlns:'+p[0]),p[1],attrs[a]);
            else      el.setAttribute(a,attrs[a]);
          }
          if (text) el.appendChild(doc.createTextNode(text));
          return parentEl.appendChild(el);
        }
      </script>
    </body></html>


## Reading/Writing Attributes
You can use either [DOM Level 2 Core](https://www.w3.org/TR/DOM-Level-2-Core/) methods `getAttribute()`, `getAttributeNS()`, `setAttribute()`, and `setAttributeNS()` to read and write values from SVG elements, or you can use custom properties and methods specified in the [SVG 1.1 IDL](https://www.w3.org/TR/SVG/idl.html) (Interface Definition Language).

## Simple Numeric Attributes

For example, if you have an SVG circle element:

    <circle id="circ" cx="10" cy="20" r="15" />

you can either use DOM methods to read and write the attributes:

    var circ = document.querySelector('#circ');
    var x = circ.getAttribute('cx') * 1; // Use *1 to convert from string to number value
    circ.setAttribute('cy', 25);

...or you can use the custom `cx`, `cy`, and `r` properties defined for [`SVGCircleElement`](https://www.w3.org/TR/SVG/shapes.html#InterfaceSVGCircleElement). Note that these are not direct numbers, but instead—as with many accessors in SVG 1.1—they allow for access to animated values. These properties are of type  [`SVGAnimatedLength`](https://www.w3.org/TR/SVG/types.html#InterfaceSVGAnimatedLength). Disregarding animation and length units, you can use such attributes like:

    var x = circ.cx.baseVal.value; // this is a number, not a string
    circ.cy.baseVal.value = 25;

## Transformations

[SVG groups](https://www.w3.org/TR/SVG/struct.html#Groups) may be used to move, rotate, scale, and otherwise transform multiple graphical elements as a whole. (For details on SVG translations, see [Chapter 7](https://www.w3.org/TR/SVG/coords.html#TransformAttribute)). Here is a group that makes a smiley face that you can adjust the size, rotation, and placement of by adjusting the `transform`:

    <g id="smiley" transform="translate(120,120) scale(5) rotate(30)">
      <circle r="20" fill="yellow" stroke-width="2"/>
      <path fill="none" d="M-10,5 a 5 3 0 0 0 20,0" stroke-width="2"/>
      <circle cx="-6" cy="-5" r="2" fill="#000"/>
      <circle cx="6"  cy="-5" r="2" fill="#000"/>
    </g>

Using script to adjust the scale of this via DOM methods requires manipulating the entire `transform` attribute as a string:

    var face = document.querySelector('#smiley');

    // Find the full string value of the attribute
    var xform = face.getAttribute('transform');

    // Use a Regular Expression to replace the existing scale with 'scale(3)'
    xform = xform.replace( /scale\s*\([^)]+\)/, 'scale(3)' );

    // Set the attribute to the new string.
    face.setAttribute('transform',xform);

With the SVG DOM one can traverse the specific transforms in the list, find the desired one, and modify the values:

    var face = document.querySelector('#smiley');

    // Get the SVGTransformList, ignoring animation
    var xforms = face.transform.baseVal;          

    // Find the scale transform (pretending we don't know its index)
    for (var i=0; i<xforms.numberOfItems; ++i){
      // Get this part as an SVGTransform
      var xform = xforms.getItem(i);
      if (xform.type == SVGTransform.SVG_TRANSFORM_SCALE){
        // Set the scale; both X and Y scales are required
        xform.setScale(3,3);
        break;
      }
    }

* For traversing and manipulating the number of transforms, see [`SVGTransformList`](https://www.w3.org/TR/SVG/coords.html#InterfaceSVGTransformList).
* For manipulating individual transforms, see [`SVGTransform`](https://www.w3.org/TR/SVG/coords.html#InterfaceSVGTransform).



## Dragging SVG Elements
Using the mouse to drag an SVG element (or group of elements) can be accomplished by:

1. Adding `mousedown` handler to starts the drag: adding a translation on the element to use during dragging (if needed), tracking `mousemove` events, and adding a `mouseup` handler to end the drag.
2. During `mousemove`, transforming the position of the mouse from screen coordinates into the local coordinates for the object you are dragging, and update the translation accordingly.
3. During `mouseup`, removing the `mousemove` and `mouseup` handlers.

<!-- language: lang-js -->

    // Makes an element in an SVG document draggable.
    // Fires custom `dragstart`, `drag`, and `dragend` events on the
    // element with the `detail` property of the event carrying XY
    // coordinates for the location of the element.
    function makeDraggable(el){
      if (!el) return console.error('makeDraggable() needs an element');
      var svg = el;
      while (svg && svg.tagName!='svg') svg=svg.parentNode;
      if (!svg) return console.error(el,'must be inside an SVG wrapper');
      var pt=svg.createSVGPoint(), doc=svg.ownerDocument;

      var root = doc.rootElement || doc.body || svg;
      var xlate, txStartX, txStartY, mouseStart;
      var xforms = el.transform.baseVal;

      el.addEventListener('mousedown',startMove,false);

      function startMove(evt){
        // We listen for mousemove/up on the root-most
        // element in case the mouse is not over el.
        root.addEventListener('mousemove',handleMove,false);
        root.addEventListener('mouseup',  finishMove,false);

        // Ensure that the first transform is a translate()
        xlate = xforms.numberOfItems>0 && xforms.getItem(0);
        if (!xlate || xlate.type != SVGTransform.SVG_TRANSFORM_TRANSLATE){
          xlate = xforms.createSVGTransformFromMatrix( svg.createSVGMatrix() );
          xforms.insertItemBefore( xlate, 0 );
        }
        txStartX=xlate.matrix.e;
        txStartY=xlate.matrix.f;
        mouseStart = inElementSpace(evt);
        fireEvent('dragstart');
      }

      function handleMove(evt){
        var point = inElementSpace(evt);
        xlate.setTranslate(
          txStartX + point.x - mouseStart.x,
          txStartY + point.y - mouseStart.y
        );
        fireEvent('drag');
      }

      function finishMove(evt){
        root.removeEventListener('mousemove',handleMove,false);
        root.removeEventListener('mouseup',  finishMove,false);
        fireEvent('dragend');
      }

      function fireEvent(eventName){
        var event = new Event(eventName);
        event.detail = { x:xlate.matrix.e, y:xlate.matrix.f };
        return el.dispatchEvent(event);
      }

      // Convert mouse position from screen space to coordinates of el
      function inElementSpace(evt){
        pt.x=evt.clientX; pt.y=evt.clientY;
        return pt.matrixTransform(el.parentNode.getScreenCTM().inverse());
      }
    }

