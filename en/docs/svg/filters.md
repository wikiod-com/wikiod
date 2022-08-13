---
title: "Filters"
slug: "filters"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## Syntax
 - Filter declaration: `<filter id="filter-id"`> ...list of child
   primitives ... `</filter>` 
 - Apply filter via SVG attribute:
   `<elementname filter="url(#filter-id)" ... />` 
 - Apply filter via CSS
   property: *(-prefix-*)filter: url("#filter-id");

## Parameters
Element attributes | Details                                                           |
-------------------|-------------------------------------------------------------------|
Filter region      |The filter element may optionally define the position, dimensions, resolution, and units for the output of a filter effect. The position and dimensions of a filter may be specified using the following parameters: x, y, width, height. The default values are *not intuitive* and are: x: -10% y: -10% width: 120% height: 120%           |
Filter resolution  |The `filterRes` attribute is an optional attribute in SVG 1.1 that can be used to specify the resolution at which the filter is processed. This attribute had uneven support and is now deprecated in current browsers.           |      
Filter units|By default, the units and coordinate system for the filter effects region (x,y,width,height) of a filter element are set relative to the bounding box of the element referencing the filter. In SVG terms, this is called the "objectBoundingBox". When we write x="50%" it means "set the starting x position of the filter region at the left side of the bounding box of the referencing element + 50% of the width of the element". But you may also specify the units and coordinates explicitly by setting the filterUnits property. The two alternatives are "objectBoundingBox" (the default which we're just described) or "userSpaceOnUse". userSpaceOnUse sets the filter units and the coordinate system to the canvas of the referencing element, or in SVG terms, the "userSpaceOnUse".    |      
Primitive Units  |In addition to the unit system for the filter itself, you may also specify the unit system that the filter's child filter primitives will use via the primitiveUnits attribute. Once again, the choice is between userSpaceOnUse and objectBoundingBox. These affect the 0,0 coordinates and the unit values for the filter primitives in the same way as for filterUnits. |  
Color Space |The default color space for SVG filters is linearRGB. The optional `color-interpolation-filters` attribute can be set to `sRGB` to change the color space to the more conventional sRGB space. |
                                                 


Most filter attributes are animateable via the `<animate>` element, although you must use the "fakeSMIL" library on IE to achieve the same results. SMIL animation (the `<animate>` element) will be deprecated in favor of the new Web Animations spec - which has very limited support as of mid 2016.

Child elements of the Filter element - filter primitives - have two optional attributes that specify the color space within which color interpolation calculations are performed: color-interpolation and color-interpolation-filters. The default for the former is sRGB, and the default for the latter is linearRGB. Manipulations that invert the color space (through feColorMatrix or feComponentTransfer) can result in non-intuitive results - for those used to the CSS sRGB color space. For example, a color inversion of a greyscale image in linearRGB will result in a pronounced shift toward whiter tones. This can be corrected by setting the value of the primitive to sRGB. These attributes can be set on the individual filter primitives or inherited from the filter element itself.

If no other input is specified, but one is required, the first filter primitive within a filter will take a rasterized (bitmapped) version of the referring element as its input. Subsequent filter primitives that expect an input will take the result of the immediately preceding filter primitive as input.

In complex filters, it can become difficult to keep track (and debug) inputs and outputs if they are left implicit; and it is good practice to explicitly declare inputs and outputs for each primitive.


----------


**SVG filter primitives can be colloquially divided into inputs, transformations, lighting effects and combinations.**


**Inputs:**

feFlood: generates a color field

feTurbulence: generates a wide variety of noise effects

feImage: generates an image from an external image reference, data URI or object 
reference (object references are not supported in Firefox as of mid Dec '12)

**Transformations:**

feColorMatrix: transforms the input values of an RBGA pixel into output values

feComponentTransfer: adjusts the color curve of an individual color channel

feConvolveMatrix: replaces each pixel with a new pixel calculated from pixel values in an area relative to the current pixel)

feGaussianBlur: replaces the current pixel with a weighted average of pixels in an area around the pixel

feDisplacementMap: moves each pixel from its current position based on the R,G or B values from another input graphic.

feMorphology: replaces each pixel with a new pixel calculated from the maximum or minimum value of all pixels in a rectangular area around that pixel.

feOffset: moves the input from its current position

**Lighting Effects:**

feSpecularLighting: provides a "shiny" 2D or pseudo-3D lighting effect

feDiffuseLighting: provides a "matte" 2D or pseudo-3D lighting effect

feDistantLight: provides a distant light source for specular or diffuse lighting

feSpotLight: provides a conic section light source for specular or diffuse lighting

fePointLight: provides a point light source for specular or diffuse lighting

**Combinations:**

feMerge: creates a simple over composite from multiple inputs (including previous filter inputs)

feBlend: blends multiple inputs using mixing rules

feComposite: combines multiple inputs using set combination rules, taking into account alpha values.

feTile: tiles input to create a repeating pattern


----------
**Other Notes**

Although SVG is a vector graphics technology, it is important to emphasize that SVG Filters perform *pixel-level* operations on all inputs (including SVG shapes) and produce rasterized (bitmapped) outputs at a specified level of resolution. Applying a 10x scale transform (for example) on an plain SVG curve that has been filtered at normal screen resolution will produce pixelated edges since the anti-aliasing of the original graphic has been converted to pixels by the filter and scaled up. (It is unclear whether this is spec compliant or just a limitation of current implementations)

Remember that SVG is XML when you write filters, so all tags must be closed and many properties and attributes are required to be specified explicitly or the filter will not execute.

A filter element is never rendered directly. It is only referenced using the filter property on the element to which the filter is applied. Note that the display property does not apply to the filter element and elements are not directly rendered even if the display property is set to a value other than "none". Conversely, filter elements are available for referencing even when thedisplay property on the filterelement or any of its ancestors is set to "none".

SVG filters can be referenced via a CSS Filter, although as of mid 2016, only a subset of primitives are supported via this mechanism, and this mechanism is not supported in Microsoft browsers.

## Blur Filters: feGaussian Blur (basic)
    <svg width="900px" height="400px" viewBox="0 0 900 400">
       <defs>
          <filter id="basicGaussian">
             <feGaussianBlur stdDeviation="5"/>
          </filter>
       </defs>
            
       <image xlink:href="https://upload.wikimedia.org/wikipedia/commons/a/af/Fruit_Stall_in_Barcelona_Market.jpg" x="20px" y="20px" width="300px" height="200px" preserveAspectRatio="xMinYMin meet" />
       <image filter="url(#basicGaussian)" xlink:href="https://upload.wikimedia.org/wikipedia/commons/a/af/Fruit_Stall_in_Barcelona_Market.jpg" x="340px" y="20px" width="300px" height="200px" preserveAspectRatio="xMinYMin meet"/>
    </svg>

[![enter image description here][1]][1]
<br><sup>([Source image](https://commons.wikimedia.org/wiki/File:Fruit_Stall_in_Barcelona_Market.jpg) by *Daderot* at Wikimedia Commons)</sup>


  [1]: https://i.stack.imgur.com/nuMLO.png

## Blur Filters: feGaussianBlur (x-axis and y-axis blur set separately)


    <svg width="900px" height="400px" viewBox="0 0 900 400">
       <defs>
          <filter id="xAxisGaussian">
             <feGaussianBlur stdDeviation="5 0"/>
          </filter>
       </defs>
            
       <image xlink:href="https://upload.wikimedia.org/wikipedia/commons/a/af/Fruit_Stall_in_Barcelona_Market.jpg" x="20px" y="20px" width="300px" height="200px" preserveAspectRatio="xMinYMin meet" />
       <image filter="url(#xAxisGaussian)" xlink:href="https://upload.wikimedia.org/wikipedia/commons/a/af/Fruit_Stall_in_Barcelona_Market.jpg" x="340px" y="20px" width="300px" height="200px" preserveAspectRatio="xMinYMin meet"/>
    </svg>

[![enter image description here][1]][1]
<br><sup>([Source image](https://commons.wikimedia.org/wiki/File:Fruit_Stall_in_Barcelona_Market.jpg) by *Daderot* at Wikimedia Commons)</sup>


  [1]: https://i.stack.imgur.com/OgT37.png

## Blur Filters: feGaussianBlur with hard edges & 100% Opacity
    <svg width="900px" height="400px" viewBox="900 400">
       <defs>
          <filter id="GaussianHardEdge" x="0%" y="0%" width="100%" height="100%">
             <feGaussianBlur stdDeviation="5"/>
             <feComponentTransfer>
               <feFuncA type="table" tableValues="1 1"/>
             </feComponentTransfer>
          </filter>
       </defs>
            
       <image xlink:href="https://upload.wikimedia.org/wikipedia/commons/a/af/Fruit_Stall_in_Barcelona_Market.jpg" x="20px" y="20px" width="300px" height="200px" preserveAspectRatio="xMinYMin meet" />
       <image filter="url(#GaussianHardEdge)" xlink:href="https://upload.wikimedia.org/wikipedia/commons/a/af/Fruit_Stall_in_Barcelona_Market.jpg" x="340px" y="20px" width="300px" height="200px" preserveAspectRatio="xMinYMin meet"/>
    </svg>

[![enter image description here][1]][1]
<br><sup>([Source image](https://commons.wikimedia.org/wiki/File:Fruit_Stall_in_Barcelona_Market.jpg) by *Daderot* at Wikimedia Commons)</sup>


  [1]: https://i.stack.imgur.com/5jgJH.png

## Blur Filters: Box Blur
    <svg width="900px" height="400px" viewBox="900 400">
       <defs>
          <filter id="GaussianHardEdge" >
             <feConvolveMatrix order="3" kernelMatrix=" 1 1 1
                                                        1 1 1
                                                        1 1 1"/>
          </filter>
       </defs>
            
       <image xlink:href="https://upload.wikimedia.org/wikipedia/commons/a/af/Fruit_Stall_in_Barcelona_Market.jpg" x="20px" y="20px" width="300px" height="200px" preserveAspectRatio="xMinYMin meet" />
       <image filter="url(#GaussianHardEdge)" xlink:href="https://upload.wikimedia.org/wikipedia/commons/a/af/Fruit_Stall_in_Barcelona_Market.jpg" x="340px" y="20px" width="300px" height="200px" preserveAspectRatio="xMinYMin meet"/>
    </svg>

[![enter image description here][1]][1]
<br><sup>([Source image](https://commons.wikimedia.org/wiki/File:Fruit_Stall_in_Barcelona_Market.jpg) by *Daderot* at Wikimedia Commons)</sup>


  [1]: https://i.stack.imgur.com/K9bJB.png

## Blur Filters: Bokeh Blur (3 layers, clipped)
    <svg width="900px" height="400px" viewBox="0 0 900 400">
       <defs>
    <filter id="BokehBlur" color-interpolation-filters="sRGB">
          <feGaussianBlur stdDeviation="2" result="blurSource"/>
          <feColorMatrix type="luminanceToAlpha"/>
          <feComponentTransfer result="brightness-mask" >
            <feFuncA type="discrete" tableValues="0 0 0 1 1"/>
            </feComponentTransfer>
    
          
         <!--bokeh Layer 1 -->
         <feTurbulence type="fractalNoise" seed="1" baseFrequency=".67" numOctaves="3"/>
        <feColorMatrix type="luminanceToAlpha"/>
          <feComponentTransfer>
            <feFuncA type="discrete" tableValues="0 0 0 1"/>
         </feComponentTransfer>
          <feComposite operator="in" in="brightness-mask"/>
          <feComposite operator="in" in="blurSource"/>
    
          <feMorphology operator="dilate" radius="5"/>
          <feGaussianBlur stdDeviation="8"/>
          <feColorMatrix type="matrix" values="1 0 0 0 0  0 1 0 0 0  0 0 1 0 0 
                                               0 0 0 9 0" /> 
          <feComponentTransfer result="bokeh1">
            <feFuncA type="linear" slope=".5" />
         </feComponentTransfer>  
          
          
          
          <!--bokeh Layer 2 -->
         <feTurbulence type="fractalNoise" seed="49" baseFrequency=".67" numOctaves="3"/>
        <feColorMatrix type="luminanceToAlpha"/>
          <feComponentTransfer>
            <feFuncA type="discrete" tableValues="0 0 0 1"/>
         </feComponentTransfer>
          <feComposite operator="in" in="brightness-mask"/>
          <feComposite operator="in" in="blurSource"/>
    
          <feMorphology operator="dilate" radius="10"/>
          <feGaussianBlur stdDeviation="12"/>
          <feColorMatrix type="matrix" values="1 0 0 0 0  0 1 0 0 0  0 0 1 0 0 
                                               0 0 0 15 0" /> 
          <feComponentTransfer result="bokeh2">
            <feFuncA type="linear" slope=".3" />
         </feComponentTransfer>  
          
        <!--bokeh Layer 3 -->
          
        <feTurbulence type="fractalNoise" seed="44" baseFrequency=".67" numOctaves="3"/>
        <feColorMatrix type="luminanceToAlpha"/>
          <feComponentTransfer>
            <feFuncA type="discrete" tableValues="0 0 0 1"/>
         </feComponentTransfer>
          <feComposite operator="in" in="brightness-mask"/>
          <feComposite operator="in" in="blurSource"/>
    
          <feMorphology operator="dilate" radius="10"/>
          <feGaussianBlur stdDeviation="18"/>
          <feColorMatrix type="matrix" values="1 0 0 0 0  0 1 0 0 0  0 0 1 0 0 
                                               0 0 0 15 0" /> 
          <feComponentTransfer result="bokeh3">
            <feFuncA type="linear" slope=".2" />
         </feComponentTransfer>  
          
        <!--Merge -->
         <feBlend mode="multiply" in="bokeh3" in2="bokeh2"/>
         <feBlend mode="lighten" in2="bokeh1"/>
          
          <feMorphology operator="erode" radius="0" result="bokeh"/>   
          <feGaussianBlur stdDeviation="9" in="SourceGraphic"/>
          <feComposite operator="over" in="bokeh"/> 
          <feComposite operator="in" in2="SourceGraphic"/>
    
        </filter>
       </defs>
            
       <image xlink:href="https://upload.wikimedia.org/wikipedia/commons/a/af/Fruit_Stall_in_Barcelona_Market.jpg" x="20px" y="20px" width="300px" height="200px" preserveAspectRatio="xMinYMin meet" />
       <image filter="url(#BokehBlur)" xlink:href="https://upload.wikimedia.org/wikipedia/commons/a/af/Fruit_Stall_in_Barcelona_Market.jpg" x="340px" y="20px" width="300px" height="200px" preserveAspectRatio="xMinYMin meet"/>
    </svg>

[![enter image description here][1]][1]
<br><sup>([Source image](https://commons.wikimedia.org/wiki/File:Fruit_Stall_in_Barcelona_Market.jpg) by *Daderot* at Wikimedia Commons)</sup>


  [1]: https://i.stack.imgur.com/PA7uE.png

## Shadow Filters: Basic Dropshadow
    <svg width="800px" height="600px">
    <defs>
      <filter id="drop-shadow">
        <feGaussianBlur in="SourceAlpha" stdDeviation="4"/>
        <feOffset dx="5" dy="5" result="offsetblur"/>
        <feFlood flood-color="red"/>
        <feComposite in2="offsetblur" operator="in"/>
        <feMerge>
          <feMergeNode/>
          <feMergeNode in="SourceGraphic"/>
      </feMerge>
    </filter> 
      </defs>
      
      <text filter="url(#drop-shadow)" x="30" y="100" font-size="80">SVG Filters</text>
      
    </svg>

## Shadow Filters: Inner Glow
    <svg width="800px" height="600px">
    <defs>
      <filter id="inner-glow">
        <feFlood flood-color="red"/>
        <feComposite in2="SourceAlpha" operator="out"/>
        <feGaussianBlur stdDeviation="2" result="blur"/>
        <feComposite operator="atop" in2="SourceGraphic"/>
    </filter> 
      </defs>
      
      <text filter="url(#inner-glow)" x="30" y="100" font-size="80" font-family="Sans-Serif" font-weight="bold">SVG Filters</text>
      
    </svg>

## Shadow Filters: Complex Dropshadow (Contoured, Noisy, Shaped)
    <svg width="800px" height="600px">
    <defs>
    <filter id="complex-shadow" color-interpolation-filters="sRGB" x="-50%" y="-50%" height="200%" width="200%">
    
    <!-- Take source alpha, offset it by angle/distance and blur it by size -->
    <feOffset id="offset" in="SourceAlpha" dx="11" dy="6" result="SA-offset"/> 
    <feGaussianBlur id="blur" in="SA-offset" stdDeviation="4" result="SA-o-blur"/>
    
    <!-- Apply a contour by using a color curve transform on the alpha and clipping the result to the input -->
    
    <feComponentTransfer in="SA-o-blur" result="SA-o-b-contIN"> 
      <feFuncA id="contour" type="table" tableValues="0 1 .3 .1 0.05 .1 .3 1 "/> 
    </feComponentTransfer>
    
    <feComposite operator="in" in="SA-o-blur" in2="SA-o-b-contIN" result="SA-o-b-cont"/>
    
    <!-- Adjust the spread by multiplying alpha by a constant factor --> <feComponentTransfer in="SA-o-b-cont" result="SA-o-b-c-sprd"> 
      <feFuncA id="spread-ctrl" type="linear" slope="2.8"/> 
    </feComponentTransfer>
    
    <!-- Adjust color and opacity by adding fixed offsets and an opacity multiplier --> 
    <feColorMatrix id="recolor" in="SA-o-b-c-sprd" type="matrix" values="0 0 0 0 0.945 0 0 0 0 0.137 0 0 0 0 0.137 0 0 0 0.49 0" result="SA-o-b-c-s-recolor"/>
    
    <!-- Generate a grainy noise input with baseFrequency between approx .5 to 2.0. And add the noise with k1 and k2 multipliers that sum to 1 --> 
    <feTurbulence result="fNoise" type="fractalNoise" numOctaves="6" baseFrequency="1.98"/> 
    <feColorMatrix in="fNoise" type="matrix" values="1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 7 -3" result="clipNoise"/> 
    <feComposite id="noisemix" operator="arithmetic" in="SA-o-b-c-s-recolor" in2="clipNoise" k1="0.67" k2="0.33" result="SA-o-b-c-s-r-mix"/>
    
    <!-- Merge the shadow with the original --> 
    <feMerge> 
      <feMergeNode in="SA-o-b-c-s-r-mix"/> 
      <feMergeNode in="SourceGraphic"/> 
    </feMerge> 
    </filter> 
      </defs>
      
      <text filter="url(#complex-shadow)" x="30" y="100" font-size="80" font-family="Sans-Serif" font-weight="bold">SVG Filters</text>
      
    </svg>

## Color Manipulation Filters: Basic Greyscale
    <svg width="800px" height="600px">
       <defs>
         <filter id="greyscale">
         <feColorMatrix type="matrix"
                 values="0.2126  0.7152  0.0722  0 0
                         0.2126  0.7152  0.0722  0 0
                         0.2126  0.7152  0.0722  0 0
                         0 0 0 1 0"/>
         </filter> 
       </defs>
            
       <image 

    xlink:href="https://upload.wikimedia.org/wikipedia/commons/a/af/Fruit_Stall_in_Barcelona_Market.jpg" x="20px" y="20px" width="300px" height="200px" preserveAspectRatio="xMinYMin meet" />
       <image filter="url(#greyscale)" xlink:href="https://upload.wikimedia.org/wikipedia/commons/a/af/Fruit_Stall_in_Barcelona_Market.jpg" x="340px" y="20px" width="300px" height="200px" preserveAspectRatio="xMinYMin meet"/>
    </svg>

[![enter image description here][1]][1]
<br><sup>([Source image](https://commons.wikimedia.org/wiki/File:Fruit_Stall_in_Barcelona_Market.jpg) by *Daderot* at Wikimedia Commons)</sup>


  [1]: https://i.stack.imgur.com/99c87.png

## Color Manipulation Filters: Greyscale (Green Channel Only)
    <svg width="800px" height="600px">
       <defs>
         <filter id="greyscale">
         <feColorMatrix type="matrix"
                 values="0  1  0  0 0
                         0  1  0  0 0
                         0  1  0  0 0
                         0 0 0 1 0"/>
         </filter> 
       </defs>
            
       <image xlink:href="https://upload.wikimedia.org/wikipedia/commons/a/af/Fruit_Stall_in_Barcelona_Market.jpg" x="20px" y="20px" width="300px" height="200px" preserveAspectRatio="xMinYMin meet" />
       <image filter="url(#greyscale)" xlink:href="https://upload.wikimedia.org/wikipedia/commons/a/af/Fruit_Stall_in_Barcelona_Market.jpg" x="340px" y="20px" width="300px" height="200px" preserveAspectRatio="xMinYMin meet"/>
    </svg>

[![enter image description here][1]][1]
<br><sup>([Source image](https://commons.wikimedia.org/wiki/File:Fruit_Stall_in_Barcelona_Market.jpg) by *Daderot* at Wikimedia Commons)</sup>


  [1]: https://i.stack.imgur.com/C3SqL.png

## Color Manipulation Filters: Monotone
    <svg width="800px" height="600px">
       <defs>
         <filter id="greyscale">
         <feColorMatrix type="matrix"
                 values=".2  .2  .2  0 0
                         .6  .6  .6  0 0
                         .2  .2  .2  0 0
                         0 0 0 1 0"/>
         </filter> 
       </defs>
            
       <image xlink:href="https://upload.wikimedia.org/wikipedia/commons/a/af/Fruit_Stall_in_Barcelona_Market.jpg" x="20px" y="20px" width="300px" height="200px" preserveAspectRatio="xMinYMin meet" />
       <image filter="url(#greyscale)" xlink:href="https://upload.wikimedia.org/wikipedia/commons/a/af/Fruit_Stall_in_Barcelona_Market.jpg" x="340px" y="20px" width="300px" height="200px" preserveAspectRatio="xMinYMin meet"/>
    </svg>

[![enter image description here][1]][1]
<br><sup>([Source image](https://commons.wikimedia.org/wiki/File:Fruit_Stall_in_Barcelona_Market.jpg) by *Daderot* at Wikimedia Commons)</sup>


  [1]: https://i.stack.imgur.com/v94EC.png

## Blur Filters: Focus Blur (Gaussian)
    <svg width="800px" height="600px">
       <defs>
      <filter id="focus-blur" >
        <feDiffuseLighting result = "diffOut" diffuseConstant = "1" lighting-color="white">
           <feSpotLight id="spotlight" x = "500" y = "100" z = "150" pointsAtX = "500" pointsAtY = "100" pointsAtZ = "0" specularExponent ="12" limitingConeAngle="70"/>
        </feDiffuseLighting>
    
         <feColorMatrix in="diffOut" result="alphaMap" type="luminanceToAlpha"/>
         <feComponentTransfer in="alphaMap" result="invertlight">
            <feFuncA type="table" tableValues="1 0 0"/>
         </feComponentTransfer>
              
         <feGaussianBlur in="invertlight" result="featherspot" stdDeviation="5"/>
         <feComposite operator="xor" result="infocus" in2="SourceGraphic" in="featherspot"/>
         <feGaussianBlur in="SourceGraphic" result="outfocus" stdDeviation="2"/>
         <feComposite operator="over" in="infocus" in2="outfocus"/> 
        <feComposite operator="in" in2="SourceGraphic"/>       
    </filter>
       </defs>
            
       <image xlink:href="https://upload.wikimedia.org/wikipedia/commons/a/af/Fruit_Stall_in_Barcelona_Market.jpg" x="20px" y="20px" width="300px" height="200px" preserveAspectRatio="xMinYMin meet" />
       <image filter="url(#focus-blur)" xlink:href="https://upload.wikimedia.org/wikipedia/commons/a/af/Fruit_Stall_in_Barcelona_Market.jpg" x="340px" y="20px" width="300px" height="200px" preserveAspectRatio="xMinYMin meet"/>
    </svg>

[![enter image description here][1]][1]
<br><sup>([Source image](https://commons.wikimedia.org/wiki/File:Fruit_Stall_in_Barcelona_Market.jpg) by *Daderot* at Wikimedia Commons)</sup>


  [1]: https://i.stack.imgur.com/pSI67.png

## Color Manipulation Filters: Posterizing 
    <svg width="800px" height="600px" >
       <defs>
         <filter id="posterize" color-interpolation-filters="sRGB">
         <feComponentTransfer>
           <feFuncR type="discrete" tableValues="0 0.25 0.75 1.0"/>
           <feFuncG type="discrete" tableValues="0 0.25 0.75 1.0"/>
           <feFuncB type="discrete" tableValues="0 0.25 0.75 1.0"/>
         </feComponentTransfer>
         </filter> 
       </defs>
            
       <image xlink:href="https://upload.wikimedia.org/wikipedia/commons/4/42/Andy_Warhol_1975.jpg" x="20px" y="20px" width="300px" height="600px" preserveAspectRatio="xMinYMin meet" />
       <image filter="url(#posterize)" xlink:href="https://upload.wikimedia.org/wikipedia/commons/4/42/Andy_Warhol_1975.jpg" x="340px" y="20px" width="300px" height="600px" preserveAspectRatio="xMinYMin meet"/>
    </svg>

## Blur Filters: Highlight Blur
This filter selects only the high luminance areas of a source graphic, blurs the contents and composites the blurred content on top of the original. 

    <svg width="800px" height="600px">
        <defs>
            <filter id="highlightblur" color-interpolation-filters="sRGB">
               <feColorMatrix type="luminanceToAlpha" in="SourceGraphic" result="lumMap"/>
               <feComponentTransfer in="lumMap" result="highlightMask">
                  <feFuncA type="discrete" tableValues="0 0 0 0 0 0 0 1"/>
               </feComponentTransfer>
               <feComposite operator="in" in="SourceGraphic" in2="highlightMask" result="highlights"/>
               <feGaussianBlur in="highlights" stdDeviation="3" result="highBlur"/>
               <feComposite operator="over" in="highBlur" in2="SourceGraphic" result="final"/>
            </filter>
         </defs>

        <image filter="url(#highlightblur)" x="0" y="-40" width="780" height="600" preserveAspectRatio="true" xlink:href="http://i554.photobucket.com/albums/jj424/allbowerpower/Christmas%202009/ChristmasTablesetting016b.jpg"
        />
    </svg>

