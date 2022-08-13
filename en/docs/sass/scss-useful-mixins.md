---
title: "Scss useful mixins"
slug: "scss-useful-mixins"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Pure css3 pointer arrows with outline border
**!!! Container should be positioned relatively or absolutely**

**$direction** - top, bottom, left, right

**$margin** - margin by the edge in **$direction**. For top and bottom direction - it's from left to right. For left and right - it's from top to bottom.

**$colors** - first is a border color, second - is a background color (maybe it's better to inherit background color from a parent)

**$arrowSide** - is a relative size of an arrow

**$isInset** - arrow is inside (true) or outside of it's container

Here is a working Plunker https://plnkr.co/edit/PRF9eLwmOg8OcUoGb22Y?p=preview

    %pointer-core {
        content: " ";
        position: absolute;
        border: solid transparent;
        z-index: 9999;
    }

    @mixin pointer($direction, $margin: 10px, $colors: (#999, $gray), $arrowSide: 8px, $isInset: false){
    
        $opposites: (
            top: bottom,
            bottom: top,
            left: right,
            right: left
        );
    
        $margin-direction: (
            top: left,
            bottom: left,
            left: top,
            right: top
        );
    
        &:before {
            @extend %pointer-core;
            border-width: $arrowSide;
    
            @if $isInset {
                border-#{$direction}-color: nth($colors, 1);
                #{$direction}: -1px;
            }
            @else
            {
                border-#{map-get($opposites, $direction)}-color: nth($colors, 1);
                #{map-get($opposites, $direction)}: 100%;
            }
    
            #{map-get($margin-direction, $direction)}: 0;
    
            margin-#{map-get($margin-direction, $direction)}: $margin - 1;
        }
    
        &:after {
            @extend %pointer-core;
            border-width: $arrowSide - 1;
    
            @if $isInset {
                border-#{$direction}-color: nth($colors, 2);
                #{$direction}: -1px;
            }
            @else
            {
                border-#{map-get($opposites, $direction)}-color: nth($colors, 2);
                #{map-get($opposites, $direction)}: 100%;
            }
    
            #{map-get($margin-direction, $direction)}: 0;
    
            margin-#{map-get($margin-direction, $direction)}: $margin;
        }
    }

## Tooltip pointer example
    $color-purple-bg: #AF6EC4;
    $color-purple-border: #5D0C66;
    
        
    $color-yellow-bg: #E8CB48;
    $color-yellow-border: #757526;

    .tooltip {
        position: relative;
        
        &--arrow-down {
            @include pointer('bottom', 30px, ($color-purple-border, $color-purple-bg), 15px);
        }
        
        &--arrow-right {
            @include pointer('right', 60px, ($color-yellow-border, $color-yellow-bg), 15px);
        }
    }

