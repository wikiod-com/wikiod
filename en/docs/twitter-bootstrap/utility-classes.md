---
title: "Utility Classes"
slug: "utility-classes"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Generate .hidden-* classes for all breakpoints - SCSS
    // Mixin to generate hidden classes
    @mixin generate-hidden-classes {
      @each $bp in map-keys($grid-breakpoints) {
        .hidden-#{$bp} {
          @include media-breakpoint-only($bp) {
            display: none !important;
          }
        }
      }
    }

    // Call to the mixin
    @include generate-hidden-classes();

