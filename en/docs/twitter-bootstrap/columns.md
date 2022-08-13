---
title: "Columns"
slug: "columns"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Responsive columns same height (CSS or SASS only)
You have to add a div with the class `.row-height` inside the row, and also add `.col-height` to the columns. If you want to restrict the effect to a certain media query, just use the responsive `.row-height` and `.col-height` classes: for example `.row-sm-height` with `.col-sm-height`.

CSS version:

    .row-height {
      display: table;
      table-layout: fixed;
      height: 100%;
      width: calc(100% + 30px);
    }
    .col-height {
      display: table-cell;
      float: none;
      height: 100%;
    }
    .col-top {
      vertical-align: top;
    }
    .col-middle {
      vertical-align: middle;
    }
    .col-bottom {
      vertical-align: bottom;
    }
    
    @media (min-width: 480px) {
      .row-xs-height {
        display: table;
        table-layout: fixed;
        height: 100%;
        width: 100%;
      }
      .col-xs-height {
        display: table-cell;
        float: none;
        height: 100%;
      }
      .col-xs-top {
        vertical-align: top;
      }
      .col-xs-middle {
        vertical-align: middle;
      }
      .col-xs-bottom {
        vertical-align: bottom;
      }
    }
    
    @media (min-width: 768px) {
      .row-sm-height {
        display: table;
        table-layout: fixed;
        height: 100%;
        width: 100%;
      }
      .col-sm-height {
        display: table-cell;
        float: none;
        height: 100%;
      }
      .col-sm-top {
        vertical-align: top;
      }
      .col-sm-middle {
        vertical-align: middle;
      }
      .col-sm-bottom {
        vertical-align: bottom;
      }
    }
    
    @media (min-width: 992px) {
      .row-md-height {
        display: table;
        table-layout: fixed;
        height: 100%;
        width: calc(100% + 30px);
      }
      .col-md-height {
        display: table-cell;
        float: none;
        height: 100%;
      }
      .col-md-top {
        vertical-align: top;
      }
      .col-md-middle {
        vertical-align: middle;
      }
      .col-md-bottom {
        vertical-align: bottom;
      }
      .row-md-height .col-md-3 {
        width: 25%;
        min-width: 25%; 
        max-width: 25%; 
      }
    }
    
    @media (min-width: 1200px) {
      .row-lg-height {
        display: table;
        table-layout: fixed;
        height: 100%;
        width: 100%;
      }
      .col-lg-height {
        display: table-cell;
        float: none;
        height: 100%;
      }
      .col-lg-top {
        vertical-align: top;
      }
      .col-lg-middle {
        vertical-align: middle;
      }
      .col-lg-bottom {
        vertical-align: bottom;
      }
    }

SASS version (needed bootstrap _variables.scss):

    @import "../bootstrap/variables.scss";
    $sizes: xs sm md lg;
    $screens: $screen-xs-min $screen-sm-min $screen-md-min $screen-lg-min;
    
    //general
    .row-height {
      display: table;
      table-layout: fixed;
      height: 100%;
      width: calc(100% + $grid-gutter-width);
    }
    .col-height {
      display: table-cell;
      float: none;
      height: 100%;
    }
    .col-top {
      vertical-align: top;
    }
    .col-middle {
      vertical-align: middle;
    }
    .col-bottom {
      vertical-align: bottom;
    }
    
    //different sizes
    @for $i from 1 through length($sizes) {
        $size: nth($sizes, $i);
        $screen: nth($screens, $i);
        
        @media (min-width: #{$screen}) {
          .row-#{$size}-height {
            display: table;
            table-layout: fixed;
            height: 100%;
            width: 100%;
          }
          .col-#{$size}-height {
            display: table-cell;
            float: none;
            height: 100%;
          }
          .col-#{$size}-top {
            vertical-align: top;
          }
          .col-#{$size}-middle {
            vertical-align: middle;
          }
          .col-#{$size}-bottom {
            vertical-align: bottom;
          }
        } 
     
    }



