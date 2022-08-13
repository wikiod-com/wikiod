---
title: "ng2-semantic-ui Transitions"
slug: "ng2-semantic-ui-transitions"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Syntax
- **@ViewChild('banner')** is the syntax for gaining access to the DOM element that should be transitioning. 
- **varname: SuiTransition** is the syntax
- **new SuiTransition(ElementRef, Renderer)** is the syntax for creating a transition. The `ElementRef` is obtained with `@ViewChild()` and the `Renderer` is obtained through dependency injection. It is a core Angular component.
- **transition.animate(ISuiAnimation)** is the syntax for animating a transition. 

## Parameters
| Parameter | Details |
| ------ | ------ |
| name   | type of transition to do   |
| duration   | the length of time for the transition to execute   |

This uses `ElementRef` internally which the Angular team [says](https://angular.io/docs/js/latest/api/core/index/ElementRef-class.html) is a security risk.

## Simple Scale Transition animation
## TypeScript Angular 2 Component

    import {Component, OnInit, ViewChild, Renderer} from '@angular/core';
    import {SuiTransition} from "ng2-semantic-ui/components/transition/transition";
    
    @Component({
      selector: 'app-home',
      templateUrl: './home.component.html',
      styleUrls: ['./home.component.css']
    })
    export class HomeComponent implements OnInit {
      @ViewChild('banner') banner;
    
      myTransition: SuiTransition;
    
      constructor(private renderer: Renderer) { }
    
      ngOnInit() {
        this.myTransition = new SuiTransition(this.banner, this.renderer);
        this.myTransition.animate({name: "scale", duration: 1000});
      }
    
    }

## HTML Partial
    <div id="home">
    
      <div class="ui inverted masthead centered segment">
        <div class="ui page grid">
    
          <!--Should this be in a separate component? Maybe a banner component-->
          <div class="ui hidden information" suiTransition #banner>
            <h1 class="ui inverted centered header">
              Managing information has never been so fun!
            </h1>
            <p class="ui centered lead">
              My software makes it easy to keep track of records.
            </p>
            <a class="large basic inverted animated fade ui button">
              <div class="visible content">Use my software now!</div>
              <div class="hidden content">Go to my software</div>
            </a>
            <div class="ui centered image">
              <img src="../../assets/images/my_background.jpg" alt="My software">
            </div>
          </div>
        </div>
    
    
      </div>
    </div>

## CSS
    #home .masthead {
      background: rgb(24, 42, 115);
      background: -moz-linear-gradient(-45deg, rgba(24, 42, 115, 1) 0%, rgba(33, 138, 174, 1) 69%, rgba(32, 167, 172, 1) 89%);
      background: -webkit-gradient(linear, left top, right bottom, color-stop(0%, rgba(24, 42, 115, 1)), color-stop(69%, rgba(33, 138, 174, 1)), color-stop(89%, rgba(32, 167, 172, 1)));
      background: -webkit-linear-gradient(-45deg, rgba(24, 42, 115, 1) 0%, rgba(33, 138, 174, 1) 69%, rgba(32, 167, 172, 1) 89%);
      background: -o-linear-gradient(-45deg, rgba(24, 42, 115, 1) 0%, rgba(33, 138, 174, 1) 69%, rgba(32, 167, 172, 1) 89%);
      background: -ms-linear-gradient(-45deg, rgba(24, 42, 115, 1) 0%, rgba(33, 138, 174, 1) 69%, rgba(32, 167, 172, 1) 89%);
      background: linear-gradient(135deg, rgba(24, 42, 115, 1) 0%, rgba(33, 138, 174, 1) 69%, rgba(32, 167, 172, 1) 89%);
      filter: progid:DXImageTransform.Microsoft.gradient(startColorstr='#182a73', endColorstr='#20a7ac', GradientType=1);
      border-radius: 0;
      margin: 0em;
      padding: 1rem 0rem 0;
    }
    
    #home .masthead .column {
      position: relative;
    }
    
    #home .masthead .information {
      margin: 6em 1em 0 1em;
      text-align: center;
    }
    
    #home .masthead .information p {
      display: block;
      text-align: center;
      width: 100%;
      font-weight: 300;
      font-size: 20pt;
    }
    
    #home .masthead .information .button {
      margin: 40px auto 20px auto;
      display: block;
      width: 200px;
      border-radius: 500px;
    }
    
    #home p.ui.centered.lead {
      font-weight: 300;
      font-size: 16pt;
      padding: 0px 30px;
      line-height: 1.5;
      text-align: center;
      margin-bottom: 0.7em;
    }

