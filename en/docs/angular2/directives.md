---
title: "Directives"
slug: "directives"
draft: false
images: []
weight: 9826
type: docs
toc: true
---

## Syntax
- `<input [value]="value">` - Binds attribute value class member `name`.

- `<div [attr.data-note]="note">` - Binds attribute `data-note` to variable `note`.

- `<p green></p>` - Custom directive

The main source of information about Angular 2 directives is the official documentation https://angular.io/docs/ts/latest/guide/attribute-directives.html

## *ngFor
form1.component.ts:

<!-- language: lang-ts -->

    import { Component } from '@angular/core';

    // Defines example component and associated template
    @Component({
        selector: 'example',
        template: `
          <div *ngFor="let f of fruit"> {{f}} </div>
          <select required>
            <option *ngFor="let f of fruit" [value]="f"> {{f}} </option>
          </select>
        `
    })

    // Create a class for all functions, objects, and variables
    export class ExampleComponent { 
        // Array of fruit to be iterated by *ngFor
        fruit = ['Apples', 'Oranges', 'Bananas', 'Limes', 'Lemons'];
    }

**Output:** 

    <div>Apples</div>
    <div>Oranges</div>
    <div>Bananas</div>
    <div>Limes</div>
    <div>Lemons</div>
    <select required>
      <option value="Apples">Apples</option>
      <option value="Oranges">Oranges</option>
      <option value="Bananas">Bananas</option>
      <option value="Limes">Limes</option>
      <option value="Lemons">Lemons</option>
    </select>

In its most simple form, `*ngFor` has two parts : <code>let **variableName** of **object/array**</code>

In the case of `fruit = ['Apples', 'Oranges', 'Bananas', 'Limes', 'Lemons'];`,

 Apples, Oranges, and so on are the values inside the array `fruit`.

 `[value]="f"` will be equal to each current `fruit` (`f`) that `*ngFor` has iterated over.

----

Unlike AngularJS, Angular2 has not continued with the use of `ng-options` for `<select>` and `ng-repeat` for all other general repetitions.

 `*ngFor` is very similar to `ng-repeat` with slightly varied syntax.

References:

Angular2 | [Displaying Data][1]

Angular2 | [ngFor][2]

Angular2 | [Forms][3]


  [1]: https://angular.io/docs/ts/latest/guide/displaying-data.html
  [2]: https://angular.io/docs/ts/latest/guide/template-syntax.html#!#ngFor
  [3]: https://angular.io/docs/ts/latest/guide/forms.html

## Attribute directive
<!-- language: lang-html -->

    <div [class.active]="isActive"></div>
    
    <span [style.color]="'red'"></span>

    <p [attr.data-note]="'This is value for data-note attribute'">A lot of text here</p>

## Component is a directive with template
<!-- language: lang-js -->

    import { Component } from '@angular/core';
    @Component({
      selector: 'my-app',
      template: `
        <h1>Angular 2 App</h1>
        <p>Component is directive with template</p>
      `
    })
    export class AppComponent {
    }

## Structural directives
<!-- language: lang-html -->

    <div *ngFor="let item of items">{{ item.description }}</div>

    <span *ngIf="isVisible"></span>

## Custom directive
<!-- language: lang-js -->

    import {Directive, ElementRef, Renderer} from '@angular/core';
    
    @Directive({
      selector: '[green]',
    })
    
    class GreenDirective {
      constructor(private _elementRef: ElementRef, 
                  private _renderer: Renderer) {
        _renderer.setElementStyle(_elementRef.nativeElement, 'color', 'green');
      }
    }

Usage:

<!-- language: lang-html -->

    <p green>A lot of green text here</p>

## Copy to Clipboard directive
In this example we are going to create a directive to copy a text into the clipboard by clicking on an element


*copy-text.directive.ts*
```javascript
import {
    Directive,
    Input,
    HostListener
} from "@angular/core";

@Directive({
    selector: '[text-copy]'
})
export class TextCopyDirective {

    // Parse attribute value into a 'text' variable
    @Input('text-copy') text:string;

    constructor() {
    }


    // The HostListener will listen to click events and run the below function, the HostListener supports other standard events such as mouseenter, mouseleave etc.
    @HostListener('click') copyText() {

        // We need to create a dummy textarea with the text to be copied in the DOM
        var textArea = document.createElement("textarea");

        // Hide the textarea from actually showing
        textArea.style.position = 'fixed';
        textArea.style.top = '-999px';
        textArea.style.left = '-999px';
        textArea.style.width = '2em';
        textArea.style.height = '2em';
        textArea.style.padding = '0';
        textArea.style.border = 'none';
        textArea.style.outline = 'none';
        textArea.style.boxShadow = 'none';
        textArea.style.background = 'transparent';

        // Set the texarea's content to our value defined in our [text-copy] attribute
        textArea.value = this.text;
        document.body.appendChild(textArea);

        // This will select the textarea
        textArea.select();

        try {
            // Most modern browsers support execCommand('copy'|'cut'|'paste'), if it doesn't it should throw an error
            var successful = document.execCommand('copy');
            var msg = successful ? 'successful' : 'unsuccessful';
            // Let the user know the text has been copied, e.g toast, alert etc.
            console.log(msg);
        } catch (err) {
            // Tell the user copying is not supported and give alternative, e.g alert window with the text to copy
            console.log('unable to copy');
        }

        // Finally we remove the textarea from the DOM
        document.body.removeChild(textArea);
    }
}

export const TEXT_COPY_DIRECTIVES = [TextCopyDirective];
```

*some-page.component.html*

Remember to inject TEXT_COPY_DIRECTIVES into the directives array of your component
```html
...
    <!-- Insert variable as the attribute's value, let textToBeCopied = 'http://facebook.com/' -->
    <button [text-copy]="textToBeCopied">Copy URL</button>
    <button [text-copy]="'https://www.google.com/'">Copy URL</button>
...
```

## Testing a custom directive
Given a directive that highlights text on mouse events

    import { Directive, ElementRef, HostListener, Input } from '@angular/core';
    
    @Directive({ selector: '[appHighlight]' })
    export class HighlightDirective {
      @Input('appHighlight') // tslint:disable-line no-input-rename
      highlightColor: string;
    
      constructor(private el: ElementRef) { }
    
      @HostListener('mouseenter')
      onMouseEnter() {
        this.highlight(this.highlightColor || 'red');
      }
    
      @HostListener('mouseleave')
      onMouseLeave() {
        this.highlight(null);
      }
    
      private highlight(color: string) {
        this.el.nativeElement.style.backgroundColor = color;
      }
    }

It can be tested like this

    import { ComponentFixture, ComponentFixtureAutoDetect, TestBed } from '@angular/core/testing';
    
    import { Component } from '@angular/core';
    import { HighlightDirective } from './highlight.directive';
    
    @Component({
      selector: 'app-test-container',
      template: `
        <div>
          <span id="red" appHighlight>red text</span>
          <span id="green" [appHighlight]="'green'">green text</span>
          <span id="no">no color</span>
        </div>
      `
    })
    class ContainerComponent { }
    
    const mouseEvents = {
      get enter() {
        const mouseenter = document.createEvent('MouseEvent');
        mouseenter.initEvent('mouseenter', true, true);
        return mouseenter;
      },
      get leave() {
        const mouseleave = document.createEvent('MouseEvent');
        mouseleave.initEvent('mouseleave', true, true);
        return mouseleave;
      },
    };
    
    describe('HighlightDirective', () => {
      let fixture: ComponentFixture<ContainerComponent>;
      let container: ContainerComponent;
      let element: HTMLElement;
    
      beforeEach(() => {
        TestBed.configureTestingModule({
          declarations: [ContainerComponent, HighlightDirective],
          providers: [
            { provide: ComponentFixtureAutoDetect, useValue: true },
          ],
        });
    
        fixture = TestBed.createComponent(ContainerComponent);
        // fixture.detectChanges(); // without the provider
        container = fixture.componentInstance;
        element = fixture.nativeElement;
      });
    
      it('should set background-color to empty when mouse leaves with directive without arguments', () => {
        const targetElement = <HTMLSpanElement>element.querySelector('#red');
    
        targetElement.dispatchEvent(mouseEvents.leave);
        expect(targetElement.style.backgroundColor).toEqual('');
      });
    
      it('should set background-color to empty when mouse leaves with directive with arguments', () => {
        const targetElement = <HTMLSpanElement>element.querySelector('#green');
    
        targetElement.dispatchEvent(mouseEvents.leave);
        expect(targetElement.style.backgroundColor).toEqual('');
      });
    
      it('should set background-color red with no args passed', () => {
        const targetElement = <HTMLSpanElement>element.querySelector('#red');
    
        targetElement.dispatchEvent(mouseEvents.enter);
        expect(targetElement.style.backgroundColor).toEqual('red');
      });
    
      it('should set background-color green when passing green parameter', () => {
        const targetElement = <HTMLSpanElement>element.querySelector('#green');
    
        targetElement.dispatchEvent(mouseEvents.enter);
        expect(targetElement.style.backgroundColor).toEqual('green');
      });
    });



