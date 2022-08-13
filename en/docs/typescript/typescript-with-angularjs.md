---
title: "TypeScript with AngularJS"
slug: "typescript-with-angularjs"
draft: false
images: []
weight: 9908
type: docs
toc: true
---

## Parameters
| Name | Description |
| ------ | ------ |
| `controllerAs` | is an alias name, to which variables or functions can be assigned to. @see: https://docs.angularjs.org/guide/directive |
| `$inject` | Dependency Injection list, it is resolved by angular and passing as an argument to constuctor functions. |


While doing the directive in TypeScript, keep in mind, that power of this language of custom type and interfaces that you can create. This is extremely helpfull when developing huge applications. Code completion supported by many IDE will show you the possible value by corresponding type you are working with, so there is far more less that should be kept in mind (comparing to VanillaJS).

"Code Against Interfaces, Not Implementations"

## Directive
```TypeScript

interface IMyDirectiveController {
    // specify exposed controller methods and properties here
    getUrl(): string;
}

class MyDirectiveController implements IMyDirectiveController {

    // Inner injections, per each directive 
    public static $inject = ["$location", "toaster"];

    constructor(private $location: ng.ILocationService, private toaster: any) {
        // $location and toaster are now properties of the controller
    }

    public getUrl(): string {
        return this.$location.url(); // utilize $location to retrieve the URL
    }
}

/*
 * Outer injections, for run once controll. 
 * For example we have all templates in one value, and we wan't to use it.
 */
export function myDirective(templatesUrl: ITemplates): ng.IDirective {
    return {
        controller: MyDirectiveController,
        controllerAs: "vm",

        link: (scope: ng.IScope,
               element: ng.IAugmentedJQuery,
               attributes: ng.IAttributes,
               controller: IMyDirectiveController): void => {

            let url = controller.getUrl();
            element.text("Current URL: " + url);

        },

        replace: true,
        require: "ngModel",
        restrict: "A",
        templateUrl: templatesUrl.myDirective,
    };
}

myDirective.$inject = [
    Templates.prototype.slug,
];

// Using slug naming across the projects simplifies change of the directive name
myDirective.prototype.slug = "myDirective";

// You can place this in some bootstrap file, or have them at the same file
angular.module("myApp").
    directive(myDirective.prototype.slug, myDirective);

```

## Simple example
    export function myDirective($location: ng.ILocationService): ng.IDirective {
        return {

            link: (scope: ng.IScope,
                element: ng.IAugmentedJQuery,
                attributes: ng.IAttributes): void => {

                element.text("Current URL: " + $location.url());

            },

            replace: true,
            require: "ngModel",
            restrict: "A",
            templateUrl: templatesUrl.myDirective,
        };
    }

    // Using slug naming across the projects simplifies change of the directive name
    myDirective.prototype.slug = "myDirective";

    // You can place this in some bootstrap file, or have them at the same file
    angular.module("myApp").
        directive(myDirective.prototype.slug, [
            Templates.prototype.slug,
            myDirective
        ]);

## Component
For an easier transition to Angular 2, it's recommended to use `Component`, available since Angular 1.5.8

**myModule.ts**

    import { MyModuleComponent } from "./components/myModuleComponent";
    import { MyModuleService } from "./services/MyModuleService";
    
    angular
        .module("myModule", [])
        .component("myModuleComponent", new MyModuleComponent())
        .service("myModuleService", MyModuleService);


**components/myModuleComponent.ts**

    import IComponentOptions = angular.IComponentOptions;
    import IControllerConstructor = angular.IControllerConstructor;
    import Injectable = angular.Injectable;
    import { MyModuleController } from "../controller/MyModuleController";
    
    export class MyModuleComponent implements IComponentOptions {
        public templateUrl: string = "./app/myModule/templates/myComponentTemplate.html";
        public controller: Injectable<IControllerConstructor> = MyModuleController;
        public bindings: {[boundProperty: string]: string} = {};
    }

**templates/myModuleComponent.html**

    <div class="my-module-component">
        {{$ctrl.someContent}}
    </div>

**controller/MyModuleController.ts**

    import IController = angular.IController;
    import { MyModuleService } from "../services/MyModuleService";
    
    export class MyModuleController implements IController {
        public static readonly $inject: string[] = ["$element", "myModuleService"];
        public someContent: string = "Hello World";
    
        constructor($element: JQuery, private myModuleService: MyModuleService) {
            console.log("element", $element);
        }
    
        public doSomething(): void {
            // implementation..
        }
    }


**services/MyModuleService.ts**
    
    export class MyModuleService {
        public static readonly $inject: string[] = [];
    
        constructor() {
        }
    
        public doSomething(): void {
            // do something
        }
    }


**somewhere.html**

    <my-module-component></my-module-component>


