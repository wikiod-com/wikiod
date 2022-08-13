---
title: "Props"
slug: "props"
draft: false
images: []
weight: 9727
type: docs
toc: true
---

camelCase <=> kebab-case
-

When defining the names of your `props`, always remember that HTML attribute names are case-insensitive. That means if you define a `prop` in camel case in your component definition...

    Vue.component('child', {
        props: ['myProp'],
        ...
    });
...you must call it in your HTML component as my-prop.

## Passing Data from parent to child with props
In Vue.js, every component instance has ***its own isolated scope***, which means that if a parent component has a child component - the child component has its own isolated scope and the parent component has its own isolated scope.  
  
For any medium to large size app, following best practices conventions prevents lots of headaches during the development phase and then after while maintenance. One of such things to follow is that ***avoid referencing/mutating parent data directly from the child component***. So then how do we reference the parent data from within a child component?  
  
Whatever parent data is required in a child component should be passed to the child as `props` from the parent.  
  
***Use Case***: Suppose we have a User database with two tables `users` and `addresses` with the following fields:  
  `users` Table  
  
  |name|phone|email|  
  |---- | ---- | --- |  
  |John Mclane|(1) 234 5678 9012| john@dirhard.com|  
  |James Bond| (44) 777 0007 0077|bond@mi6.com|
  
  
  `addresses` Table  
  
  | block| street|city|
| ------ | ------ | ----- |
| Nakatomi Towers| Broadway|New York|
| Mi6 House| Buckingham Road|London|  
  
and we want to have three components to display corresponding user information anywhere in our app  
  
**user-component.js**  

    export default{
        template:`<div class="user-component">
                    <label for="name" class="form-control">Name: </label>  
                    <input class="form-control input-sm" name="name" v-model="name">
                    <contact-details :phone="phone" :email="email"></contact-details>
                  </div>`,
        data(){
            return{
                name:'',
                phone:'',
                email:''
            }
        },
    }  
  
**contact-details.js**
    
    import Address from './address';
    export default{
        template:`<div class="contact-details-component>
                    <h4>Contact Details:</h4>
                    <label for="phone" class="form-control">Phone: </label>  
                    <input class="form-control input-sm" name="phone" v-model="phone">
                    <label for="email" class="form-control">Email: </label>  
                    <input class="form-control input-sm" name="email" v-model="email"> 
            
                    <h4>Address:</h4>
                    <address :address-type="addressType"></address>
                    //see camelCase vs kebab-case explanation below
                </div>`,
        props:['phone', 'email'],
        data:(){
            return:{
                addressType:'Office'
            }
        },
        components:{Address}  
    }  
  
**address.js**  
    
    export default{
        template:`<div class="address-component">
                    <h6>{{addressType}}</h6>
                    <label for="block" class="form-control">Block: </label>  
                    <input class="form-control input-sm" name="block" v-model="block">
                    <label for="street" class="form-control">Street: </label>  
                    <input class="form-control input-sm" name="street" v-model="street">
                    <label for="city" class="form-control">City: </label>  
                    <input class="form-control input-sm" name="city" v-model="city">
                 </div>`,
        props:{
            addressType:{
                required:true,
                type:String,
                default:'Office'
            },
        data(){
            return{
                block:'',
                street:'',
                city:''
            }
        }
    }  
  
**main.js**  
  
    import Vue from 'vue';  
      
    Vue.component('user-component', require'./user-component');  
    Vue.component('contact-details', require'./contact-details');  
  
    new Vue({
        el:'body'  
    });  
   
**index.html**  
    
    ...  
    <body>
        <user-component></user-component>
            ...
    </body>
  
We are displaying the `phone` and `email` data, which are properties of `user-component` in `contact-details` which doesn't have phone or email data.   
   
**Passing data as props**   
 
So within the `user-component.js` in the ***template*** property, where we include the `<contact-details>` component, we are passing the _**phone**_ and the _**email**_ data from `<user-component>`(parent component) to `<contact-details>`(child component) by dynamically binding it to the **props** - `:phone="phone"` and `:email="email` which is same as `v-bind:phone="phone"` and `v-bind:email="email"`  
  
**Props - Dynamic Binding**  
  
Since we are dynamically binding the props any change in ***phone*** or ***email*** within the parent component i.e. `<user-component>` will immediately be reflected in the child component i.e. `<contact-details>`.  
  
**Props - as Literals**  
  
However, if we would have passed the values of ***phone*** and ***email*** as string literal values like `phone="(44) 777 0007 0077" email="bond@mi6.com"` then it would not reflect any data changes which happen in the parent component.  
  
**One-Way binding**    

By default the direction of changes is top to bottom i.e. any change to dynamically bound props in the parent component will propagate to the child component but any change to the prop values in a child component will not propagate to the parent.  
  
 For eg: if from within the `<contact-details>` we change the email from `bond@mi6.com` to `jamesbond@mi6.com`, the parent data i.e. phone data property in `<user-component>` will still contain a value of `bond@mi6.com`.    
  
However, if we change the value of email from `bond@mi6.com` to `jamesbond@mi6.co` in the parent component (`<user-component>` in our use case) then the value of email in the child component (`<contact-details>` in our use case) will change to `jamesbond@mi6.com` automatically - change in parent is instantly propagated to the child.
  
**Two-Way Binding**   
 
If we want two-way binding then we have to explicitly specify two-way binding as `:email.sync="email"` instead of `:email="email"`. Now if we change the value of prop in the child component the change will be reflected in the parent component as well.  
  
  
  
In a medium to large app changing parent state from the child state will be very hard to detect and keep track of especially while debugging - **Be cautious** .  
  
There won't be any .sync option available in Vue.js 2.0. **The two-way binding for props is being deprecated in Vue.js 2.0**. 
  
**One-time Binding**  
  
It is also possible to define explicit **one-time** binding as `:email.once="email`, it is more or less similar to passing a literal, because any subsequent changes in the parent property value will not propagate to the child.  
  
**CAVEAT**     
When **Object or Array** is passed as prop, they are **ALWAYS PASSED BY REFERENCE**, which means irrespective of the binding type explicitly defined `:email.sync="email"` or `:email="email"` or `:email.once="email"`, if email is an Object or an Array in the parent then regardless of the binding type, any change in the prop value within the child component will affect the value in the parent as well.  

**Props as Array**   

In the `contact-details.js` file we have defined `props:['phone', 'email']` as an array, which is fine if we do not want fine grained control with props.  
  
**Props as Object**    

If we want more fine grained control over props, like   
  - if we want to define what type of values are acceptable as the prop  
  - what should be a default value for the prop  
  - whether a value is MUST (required) to be passed for the prop or is it optional  
  
then we need to use object notation for defining the props, as we have done in `address.js`.  
  

If we are authoring reusable components which may be used by other developers on the team as well, then it is a good practice to define props as objects so that anyone using the component has a clear idea of what should be the type of data and whether it is compulsory or optional.  
  
It is also referred to as ***props validation***.  The **type** can be any one of the following native constructors:  
  - String  
  - Number  
  - Boolean  
  - Array  
  - Object  
  - Function  
  - or a Custom Constructor  
  
Some examples of prop validation as taken from http://vuejs.org/guide/components.html#Props  
  
    Vue.component('example', {
       props: {
           // basic type check (`null` means accept any type)
           propA: Number,
           // multiple possible types (1.0.21+)
           propM: [String, Number],
           // a required string
           propB: {
             type: String,
             required: true
           },
           // a number with default value
           propC: {
              type: Number,
              default: 100
           },
           // object/array defaults should be returned from a
           // factory function
           propD: {
              type: Object,
              default: function () {
                 return { msg: 'hello' }
             }
           },
           // indicate this prop expects a two-way binding. will
           // raise a warning if binding type does not match.
           propE: {
              twoWay: true
           },
           // custom validator function
           propF: {
              validator: function (value) {
                 return value > 10
              }
           },
           // coerce function (new in 1.0.12)
           // cast the value before setting it on the component
           propG: {
              coerce: function (val) {
                return val + '' // cast the value to string
              }
           },
           propH: {
              coerce: function (val) {
                return JSON.parse(val) // cast the value to Object
              }
           }
        }
    });
  
**camelCase vs kebab-case**  
  
HTML attributes are case-insensitive, which means it cannot differentiate between `addresstype` and `addressType`, so when using camelCase prop names as attributes we need to use their kebab-case(hyphen-delimited) equivalents:  
`addressType` should be written as `address-type` in HTML attribute.
 
        
        

## Passing Props While Using Vue JSX
We have a parent component: Importing a child component in it we'll pass props via an attribute. Here the attribute is 'src' and we're passing the 'src' too.

# ParentComponent.js

    import ChildComponent from './ChildComponent';
    export default {
        render(h, {props}) {
            const src = 'https://cdn-images-1.medium.com/max/800/1*AxRXW2j8qmGJixIYg7n6uw.jpeg';
            return (
               <ChildComponent src={src} />   
            );
        }
    };

And a child component, where we need to pass props. We need to specify which props we are passing.

# ChildComponent.js:
    export default {
        props: ['src'],
        render(h, {props}) {
            return ( 
                <a href = {props.src} download = "myimage" >
                    Click this link
                </a>
            );
        }
    };



## Dynamic Props
Just as you're able to bind data from a view to the model, you can also bind props using the same v-bind directive for passing information from parent to child components.

JS
-
    new Vue({
        el: '#example',
        data: {
            msg: 'hello world'
        }
    });

    Vue.component('child', {
        props: ['myMessage'],
        template: '<span>{{ myMessage }}</span>
    });

HTML
-
    <div id="example">
        <input v-model="msg" />
        <child v-bind:my-message="msg"></child>
        <!-- Shorthand ... <child :my-message="msg"></child> -->
    </div>
Result
-
    hello world

