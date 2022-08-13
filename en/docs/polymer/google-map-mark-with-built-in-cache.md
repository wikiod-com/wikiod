---
title: "Google Map Mark With Built in Cache"
slug: "google-map-mark-with-built-in-cache"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Syntax
 - <g-map
        
       api-key="AIzaSyBLc_T17p91u6ujSpThe2H3nh_8nG2p6FQ"
        
       locations='["Boston","NewYork","California","Pennsylvania"]'></g-map>

## Parameters
|api-key|google's [javascript api key][1]|
|--------|--------|
|**locations**|List of locations you want to mark on the google map|
|-----------|------------|


  [1]: https://developers.google.com/maps/documentation/javascript/get-api-key

For the entire code, [Navigate to the Repository][1]

To see the google map in action, [Look Here][2]


  [1]: https://github.com/Nitin-Prabhakar/g-map.git
  [2]: http://polymer-project.nitinprabhakar.com/g-map/

## A - Getting Started
In a nutshell, Our Custom Element Should

 - Have a Built in Search Functionality, that searches and marks places on the map.
 - Accept An attribute called location-array , which is a list of places
 - Have a Listener , for listening to an event called “google-map-ready“.This event is fired, when the element has loaded.
   - This listener, should loop through the location-array, and assign the next element in your location-array as the current “search query”
 - Have A method called cache
   - This method, caches the latitude and longitude of each place in the location-array, as soon as the search returns a result
 - Have a Dom-repeat template, that loops through the cached results, and drops a marker at each location

So, the purpose of our custom element in essence means, If you pass an array of locations like so:

`<g-map location-array='["Norway","Sweden"]'></g-map>`

The custom element

 - Should Not only mark Norway and Sweden on the map, but,
 - Should also mark any subsequent searches on the map – i.e, If you search for Boston, after the map has marked Norway and Sweden, Boston should also have a pin on the map

Importing Dependencies
---------

First, Let us import whatever we need into our element.This element is under

elements/g-map.html

```html
<link rel=import href="../bower_components/google-map/google-map.html">
<link rel=import href="../bower_components/google-map/google-map-marker.html">
<link rel=import href="../bower_components/google-map/google-map-search.html">
<link rel=import href="../bower_components/google-map/google-map-directions.html">
```

> 
> 
> 
> Note
> ----
> 
> Some of the above imports, aren’t necessary, but good to have, in case
> you fancy add on functionality on your markers
> 
> We necessarily need,
> 
>  - google-map
> -  google-map-marker 
> - google-map-search
>
>
> Also, It is presumed, you know how to install the individual Polymer Elements Via Bower

Registering Our Element With Polymer
---------

Next, we register our custom element as “g-map”

> 
> ```js    
> Polymer({
>        is:"g-map" 
> });
> ```

So! our custom element is registered with Polymer.

We will add google map search specific properties and any other properties we need, later.

Adding a Blueprint to our custom element - via templates
----

Next, we add the template for our custom element

Open up elements/g-map.html, and add a template that binds the custom elements DOM

```html
<template id="google-map-with-search">
</template>
```
Now, wrap the entire html and javascript you wrote for our custom element, inside of a dom-module.

```html
<dom-module id="g-map">
 
    <template id="google-map-with-search">
       
    </template>
    <script>
    Polymer({
        is:"g-map",
        properties: {}
 });
     </script> 
</dom-module>
```
Very Nice! We have a basic blueprint



## B - Rendering the basic google map
Getting You Browser Ready - Polyfill it
----

Create a new landing page for our element, at index.html.

Include polyfills for a custom element to work.Also import the custom element g-map, which we registered with Polymer.

> Note:
> -----
> 
>  - Webcomponents, are the necessary polyfill for browser support.
>  - Polymer, is included, so that we can use the library to create our
> custom element

So Open up index.html, and include the necessary Polyfills and Polymer.
Also, **import** the custom element we registered

```html
<head>
    <title>Google Map</title>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width,initial-scale=1.0">
     <script src="bower_components/webcomponentsjs/webcomponents.min.js"></script>
     <link rel="stylesheet" href="bower_components/bootstrap/dist/css/bootstrap.min.css">
     <link rel="import" href="bower_components/polymer/polymer.html">
     <link rel="import" href="elements/g-map.html">
 
</head>
```
So With the Browser Polyfilled,

Let us invoke the google-map polymer element inside our custom element.

```html
<template id="google-map-with-search">
 
    <google-map></google-map>
 
</template>
```
and then, invoke the custom element inside index.html, our landing page.

So this goes inside the index.html

```html
<body>
 
    <div class="container">
 
        <div class="row">
 
            <div class="col-xs-12">
                <g-map></g-map>
            </div>
 
        </div>
 
    </div>
 
</body>
```
I tried rendering my page with the custom element g-map at this point, and found an EMPTY page!

WHY??

When I see the console log, I see this

[![enter image description here][1]][1]


Ah!

So, We need an api key to render a map from google.

> 
> 
> 
> Note:
> -----
> 
> 
> Getting an API key from google’s javascript API, is pretty simple.
> 
> Just follow the link below, to generate your personal key per project.
> 
> [Google API Key][2]


Now,
Since the API Key is to be used, we have two choices.

 - Let the user pass it as an attribute
 - Have a default API Key configured

Now, I would go for the first, simply because, any API Key has its imposed limits. i.e it can only be used so many times.So, a key configured and shipped with the element we develop, could soon run out.

Whoever wants to use the custom element, can generate a new key, and pass it as an attribute.

like so:
```html
<g-map api-key="AIzaSyBLc_T17p91u6ujSpThe2H3nh_8nG2p6FQ"></g-map>
```

So We are passing the api key as an attribute and within our custom element, we bind it on to the Polymer element’s api-key attribute.

like so:

```html
<google-map api-key=[[apiKey]]></google-map>
```
So Once we pass the API Key, and refresh, we Still do not see the map!

> Note:
> ----
> 
> Google maps, need some css to render.We need the height of the map set
> explicitly.
> 
> So, make a css file inside the css directory, say app.css and, add
> these lines
> 
> ```css 
> google-map{
>  
>     min-height:30vmax;   
> } 
>```

And now upon refresh, we see this

[![enter image description here][3]][3]


  [1]: https://i.stack.imgur.com/0vfwD.jpg
  [2]: https://developers.google.com/maps/documentation/javascript/get-api-key
  [3]: https://i.stack.imgur.com/9Qfea.jpg


Yay! We just rendered a map with just minimal markup! We just wrote this!

```html
<google-map api-key="[[apiKey]]"></google-map>
```

## C - Adding Search Ability to our custom element
For Searching a place, we use the powerful element that Polymer ships, called google-map-search .

All you need to do, is pass

 - a map object  and
 - a query string
to the element like so:

```html
<google-map-search map=[[map]] query=[[query]]></google-map-search>
```
How do we pass the map object? Where do we get that from?

Simple!

When you invoke the google-map element, bind your custom element <g-map>’s property map, to the <google-map> element’s property map.

So, we invoke the google-map element like so:

```html
<google-map api-key="whatever key you generated for google's javascript API" map={{map}}></google-map>
```

> Note:
> ----
> 
> We do a two way data bind for map above, when invoking the google-map
> polymer.
> 
> It is represented by the curly braces {{}} instead of a one way
> binding indicated by square braces[[]].
> 
> When we do a two way bind,
> 
>  - Whenever our custom element g-map’s map property changes, google-map element is notified  
> 
> and,
> 
>  
> 
>  - Whenever google-map’s map property changes, we are notified in our custom element g-map.

So, with the two way data binding in place, anytime the map object changes in google-map, our custom element is updated with the changes.

and we pass the map object as an attribute to the element google-map-search, so that , any search results are marked in the current map rendered.

How does our custom element’s DOM look at this moment?

```html
<template id="google-map-custom-element">
 
<google-map-search map=[[map]] query=[[query]]></google-map-search>
<google-map api-key=[[apiKey]] map={{map}}></google-map>
</template>
```
We never added the map and query properties for our custom element!

Add the properties in the registration call to Polymer

```js
Polymer({
 
    is:"g-map",
 
    properties:{
 
        map:{
 
            type: Object
 
        },
 
        query:{
 
            type:String,
 
            value:""
 
        }
    }
 
});
```

Now that we have registered the properties, we note, that we

 - Are getting the currently rendered map as an object  – via data binding and storing it as a local property also called map,
 - Have a property query, passed to google-map-search

Accepting User Input as Search Query
-----

But who gives us the query to search for? Err! right now? NOBODY.

Thats precisely what we do next.

Let us add a search form, which the user who renders the map, would use to input a query. We need inputs for the search box, and we use Polymer’s iron input.

Adding a Search Form
-----
Include the necessary iron elements at the beginning of the file g-map.html

```html
<link rel=import href=../bower_components/iron-input/iron-input.html>
<link rel=import href=../bower_components/iron-icon/iron-icon.html>
<link rel=import href=../bower_components/iron-icons/iron-icons.html>
<link rel=import href=../bower_components/iron-icons/maps-icons.html>
```
We want the search form, at the top left corner, so we add an iron input with position absolute, and some css to pin it there.

So, wrap the DOM for our custom element, inside a div called “map-container“, and add a child div called “search_form“.

```html
<template id="google-map-custom-element">
    <div class="map-container">
         <google-map-search map="[[map]]" query="[[query]]"></google-map-search>
         <google-map api-key="[[apiKey]]"></google-map>
         <div class="search_form">
             <iron-icon icon="icons:search" on-tap=__search></iron-icon>
             <input is="iron-input" placeholder="Search Places" type=text name="search" bind-value="{{query}}">
         </div>
     </div>
 </template>
```
Add some basic css to pin the search form we wrote to the top right of the map

```css
.search_form{
    position: absolute;
    top:10px;
    right:10px;
}
.search_form iron-icon{
    position: absolute;
    right: 1px;
    top:1px;
    cursor: pointer;
}
```
And , then when we refresh, we have this. The search form at the top right

[![enter image description here][1]][1]


All that is left to do is, for us to provide an input to google-map-search element, via the query property of our custom element.

So, we bind our property “query”, to the iron input in the search form. like so:


```html
<input is="iron-input" placeholder="Search" type=text name="search" bind-value="{{query}}">
```

> Note:
> ---- 
> We bind our custom element’s property query, to the search input using,
> 
> “bind-value” attribute of an iron input.
> 
> So, bind-value={{query}} in the above iron input, makes sure, any
> changes to the text inside the iron input, is notified back to the
> query property.
> 
> With our current setup,
> 
> For every alphabet a user types into the search box, a search happens,
> which slows down the map, and Consumes the limit on our API Key.
> 
> Why?
> 
> Because, our custom element’s property query, is bound to the iron
> input, and every letter typed into the iron input, changes the value
> of “query“, which then affects the html in
> 
> ```html 
> <google-map-search map="[[map]]" query="[[query]]">
> ```
> causing a search to happen

Our solution is simple! Just bind the “query” attribute of google-map-search element, to a separate property.

Let us say, we create a property “searchQuery” for our custom element.

Add this, below the “query” property in the Polymer call

```js
searchQuery :{
 
    type:String
 
}
```
Next, change the bind in google-map-search element call in our custom element’s DOM

like so:

```html
<google-map-search map="[[map]]" query="[[searchQuery]]">
```
Lastly, Note that we added an iron icon (search icon) to our search form?

```html
<iron-icon icon="icons:search" on-tap=search></iron-icon>
```
Let us add that search function to our custom element. It is called on tapping the search icon.

In our search method, we assign whatever is there in the search box, to the “query” attribute of google-map-search!

Add this function, after the “properties” object in the polymer call for registration.

```js
//paste this after, the properties object
 
search: function() {
this.query = this.searchQuery;
}
```
Very Nice! So, Now, A Search happens only on tap of the search icon.

Display The Search Results
----

We have

 - Rendered a Map
 - Pinned a search form to the Map
 - Wrote the functionality to search on tap of the search icon

Now we need to display the results.

For that, we need to bind the results from the google-map-search element, onto a local property.

So, Let us create a property for our custom element g-map, and name it results duh!

In the Polymer registration call, add the property results of type object in the properties declaration

```js
results:{
 
type:Object
 
}
```
And bind the results property of google-map-search to the local results property you defined above.

```html
<google-map-search results={{results}} map=[[map]] query=[[query]]></google-map-search>
```
With that in place, let us refresh the page, and search for **starbucks** on the map.

[![enter image description here][2]][2]

Nothing happened!

Well Duh! we just bound the results from google-map-search, to the results property. Did we write anything to display them on the map? NO!

We Need to.

So, we know, that the Polymer element google-map-search, returns an array of markers as results.

We need to loop through the array, and place markers on the map.

So, we write a dom-repeat template, within our <google-map> element, to display the markers for each result

like so:


```html
<google-map api-key=[[apiKey]] map=[[map]]>
 
    <template is="dom-repeat" items="{{results}}" as="marker">
        <google-map-marker latitude="{{marker.latitude}}" longitude="{{marker.longitude}}">
            <h2>{{marker.name}}</h2>
            <span>{{marker.formatted_address}}</span>
        </google-map-marker>
    </template>
 
</google-map>
```

And Now on refresh, we see this

[![enter image description here][3]][3]


  [1]: https://i.stack.imgur.com/X9gYH.jpg
  [2]: https://i.stack.imgur.com/xG6EV.png
  [3]: https://i.stack.imgur.com/yCHBc.jpg


Very Nice!

We have successfully added the search functionality to our custom element.

All we need to do is,

write this in our index.html

```html
<g-map api-key="Whatever API key you generated"></g-map>
```
And you get a map on which you can search!

## D - Caching Search Results
Our goal however, was to

 - make a list of places
 - mark all of them on the map and,
 - mark any other place of our choice, by using the search box

We have a search box, where the user can search for one query at a time.In order to accommodate multiple queries, we need to cache the results for each query,  before we display the markers and pin them on the map

So, Let us add

 - A Property say locations  which accepts an array of locations
 - A Property cache which stores the results of each query in the location array passed to our custom element

```js
cache:{
 
type:Object
 
},
 
locations:{
 
type:Object
 
}
```
Next, we write a function to cache each search result , resulting from a search for every place in the location array.

It is pretty simple!

We already are gathering the results into a property called “results”, which we then are looping through to display markers.

All we now need to do, is push each search result into an array, and our cache is done!

We need to use an event to trigger caching. That event, is called, “on-google-map-search-result” and it is fired, after the element google-map-search finishes a search.

So “on-google-map-search-result” , we call a function say “cacher” , to cache the current search results into a property called cache.

```html
<google-map-search map=[[map]] query=[[searchQuery]] on-google-map-search-results=cacher>
```

> Note:
> ----
> 
> Normally, to push an item into an array, we use the vanilla push
> method
> 
> e.g: I have a new place to add to my list of places I want to mark,
> say NewYork.
> 
> If the array I want to add it to is called locations, I would write
> 
> ``` 
> locations.push('New York');
>  ``` 
> However, in our case, we need to
> push any result from google-map-search into an array called cache.Then
> we need to loop through the cache with the latest results included.Not
> the existing cache.

Now, we use the dom-repeat template to loop through the results and place markers on the map.

However, If we pass a location array like so:

```html
<g-map locations=["Iceland","Argentina","London"]></g-map>
```
Then, the results property of our custom element <g-map>, will be overwritten for every item in that array.

Hence, we call the cacher method on-google-map-search-results event, and push the current result into the cache property.

This can not be a vanilla push like in the note above.

We Need to let the dom-repeater know, that our cache has been overwritten with each new search.

So, we use a special push that Polymer ships, like so:

```js
cacher: function() {
this.push('cache', this.results);
}
```
Add that function, after the search function we wrote earlier.

The syntax implies, that the property cache is to be mutated, by addition of the current value of the property results and any dom-repeaters using the cache property to loop through, are to be notified of the mutation.

Lastly, we change the dom-repeat template to loop through cache, instead of results

> Note:
> ----
> 
> cache is an array of arrays.
> 
> each element of the cache array, is a results array

So our dom-repeater, will be like so:

```html
<template id="resultList" is="dom-repeat" items="[[cache]]">
 
    <template is="dom-repeat" items="[[item]]" as="marker">
 
        <google-map-marker latitude="{{marker.latitude}}" longitude="{{marker.longitude}}">
            <h2>{{marker.name}}</h2>
            <span>{{marker.formatted_address}}</span>
        </google-map-marker>
    </template>
</template>
```
Very Nice! We have the custom element all coded and ready to be used!

Let us test it out by passing an array of locations from index.html shall we?

```html
<g-map
 
api-key="AIzaSyBLc_T17p91u6ujSpThe2H3nh_8nG2p6FQ"
 
locations='["Boston","NewYork","California","Pennsylvania"]'>
</g-map>
```
and there we go!

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/cmGCY.png

## The Complete Custom Google Map Polymer With In Built Cache
```
<link rel=import href="../bower_components/google-map/google-map.html">
<link rel=import href="../bower_components/google-map/google-map-marker.html">
<link rel=import href="../bower_components/google-map/google-map-search.html">
<link rel=import href="../bower_components/google-map/google-map-directions.html">
<link rel=import href="../bower_components/iron-input/iron-input.html">
<link rel=import href="../bower_components/iron-icon/iron-icon.html">
<link rel=import href="../bower_components/iron-icons/iron-icons.html">
<link rel=import href="../bower_components/iron-icons/maps-icons.html">
<dom-module id="g-map">
    <template id="google-map">
        <div class="map-container">
            <google-map-search map="[[map]]" query="[[query]]" results="{{results}}" on-google-map-search-results=cacher>
            </google-map-search>
            <google-map api-key="[[apiKey]]" map="{{map}}" on-google-map-ready="_onMapResolve" fit-to-markers disable-zoom single-info-window>
                <template id="resultList" is="dom-repeat" items="[[cache]]">
 
                    <template is="dom-repeat" items="[[item]]" as="marker">
 
                        <google-map-marker latitude="{{marker.latitude}}" longitude="{{marker.longitude}}">
                            <h2>{{marker.name}}</h2>
                            <span>{{marker.formatted_address}}</span>
                        </google-map-marker>
                    </template>
                </template>
            </google-map>
            <div class="search_form">
                <p>
                    <iron-icon icon=icons:search on-tap=search></iron-icon>
                    <input is=iron-input placeholder="Search" type=text name=start bind-value={{searchQuery}}>
                </p>
            </div>
        </div>
    </template>
    <script>
        Polymer({
            is: "g-map",
            properties: {
                apiKey: {
                    type: String,
                    value: "AIzaSyBLc_T17p91u6ujSpThe2H3nh_8nG2p6FQ"
                },
                map: {
                    type: Object
                },
                query: {
                    type: String,
                    value: ""
                },
                locations: {
                    type: Array,
                    value: []
                },
                cache: {
                    type: Array,
                    value: []
                },
                results: {
                    type: Array,
                    value: function() {
                            return [];
                        }
                    }
            },
            _onMapResolve: function() {
                var search = document.querySelector("google-map-search");
                this.locations.forEach(function(location) {
                    search.query = location;
                })
            },
            search: function() {
                this.query = this.searchQuery;
            },
            cacher: function() {
                this.push('cache', this.results);
            }
        })
    </script>
</dom-module>
```

