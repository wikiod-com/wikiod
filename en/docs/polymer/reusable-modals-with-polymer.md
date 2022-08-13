---
title: "Reusable Modals with Polymer"
slug: "reusable-modals-with-polymer"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Syntax
 - Element Declaration: <tool-bar link2-share=""link2-fork=""modal-id=""title=""></tool-bar> 

> Note:
> 
> The code through this writeup, is not a working copy. You need to
> replace the fillers for hrefs,src’s and project names.
> 
> The code illustrates only a Proof of concept.
> 
> To see the custom element <tool-bar> in action,
> 
> [go here][1]
> 
> And, to browse through the usage and structure of the custom element,
> 
> [go here][2]
> 
> The ***Usage*** is in “index.html”
> 
> The ***element*** is in, “elements/tool-bar.html“


  [1]: http://portfolio.nitinprabhakar.com
  [2]: https://github.com/Nitin-Prabhakar/career-portfolio

## Modals
So you want to add material design to your business or career portfolio.huh? You Just can not resist using a Modal? which pops upon a click on those crisp cards you intend to design, for each of your projects/products!

Let us say, for each project, you have done, or for each product you have designed, you need a material card.Each such card should have

 - An icon, that pops up an info box, that lists all the features of
 - your product or project, in detail. A “share this” icon, that shares
 - your product or project across social media A “fork this” icon, that
   opens up the github page for your product/project

With native html, and assuming you use bootstrap or any other flex box layout, you would need to

 - Write a wrapper div for each new project with a class row
 - Wrap your hero image for each project in a column 12 cols wide
 - Write Another row
 - Wrap each link( info | share | fork) in a column 4 cols wide of its own
 - Inject these two rows in to a container

For starters.
Once you have done the above for all your projects, you need to work on creating the required pop up for each project.So,

 - You download tons of javascript and css (by customizing bootstrap for modal functionality)
 - You write a Modal with a unique id, to differentiate each project’s info(modal- P1 for Project 1, modal-P2 for Project 2 and so on)
 - You then compose the html required, to display information for each project, in its corresponding Modal.

What are our goals?
-------------------

Just to reiterate, visually, If you have two projects in your portfolio, the goal is to have cards like so:

[![enter image description here][1]][1]



  [1]: http://i.stack.imgur.com/4zPvC.jpg

So we have two concerns for EACH Project

 - The hero image
 - The tool bar with info(Pops up a Modal),share and fork links

First, Let us tackle what happens when a user clicks on the info link in the bar below each project.We need a Modal to pop up, with more info about the project.

So How exactly do you configure a Modal with bootstrap?

 - You write this to trigger it, for each project

`<button type="button" class="btn btn-info btn-lg" data-toggle="modal" data-target="#myProject1"><img src="path-to-info-icon"></img></button>`

 - You then write the body of the Modal like so:

```html
<div id="myProject1" class="modal fade" role="dialog">
     <div class="modal-dialog">
         <div class="modal-content">
             <div class="modal-header">
                 <button type="button" class="close" data-dismiss="modal"></button>
                 <h4 class="modal-title">Project Title</h4>
             </div>
             <div class="modal-body">
                 <p>Some text in the modal.</p>
             </div>
             <div class="modal-footer">
                 <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
             </div>
         </div>
     </div>
</div>
```
and depending on the content and the length of your Project Description, You actually get a little lost monitoring the opening and closing of a div soup above in the body of the Modal!

Plus, That markup is downright tedious to compose EVERYTIME you need a modal.

Phew! And did it end there? No. You still need to write for each project,

 - The Project card’s hero image,that is displayed above the tool bar.


So How do we add the hero image for each project? MORE html!


The hero image needs

- A row of its own
 - A column 12 cols wide of its own inside that row

How would the complete html look for us to display one project as a card shown above?

```html
<article id="project-neighbourhood">
 
    <div class="row">
 
        <div class="col-12">
 
            <img src="path-to-hero-image"></img>
 
        </div>
 
    </div>
 
    <div class="row">
 
        <div class="col-12">
            <div class="row">
                <div class="col-4">
                    <button type="button" class="btn btn-info btn-lg" data-toggle="modal" data-target="#myProject1"><img src="path-to-info-icon"></img></button>
                </div>
                <div class="col-4">
                    <button class="btn btn-lg" id="share-on-g-plus"><img src="path-to-share-icon"></img></button>
                </div>
                <div class="col-4">
                    <button class="btn btn-lg" id="fork"><img src="path-to-fork-icon"></img></button>
                </div>
             </div> 
        </div> 
    </div> 
</article> 
```

Now say you have 10 projects to show off! You need to rewrite that messy markup 10 times! Plus 10 different Modals!

I do not have patience to rewrite that html, and 10 Modals, and I know you dont too!








## Modals, With Polymer?


Note
----

>
> 
> I assume you know
> 
> [Webcomponent Specifications][1]
>
> [Polymer from Google][2]


  [1]: http://blog.nitinprabhakar.com/webcomponents
  [2]: http://blog.nitinprabhakar.com/polymer-beginner/

What if, we could just have one custom element, say

`<tool-bar></tool-bar>`

and it magically did everything that messy Modal markup could?

Tempting eh!

How do you specify which Modal belongs to which project?

Simple! just write

`<tool-bar modal-id="Project-cats"></tool-bar>`

So that reserves the markup with an id of “Project-cats” for the project on cats for example.

How do you write what goes into the modal? simple! Just write your normal markup, wrapped, within the custom tag “<tool-bar>”

Like so:

```
<tool-bar modal-id="Project-cats">
 
<div class="col-12 modal-desc">
    <p>Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.</p>
</div>
 
</tool-bar>
```

Ain’t that simple enough?

And If you are wondering How the complete markup look? Including the share and the fork links, see below.

```
<div class=row>
 
    <div class="col-12"> <img src="path-to-hero-image"></img></div>
 
</div>
 
<div class="row">
 
    <tool-bar
 
    modal-id="Project-cats"
 
    link2-share="http://www.project-cats.com/kitten"
 
    link2-fork="https://github.com/myusername/my-project-cats">
 
        <div class="col-12 modal-desc">
 
            <p> yadda yadda blah blah</p>
 
        </div>
 
    </tool-bar>
 
</div>
```
Much better than rewriting the complete div soup eh!

Of course you could shorten it further, and make it completely composible by abstracting the hero image implementation inside the custom element, but let us hold off on that, as a later concern.




## Reusable Toolbar with Modal | Share | Fork icons  - Coding the element

So far, we defined how easy it is to write a custom element that hides the messy html behind it and gives the user, an easy to write and brief markup.

Time to code it!

Our custom element, to display the bar below the hero image should

 - Accept a Link to be shared
 - Accept a Link to the Repo to be forked
 - Accept a modalId to differentiate between other Modals.
 - Accept a Title for the Modal

Register the tool bar custom element with Modal | Share | Fork icons
--------------------------------------------------------------------

Let us register our custom element, that accepts the above list of properties.

```
<script>
Polymer({
    is:"tool-bar",
    properties:{
        link2Share:{
            type:String,
            value:""
        },
        link2Fork:{
            type:String,
            value:""
        },
        modalId:{
            type:String,
            value:""
        },
       title:{
            type:String,
            value:"myModal"
        }
    }
});
</script>
```

Very nice! Next, add the Html we need.

First up, The icons for Modal/Share/Fork need to be added.A simple ul will do.Also,we’ll be using, Polymer’s iron icons for displaying Modal/Share/Fork icons.

Note: Iron Icons from Polymer
---

>
>
>Install the icon sets like so, in the root of your project.
>
>```bash $ bower install --save PolymerElements/iron-icon```
> 
>```bash $ bower install --save PolymerElements/iron-icons```

Next, Include the installed Polymer elements in the custom element declaration, like so

> Note:
> 
> For all practical purposes, our custom element, will be lodged inside
> 
> ***Project-root/elements***

```
<link rel="import" href="../bower_components/iron-icon/iron-icon.html">
<link rel="import" href="../bower_components/iron-icons/iron-icons.html">
<link rel="import" href="../bower_components/iron-icons/social-icons.html">
```

Very Nice! If you are wondering how to use iron icons, see the note below.

Note:  Iron Icon usage and Grammar
--- 
>If we need to use the info icon for our Modal Popup, we write the markup like so: 
>
>```<iron-icon icon="icons:info"></iron-icon>``` 
>
>If we
> want the social networking share icon, then the icon attribute above,
> becomes
> 
> icon=”**social:share**”
> 
> So the grammar meaning, I need the iconset from “SOCIAL” and I want
> the SHARE icon from that set.
> 
> You can alternatively, use font-awesome , in which case, you can use,
> 
> <i class="fa fa-share-alt"></i>
> 
> choose what you deem best fit.

Once the includes are done,we write the template for our custom element. We will be using one way binders for data in our custom element’s markup.We bind whatever is passed as attributes by the user, to their corresponding properties of our element, while embarking on our element’s registration call.

So the html blueprint for our custom element is:

```
<dom-module id="tool-bar">
 
    <template id="tool-box">
 
        <ul class="flex-wrap toolbar">
            <li>
                <iron-icon icon="icons:info" id="anchor-for-[[modalId]]" onclick="clickHandler(event)"></iron-icon>
            </li>
            <li>
                <a href="https://plus.google.com/share?url=[[link2Share]]" target="_blank" ><iron-icon icon="social:share"></iron-icon></a>
            </li>
            <li>
                <a href="[[link2Fork]]" target="_blank"><i class="fa fa-2x fa-code-fork" aria-hidden=true></i></a> 
            </li>
        </ul>
 
    </template>
 
</dom-module>
```

Cool! So the tool bar has its templated ul , which lists the three links

 - info icon – Modal Popup
 - share this icon – share on google
 - Fork this icon – Fork the repo

Also, we have bound the attributes the user writes, to the respective properties.

Note: Making the Modal ID unique
---

> We have addressed uniqueness of the info icon, by specifying the id
> attribute like so
>
> ``` <iron-icon icon="icons:info"
> id="anchor-for-[[modalId]]" onclick="clickHandler(event)"></iron-icon>
> ```
> 
> So each Project, will have a unique anchor id.
> 
> i.e,
> 
> If the user passes “project-cats” as modal-id, in the call to our
> custom <tool-bar> element, the anchor id would be
> “anchor-for-project-cats”
> 
> If the user adds another project, and passes “project-puppy” as the
> modal-id attribute, the anchor id would be anchor-for-project-puppy
> 
> and so on.
> 
> But How do we ensure, each info icon is mapped to its own Modal?
> 
> The “**iron icon**“ for **info**, should be assigned a
> “***data-dialog***” attribute, which needs to be unique as well.
> 
> The data-dialog attribute, is similar to the data-target attribute in
> the bootstrap modal.It needs to be unique for every new Project.
> 
> We leave it to the user, to maintain uniqueness. So the user can not
> have the same modal-id attribute for different `<tool-bar>` calls.

So as the note above states, we need to map each Project’s info icon, to a data-dialog.

Let us add that functionality.

We use the ready method.It is very similar to Jquery’s

`$.ready();`

.It is invoked, when the custom element <tool-bar>, is finished loading.

Add this function, after the properties object,in the Polymer call for registration.

```
ready: function() {
    document.querySelector("#anchor-for-" + this.modalId).setAttribute('data-dialog', this.modalId);
}
```

Very Cool! So now we have

 - A custom element registered as a Polymer element
 - Modal | Share | Fork icons added to the custom element

A Modal is attached to the info iron-icon,with the id “anchor-for-[Whatever is passed as modal-id]”. That is courtesy the ready method of our element.

So, so far, writing

```
<tool-bar
link2-share="http://www.myportfolio.com/project-neighbourhood-map"
link2-fork="https://github.com/me/project-neighbourhood-map"
modal-id="project-neighbourhood-map">
</tool-bar>
```

Produces this:

[![enter image description here][1]][1]


But when the user clicks on that info icon, something like This should happen

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/y8sJp.jpg
  [2]: http://i.stack.imgur.com/2osBg.jpg

and our goal was to wrap this complete description of the project, within the `<tool-bar>` tag on the users page.

Remember Shadow DOM? We need to have a container in our custom element, that provides the shadow DOM for our Modal’s contents.The content itself, is wrapped inside a `<content>` tag inside the custom element’s shadowDOM.

For the container that hosts the Project’s description, we use the “**paper-dialog**” Polymer element. Paper-dialog, is assigned as id, whatever is passed as the modal-id in the <tool-bar> declaration on the user’s page.

So, add this markup, after the ***ul*** in the tool-bar custom element’s DOM

```
<paper-dialog id="[[modalId]]" modal>
    <div class="text-right modal-close">
        <iron-icon icon="icons:power-settings-new" dialog-dismiss></iron-icon>
    </div>
    <h2 class="text-center text-capitalize">[[title]]</h2>
    <paper-dialog-scrollable>
        <div class="container-fluid">
            <div class="row flex-wrap info">
                <content></content>
            </div>
        </div>
    </paper-dialog-scrollable>
</paper-dialog>
```

Note: Paper-dialog-scrollable
---

> We use ***Paper-dialog-scrollable***, to wrap our Project’s description, so
> that, Any lengthy descriptions, do not overflow.

The last bit, is to add the onclick functionality for when the modal should open, on click of the info icon.

When a click happens, as is normal, an event is triggered, which we pass to our clickHandler function like so:

```
<iron-icon icon="icons:info" id="anchor-for-[[modalId]]" onclick="clickHandler(event)"></iron-icon>
```

So Once the clickHandler function gets the event passed to it,

 - It needs to gather, which element triggered the click,
 - It needs to gather, the data-dialog attribute of that element, to ascertain which modal to open(Remember each project has its own modal id?)
 - It then needs to call the open method, for exactly that modal


So, the function is like so:

Add this, before the element’s registration call.This is not a part of the element, but is scoped to within the element.

```
function clickHandler(e) {
    var button = e.target;
    while (!button.hasAttribute('data-dialog') && button !== document.body) {
        button = button.parentElement;
    }
 
    if (!button.hasAttribute('data-dialog')) {
        return;
    }
 
    var id = button.getAttribute('data-dialog');
    var dialog = document.getElementById(id);
    if (dialog) {
        dialog.open();
    }
}
```
So, with that, we have completed coding the custom element, `<tool-bar>`.

## The complete picture. Custom Element <tool-bar>
```
<link rel="import" href="../bower_components/iron-icon/iron-icon.html">
<link rel="import" href="../bower_components/iron-icons/iron-icons.html">
<link rel="import" href="../bower_components/iron-icons/social-icons.html">
<link rel="import" href="../bower_components/paper-dialog/paper-dialog.html">
<link rel="import" href="../bower_components/paper-button/paper-button.html">
<link rel="import" href="../bower_components/paper-dialog-scrollable/paper-dialog-scrollable.html">
<dom-module id="tool-bar">
        <template id="tool-set">
            <ul class="flex-wrap toolbar">
                <li>
                    <iron-icon icon="icons:info" id="anchor-for-[[modalId]]" onclick="clickHandler(event)"></iron-icon>
                </li>
                <li>
                    <a href="https://plus.google.com/share?url=[[link2Share]]" target="_blank">
                        <iron-icon icon="social:share"></iron-icon>
                    </a>
                </li>
                <li><a href="[[link2Fork]]" target="_blank"><i class="fa fa-2x fa-code-fork" aria-hidden=true></i></a>
                </li>
            </ul>
            <paper-dialog id="[[modalId]]" modal>
                <div class="text-right modal-close">
                    <iron-icon icon="icons:power-settings-new" dialog-dismiss>                   </iron-icon>
                </div>
                <h2 class="text-center text-capitalize">[[title]]</h2>
                <paper-dialog-scrollable>
                    <div class="container-fluid">
                        <div class="row flex-wrap info">
                            <content></content>
                        </div>
                    </div>
            </paper-dialog-scrollable>
        </paper-dialog>
    </template>
 
    <script>
            function clickHandler(e)
            {
                 var button = e.target;
                 while (!button.hasAttribute('data-dialog') && button !== document.body) {
                    button = button.parentElement;
                 }
 
                 if (!button.hasAttribute('data-dialog'))
                 {
                   return;
                 }
 
                var id = button.getAttribute('data-dialog');
                var dialog = document.getElementById(id);
                if (dialog) {
                  dialog.open();
               }
           }
      Polymer({
             is: "tool-bar",
            properties: {
                  link2Fork:{
                      type:String,
                      value:""
                  },
                 link2Share: {
                       type: String,
                       value: ""
                  },
                 title: {
                       type: String,
                       value: null
                 },
                modalId:{
                       type:String,
                       value:""
               }
             },
            ready: function() {
                  document.querySelector("#anchor-for-" + this.modalId).setAttribute('data-dialog', this.modalId);
            }
         });
    </script>
</dom-module>
```


## Using the custom element <tool-bar> with Modal | Share | Fork icons
On whichever page you want to display your product / project portfolio, invoke the custom element like so:

```
<article id="project-neighbourhood">
    <div class="row">
        <div class="col-12 hero">
            <img src="path-to-hero-image">
        </div>
    </div>
    <tool-bar link2-share="http://www.mywebsite.com/neighbourhood" link2-fork="https://github.com/myusername/neighbourhood" modal-id="project-neighbourhood-map" title="project-neighbourhood-map">
                <div class="col-12">
                      <p> a single-page application featuring a map of my neighborhood or a neighborhood I would like to visit. I will then add additional functionality to this application, including: map markers to identify popular locations or places you’d like to visit, a search function to easily discover these locations, and a listview to support simple browsing of all locations. I will then research and implement third-party APIs that provide additional information about each of these locations (such as Street View images, Wikipedia articles, Yelp reviews, etc).</p>
               </div>
    </tool-bar>
</article>
```
and you get this as preview 

[![enter image description here][1]][1]


And when you click on info, you get this

[![enter image description here][2]][2]



  [1]: http://i.stack.imgur.com/Y3wuF.png
  [2]: http://i.stack.imgur.com/vXlcQ.jpg

