---
title: "Blaze User Interface Recipes (Bootstrap; No jQuery)"
slug: "blaze-user-interface-recipes-bootstrap-no-jquery"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

The above Blaze examples are highly compatible with the http://bootsnipp.com/ library, which only provides the HTML and CSS for components, and leaves the javascript up to the developer.  This allows for components to share the same underlying sorting, filtering, query, and cursor methods.

## Drop Down Menu
The following example creates a Bootstrap Drop-Down menu, using only Blaze and no JQuery.  


**Document Object Model**  
<!-- language: lang-js -->
```

    <nav class="nav navbar-nav">
      <li class="dropdown">
        <a href="#" class="dropdown-toggle" data-toggle="dropdown">{{getSelectedValue}} <span class="glyphicon glyphicon-user pull-right"></span></a>
        <ul class="fullwidth dropdown-menu">
          <li id="firstOption" class="fullwidth"><a href="#">15 Minutes <span class="glyphicon glyphicon-cog pull-right"></span></a></li>
          <li class="divider"></li>
          <li id="secondOption"><a href="#">30 Minutes <span class="glyphicon glyphicon-stats pull-right"></span></a></li>
          <li class="divider"></li>
          <li id="thirdOption"><a href="#">1 Hour <span class="badge pull-right"> 42 </span></a></li>
          <li class="divider"></li>
          <li id="fourthOption"><a href="#">4 Hour <span class="glyphicon glyphicon-heart pull-right"></span></a></li>
          <li class="divider"></li>
          <li id="fifthOption"><a href="#">8 Hours <span class="glyphicon glyphicon-log-out pull-right"></span></a></li>
        </ul>
      </li>
    </nav>

```

**Javascript**  
<!-- language: lang-js -->
```
Template.examplePage.helpers({
  getSelectedValue:function(){
    return Session.get('selectedValue');
  }
});
Template.dropDownWidgetName.events({
  'click #firstOption':function(){
    Session.set('selectedValue', 1);
  },
  'click #secondOption':function(){
    Session.set('selectedValue', "blue");
  },
  'click #thirdOption':function(){
    Session.set('selectedValue', $('#thirdOption').innerText);
  },
  'click #fourthOption':function(){
    Session.set('selectedValue', Session.get('otherValue'));
  },
  'click #fifthOption':function(){
    Session.set('selectedValue', Posts.findOne(Session.get('selectedPostId')).title);
  },
});
```

## Navbars
A very common task is to create responsive navbars and to create action/footer bars that have different controls based on what page a user is on, or what role a user belongs to. Lets go over how to make these controls.

**Router**  
<!-- language: lang-js -->
```
Router.configure({
  layoutTemplate: 'appLayout',
});
Router.route('checklistPage', {
    path: '/lists/:_id',
    onBeforeAction: function() {
      Session.set('selectedListId', this.params._id);
      this.next();
    },
    yieldTemplates: {
      'navbarFooter': {
        to: 'footer'
      }
    }
  });
```


**Create a Navbar Template**  
<!-- language: lang-html -->
```
<template name="navbarFooter">
  <nav id="navbarFooterNav" class="navbar navbar-default navbar-fixed-bottom" role="navigation">
    <ul class="nav navbar-nav">
      <li><a id="addPostLink"><u>A</u>dd Post</a></li>         
      <li><a id="editPostLink"><u>E</u>dit Post</a></li>          
      <li><a id="deletePostLink"><u>D</u>elete Post</a></li>
    </ul>
    <ul class="nav navbar-nav navbar-right">       
      <li><a id="helpLink"><u>H</u>elp</a></li>
    </ul>
  </nav>
</template>
```

**Define Yields in the Layout**  
<!-- language: lang-html -->
```
<template name="appLayout">
  <div id="appLayout">
    <header id="navbarHeader">
      {{> yield 'header'}}     
    </header>

      <div id="mainPanel">
        {{> yield}}
      </div>

    <footer id="navbarFooter" class="{{getTheme}}"">
      {{> yield 'footerActionElements' }}
    </footer>
  </div>
</template>
```

## Modals
This follwing is a pure-Blaze approach to toggling UI elements into and outof existence. Think of this as a replacement for modal dialogs. In fact, there are a number of ways to implement modal dialogs using this method (simply add background masks and animations).  

**Document Object Model**
<!-- language: lang-html -->
```
<template name="topicsPage">
  <div id="topicsPage" class="container">
    <div class="panel">
      <div class="panel-heading">
        Nifty Panel
      </div>
      <!-- .... -->
      <div class="panel-footer">
        <!-- step 1.  we click on the button object -->
        <div id="createTopicButton" class="btn {{ getPreferredButtonTheme }}">Create Topic</div>
      </div>
    </div>

    <!-- step 5 - the handlebars gets activated by the javascript controller -->
    <!-- and toggle the creation of new objects in our model -->
    {{#if creatingNewTopic }}
    <div>
      <label for="topicTextInput"></label>
      <input id="topicTextInput" value="enter some text..."></input>
      <button class="btn btn-warning">Cancel</button>
      <button class="btn btn-success">OK</button>
    </div>
    {{/if}}
  </div>
</template>
```

**Javascript**  
<!-- language: lang-js -->
```
// step 2 - the button object triggers an event in the controller
// which toggles our reactive session variable
Template.topicsPage.events({
  'click #createTopicButton':function(){
    if(Session.get('is_creating_new topic'){
      Session.set('is_creating_new_topic', false);
    }else{
      Session.set('is_creating_new_topic', true);
    }
  }
});

// step 0 - the reactive session variable is set false
Session.setDefault('is_creating_new_topic', false);

// step 4 - the reactive session variable invalidates
// causing the creatNewTopic function to be rerun
Template.topicsPage.creatingNewTopic = function(){
  if(Session.get('is_creating_new_topic')){
    return true;
  }else{
    return false;
  }
}
```

## Tagging
**The Database Layer**
First, we want to set up the Data Distribution Protocol, to make sure that we can persist data to the database, and get it to the client. Three files need to be created... one on the server, one on the client, and one shared between both.
<!-- language: lang-js -->
```
// client/subscriptions.js
Meteor.subscribe('posts');

//lib/model.js
Posts  = new Meteor.Collection("posts");
Posts.allow({
    insert: function(){
        return true;
    },
    update: function () {
        return true;
    },
    remove: function(){
        return true;
    }
});


// server.publications.js
Meteor.publish('posts', function () {
  return Posts.find();
});
```

This example assumes the following document schema for the tagging pattern:
<!-- language: lang-js -->
```
{
  _id: "3xHCsDexdPHN6vt7P",
  title: "Sample Title",
  text: "Lorem ipsum, solar et...",
  tags: ["foo", "bar", "zkrk", "squee"]
}
```

**Document Object Model**  
Second, we want to create our object model in the application layer. The following is how you would use a Bootstrap panel to render a post with title, text, and tags. Note that ``selectedPost``, ``tagObjects``, and ``tag`` are all helper functions of the blogPost template. ``title`` and ``text`` are fields from our document record.
<!-- language: lang-html -->
```
<template name="blogPost">
  {{#with selectedPost }}
    <div class="blogPost panel panel-default">
      <div class="panel-heading">
        {{ title }}
      </div>
        {{ text }}
      <div class="panel-footer">
        <ul class="horizontal-tags">
          {{#each tagObjects }}
          <li class="tag removable_tag">
            <div class="name">{{tag}}<i class="fa fa-times"></i></div>
          </li>
          {{/each}}
          <li class="tag edittag">
            <input type="text" id="edittag-input" value="" /><i class="fa fa-plus"></i>
          </li>
        </ul>
      </div>
    </div>
  {{/with}}
</template>
```

**Javascript**  
Next, we want to set up some controllers to return data, implement some data input, and so forth. 
<!-- language: lang-js -->
```
// you will need to set the selectedPostId session variable 
// somewhere else in your application
Template.blogPost.selectedPost = function(){
  return Posts.findOne({_id: Session.get('selectedPostId')});
}

// next, we use the _.map() function to read the array from our record
// and convert it into an array of objects that Handlebars/Spacebars can parse
Template.blogPost.tagObjects = function () {
    var post_id = this._id;
    return _.map(this.tags || [], function (tag) {
        return {post_id: post_id, tag: tag};
    });
};

// then we wire up click events 
Template.blogPost.events({
    'click .fa-plus': function (evt, tmpl) {
        Posts.update(this._id, {$addToSet: {tags: value}});
    },
    'click .fa-times': function (evt) {
        Posts.update({_id: this._id}, {$pull: {tags: this.tag}});
    }
});
```

**Styling**  
Lastly, we want to define some different Views for phone, tablet, and desktops; and some basic UI styling depending on user input.  This example uses the Less precompiler, although the syntax should be roughly the same for Sass and Stylus.
<!-- language: lang-css -->
```
// default desktop view
.fa-plus:hover{
  cursor: pointer;
}
.fa-times:hover{
  cursor: pointer;
}
// landscape orientation view for tablets
@media only screen and (min-width: 768px) {
  .blogPost{
     padding: 20px;
  }
}
// portrait orientation view for tablets
@media only screen and (max-width: 768px) {
  .blogPost{
     padding: 0px;
       border: 0px;
  }
}
// phone view
@media only screen and (max-width: 480px) {
  blogPost{
   .panel-footer{
       display: none;
    }
  }
}
```


## Alerts and Errors
Alerts and errors are nearly the simplest of all Meteor component patterns. They're so simple, in fact, that they barely register as a pattern in of themselves. Instead of adding FlashAlert modules or patterns, all you really need to do is style a Handlebar template appropriate, add a helper, and wire it up to a reactive Session variable.

**Prerequisites**  
The following code requires the LESS precompiler and Bootstrap-3, respectively. You will need to run the following commands at the command prompt to get them to work.

```
meteor add less
meteor add ian:bootstrap-3 
```

**Document Object Model: Define Alert Object**
Start by adding some elements to your document object model. In this case, we want to create a div element for our alert, that's wired up to two Handlebar helpers.

<!-- language: lang-html -->

```
<template name="postsPage">
  <div id="postsPage" class="page">
    <div id="postsPageAlert" class="{{alertColor}}">{{alertMessage}}</div>
    <div class="postsList">
      <!-- other code you can ignore in this example -->
    </div>
    <div id="triggerAlertButton" class="btn btn-default">
  </div>
</template>
```

**Javascript: Define Template Helpers**
Then we want to wire up some controllers that will populate the object model with data. We do so with two reactive session variables, and two handlebar helpers.
<!-- language: lang-js -->
```
Session.setDefault('alertLevel', false);
Session.setDefault('alertMessage', "");

Template.postsPage.alertColor = function(){
 if(Session.get('alertLevel') == "Success"){
  return "alert alert-success";
 }else if(Session.get('alertLevel') == "Info"){
  return "alert alert-info";
 }else if(Session.get('alertLevel') == "Warning"){
  return "alert alert-warning";
 }else if(Session.get('alertLevel') == "Danger"){
  return "alert alert-danger";
 }else{
  return "alert alert-hidden"
 }
}

Template.postsPage.alertMessage = function(){
  return Session.get('alertMessage');
}
```

**Styling: Define DOM Visibility** 
Then we want to go back to our CSS, and define two views of the postsPage element. In the first View, we display all of the contents in our object model. In the second view, only some of the contents of our object model are displayed.
<!-- language: lang-css -->
```
#postsPage{
  .alert{
    display: block;
  }
  .alert-hidden{
    display: none;
  }
}
```

**Javascript: Triggering the Alert**  
Lastly, we go back to our controllers, and we define an event controller, which will trigger our alert when clicked.
<!-- language: lang-js -->
```
Template.postsPage.events({
  'click #triggerAlertButton':function(){
    Session.set('alertLevel', 'Success');
    Session.set('alertMessage', 'You successfully read this important alert message.');
  }
});
```
And that's all there is to it! Super simple, right? You can now set the ``alertLevel`` and ``alertMessage`` session variables anywhere in your codebase, and your application will reactively show alerts and error messages! :)


## Tabbed Workflow
**Document Object Model**  
Start by creating your tabs and panes in your Object Model...
<!-- language: lang-html -->
```
<template name="samplePage">
  <div id="samplePage" class="page">
    <ul class="nav nav-tabs">
      <li id="firstPanelTab"><a href="#firstPanel">First</a></li>
      <li id="secondPanelTab"><a href="#secondPanel">Second</a></li>
    </ul>

    <div id="firstPanel" class="{{firstPanelVisibility}}">
      {{> firstPanel }}
    </div>
    <div id="secondPanel" class="{{secondPanelVisibility}}">
      {{> secondPanel }}
    </div>
  </div>
</template>
```

**Javascript**  
<!-- language: lang-js -->
```
// this variable controls which tab is displayed and associated application state
Session.setDefault('selectedPanel', 1);

Template.name.helpers({
  firstPanelVisibility: function (){
    if(Session.get('selectedPanel') === 1){
      return "visible";
    }else{
      return "hidden";
    }
  },
  secondPanelVisibility: function (){
    if(Session.get('selectedPanel') === 2){
      return "visible";
    }else{
      return "hidden";
    }
  },
  thirdPanelVisibility: function (){
    if(Session.get('selectedPanel') === 3){
      return "visible";
    }else{
      return "hidden";
    }
  },
  firstPanelActive: function (){
    if(Session.get('selectedPanel') === 1){
      return "active panel-tab";
    }else{
      return "panel-tab";
    }
  },
  secondPanelActive: function (){
    if(Session.get('selectedPanel') === 2){
      return "active panel-tab";
    }else{
      return "panel-tab";
    }
  },
  thirdPanelActive: function (){
    if(Session.get('selectedPanel') === 3){
      return "active panel-tab";
    }else{
      return "panel-tab";
    }
  }
});
```

**Styling**  
<!-- language: lang-css -->
```
.visible {
  display: block;
  visibility: visible;
}
.hidden {
  display: none;
  visibility: hidden;
}
```

**Active Tab**
For added effect, you can extend this pattern by injecting classes to indicate the active tab.
<!-- language: lang-html -->
```
<li id="firstPanelTab" class="{{firstPanelActive}}"><a href="#firstPanel">First</a></li>
<li id="secondPanelTab" class="{{secondPanelActive}}"><a href="#secondPanel">Second</a></li>
```
<!-- language: lang-js -->
```
Template.firstPanel.helpers({
  firstPanelActive: function (){
    if(Session.get('selectedPanel') === 1){
      return "active";
    }else{
      return "";
    }
  },
  secondPanelActive: function (){
    if(Session.get('selectedPanel') === 2){
      return "active";
    }else{
      return "";
    }
  },
});

```


