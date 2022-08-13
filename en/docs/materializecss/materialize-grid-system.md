---
title: "Materialize Grid System"
slug: "materialize-grid-system"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

The [Materialize CSS framework](http://materializecss.com/) has a semantic, 12-column grid system that's incredibly easy to use. The grid helps you layout your page in an ordered, easy fashion.

## A Basic Features Strip
    <div class="container">
      <div class="row" id="features-strip">
        <div class="col s12 l4">
          <h1>Feature 1</h1>
          <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit. Doloribus, unde facere tempore vero? Id nostrum eligendi sapiente, ratione blanditiis atque dolores placeat eveniet autem ab aut esse, doloribus nam quas.</p>
        </div>
        <div class="col s12 l4">
          <h1>Feature 2</h1>
          <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit. Doloribus, unde facere tempore vero? Id nostrum eligendi sapiente, ratione blanditiis atque dolores placeat eveniet autem ab aut esse, doloribus nam quas.</p>
        </div>
        <div class="col s12 l4">
          <h1>Feature 3</h1>
          <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit. Doloribus, unde facere tempore vero? Id nostrum eligendi sapiente, ratione blanditiis atque dolores placeat eveniet autem ab aut esse, doloribus nam quas.</p>
        </div>
      </div>
    </div>

## Sidebar With Grid System
      <link rel="stylesheet"
            href="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.98.2/css/materialize.min.css">

    <style media="screen">
      .sidebar {
        height: 100vh;
      }
    </style>

    <div class="row">
      <div class="col s3 sidebar card">
        <div class="container">
          <h4>Sidebar</h4>
          <p>This is some sidebar content.</p>

          <ul>
            <li>List Item 1</li>
            <li>List Item 2</li>
            <li>List Item 3</li>
            <li>List Item 4</li>
          </ul>
        </div>
      </div>
      <div class="col s9">
        <div class="container">
          <div class="card-panel teal lighten-2 white-text">
            <h1>This is the main website</h1>
            <p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>
          </div>
          <div class="divider"></div>
          <div class="card-panel">
            <h3>Some more content</h3>
            <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin ante enim, sollicitudin non tristique ut, iaculis in eros. Donec et purus eget tellus suscipit sollicitudin. Suspendisse erat turpis, faucibus ut blandit non, bibendum non tortor. Nullam et neque sed felis facilisis finibus. Nulla dictum elit enim, in finibus dolor ultrices id. Proin suscipit ipsum imperdiet dui hendrerit tempus. Donec quis aliquam velit. In viverra enim orci, nec tristique mi mollis id. Etiam a luctus eros. Duis convallis sem arcu, quis posuere tellus accumsan vel. Nunc augue nunc, tempor vel ornare a, congue ut nibh. Donec viverra arcu dui, eu rutrum ipsum elementum at. Proin ac quam ac est tincidunt lobortis vitae quis risus. <br><br>
    Donec id mollis erat. Proin porttitor tincidunt dui, ut luctus turpis lacinia a. Donec in maximus ligula, sit amet elementum erat. Curabitur rhoncus eleifend euismod. In a fringilla tellus, sed placerat felis. Pellentesque dictum risus nibh, vel sollicitudin eros facilisis nec. Proin sed auctor est. Vestibulum iaculis, eros ac sagittis sagittis, dui magna tempor arcu, in imperdiet libero neque ut odio.</p>
          </div>
        </div>
      </div>
    </div>

Notice the use of `.col.s3` for the sidebar and `.col.s9` for the main content.

Here's what it looks like:
![Sidebar](http://i.imgur.com/lQqKRNI.png)

