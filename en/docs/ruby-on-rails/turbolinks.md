---
title: "Turbolinks"
slug: "turbolinks"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

Turbolinks is a javascript library that makes navigating your web application faster. When you follow a link, Turbolinks automatically fetches the page, swaps in its \<body>, and merges its \<head>, all without incurring the cost of a full page load.

As a rails developer, you will likely interact minimally with turbolinks during your development. It is, however, an important library to be familiar with because it can be the cause of some hard-to-find bugs. 

# Key takeaways:

 - Bind to the `turbolinks:load` event instead of the `document.ready` event
 - Use the `data-turbolinks-false` attribute to disable turbolink functionality on a per-link basis.
 - Use the `data-turbolinks-permanent` attribute to persist elements across page loads and to avoid cache-related bugs.

For a more in-depth treatment of turbolinks, visit the [official github repository][1]. 

Credit for much of this documentation goes to the folks who drafted the turbolinks documentation on the github repository. 


  [1]: https://github.com/turbolinks/turbolinks#persisting-elements-across-page-loads

## Binding to turbolink's concept of a page load
With turbolinks, the traditional approach to using:

    $(document).ready(function() {
      // awesome code
    });

won't work. While using turbolinks, the `$(document).ready()` event will only fire once: on the initial page load. From that point on, whenever a user clicks a link on your website, turbolinks will intercept the link click event and make an ajax request to replace the \<body> tag and to merge the \<head> tags. The whole process triggers the notion of a "visit" in turbolinks land. Therefore, instead of using the traditional `document.ready()` syntax above, you'll have to bind to turbolink's visit event like so:

    // pure js
    document.addEventListener("turbolinks:load", function() {
      // awesome code
    });

    // jQuery
    $(document).on('turbolinks:load', function() {
      // your code
    });


## Disable turbolinks on specific links
It is very easy to disable turbolinks on specific links. According to [the official turbolinks documentation][1]:

  > Turbolinks can be disabled on a per-link basis by annotating a link or any of its ancestors with data-turbolinks="false".

# Examples:

    // disables turbolinks for this one link
    <a href="/" data-turbolinks="false">Disabled</a>

    // disables turbolinks for all links nested within the div tag
    <div data-turbolinks="false">
      <a href="/">I'm disabled</a>
      <a href="/">I'm also disabled</a>
    </div>

    // re-enable specific link when ancestor has disabled turbolinks
    <div data-turbolinks="false">
      <a href="/">I'm disabled</a>
      <a href="/" data-turbolinks="true">I'm re-enabled</a>
    </div>
    
  [1]: http://github.com/turbolinks/turbolinks

## Understanding Application Visits
Application visits are initiated by clicking a Turbolinks-enabled link, or programmatically by calling 

    Turbolinks.visit(location)

By default, the visit function uses the 'advance' action. More understandably, the default behavior for the visit function is to advance to the page indicated by the "location" parameter. Whenever a page is visited, turbolinks pushes a new entry onto the browser's history using `history.pushState`. The history is important because turbolinks will try to use the history to load pages from cache whenever possible. This allows for extremely fast page rendering for frequently visited pages.  

However, if you want to visit a location without pushing any history onto the stack, you can use the 'replace' action on the visit function like so:

    // using links
    <a href="/edit" data-turbolinks-action="replace">Edit</a>

    // programatically
    Turbolinks.visit("/edit", { action: "replace" })

This will replace the top of the history stack with the new page so that the total number of items on the stack remains unchanged. 

There is also a "restore" action that aids in [restoration vists][1], the visits that occur as a result of the user clicking the forward button or back button on their browser. Turbolinks handles these types of events internally and recommends that users don't manually tamper with the default behavior.


  [1]: https://github.com/turbolinks/turbolinks#restoration-visits

## Cancelling visits before they begin
Turbolinks provides an event listener that can be used to stop visits from occurring. Listen to the `turbolinks:before-visit` event to be notified when a visit is about to commence. 

In the event handler, you can use:

    // pure javascript
    event.data.url 

or 

    // jQuery
    $event.originalEvent.data.url

to retrieve the location of the visit. The visit can then be cancelled by calling:
 
    event.preventDefault()

# NOTE:
According to the [official turbolinks docs][1]:

  > Restoration visits cannot be canceled and do not fire turbolinks:before-visit.


  [1]: https://github.com/turbolinks/turbolinks#canceling-visits-before-they-start

## Persisting elements across page loads
Consider the following situation: Imagine that you are the developer of a social media website that allows users to be friends with other users and that employs turbolinks to make page loading faster. In the top right of every page on the site, there is a number indicating the total number of friends that a user currently has. Imagine you are using your site and that you have 3 friends. Whenever a new friend is added, you have some javascript that runs which updates the friend counter. Imagine that you just added a new friend and that your javascript ran properly and updated the friend count in the top right of the page to now render 4. Now, imagine that you click the browser's back button. When the page loads, you notice that the friend counter says 3 even though you have four friends.

This is a relatively common problem and one that turbolinks has provided a solution for. The reason the problem occurs is because turbolinks automatically loads pages from the cache when a user clicks the back button. The cached page won't always be updated with the database.

To solve this issue, imagine that you render the friend count inside a \<div> tag with an id of "friend-count":

    <div id="friend-count" data-turbolinks-permanent>3 friends</div>

By adding the `data-turbolinks-permanent` attribute, you're telling turbolinks to persist certain elements across page loads. The [official docs say][1]:

  > Designate permanent elements by giving them an HTML id and annotating them with data-turbolinks-permanent. Before each render, Turbolinks matches all permanent elements by id and transfers them from the original page to the new page, preserving their data and event listeners.

  [1]: https://github.com/turbolinks/turbolinks#persisting-elements-across-page-loads

