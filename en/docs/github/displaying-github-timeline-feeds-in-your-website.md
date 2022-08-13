---
title: "Displaying GitHub timeline  feeds in your Website"
slug: "displaying-github-timeline--feeds-in-your-website"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Displaying GitHub timeline / feeds on your website

This document explains how to display your GitHub feeds/timeline on your website.

**Example:** A live example is available at:

> https://newtonjoshua.com

**GitHub timeline:**


GitHub provides the public timeline for any user in Atom format.

You can view your timeline at:

> https://github.com/{{GitHub_username}}.atom

refer:
https://developer.github.com/v3/activity/feeds


**Google Feed API:**

With the Feed API, you can download any public Atom, RSS, or Media RSS feed using only JavaScript, so you can mash up feeds with your content and other APIs with just a few lines of JavaScript. This makes it easy to quickly integrate feeds on your website.

refer:
https://developers.google.com/feed/v1/devguide

**Loading the JavaScript API:** To begin using the Feed API, include the following script in the header of your web page.

    <script type="text/javascript" src="https://www.google.com/jsapi"></script>

Next, load the Feed API with google.load(module, version, package).

    <script type="text/javascript">
      google.load("feeds", "1");
    </script>

**Specifying the feed URL:** You can call google.feeds.Feed() as follows:

    var feed = new google.feeds.Feed("https://github.com/{{GitHub_UserName}}.atom");

**Loading a feed:** .load(callback) downloads the feed specified in the constructor from Google's servers and calls the given callback when the download completes.

    <script type="text/javascript">
    
        function initialize() {
          feed.load(function(result) {
            if (!result.error) {
              var container = document.getElementById("feed");
              result.feed.entries.forEach(function (feed) {
                var feedTitle= feed.title; 
                var feedLink = feed.link;
                var feedDate = formatDate(feed.publishedDate);
                var feedContent = formatContent(feed.content);
    
               // display the feed in your website
              });
            }
          });
        }
        google.setOnLoadCallback(initialize);
    
        </script>

**Calling the onLoad handler:**
setOnLoadCallback(callback) is a static function that registers the specified handler function to be called once the page containing this call loads, where callback is a required function called when the containing document is loaded and the API is ready for use


    <script type="text/javascript">
        google.setOnLoadCallback(initialize);
     </script>

**Setting the number of feed entries:** .setNumEntries(num) sets the number of feed entries loaded by this feed to num. By default, the Feed class loads four entries.

    var feed = new google.feeds.Feed("https://github.com/{{GitHub_UserName}}.atom");
    feed.setNumEntries(500);


Now you can format and display your GitHub feeds/timeline on your website.


