---
title: "AJAX requests and binding"
slug: "ajax-requests-and-binding"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Sample AJAX request w/ binding
Page.html

 

    <div data-bind="foreach: blogs">
        <br />
        <span data-bind="text: entryPostedDate"></span>
        <br />
        <h3>
            <a data-bind="attr: { href: blogEntryLink }, text: title"></a>
        </h3>
        <br /><br />
        <span data-bind="html: body"></span>
        <br />
        <hr />
        <br />
    </div>

    <!--- include knockout and dependencies (Jquery) --->
    <script type="text/javascript" src="blog.js"></script>

blog.js

    function vm() {
        var self = this;
    
        // Properties
        self.blogs = ko.observableArray([]);
    
        // consists of entryPostedDate, blogEntryLink, title, body
        var blogApi = "/api/blog";
    
        // Load data
        $.getJSON(blogApi) 
            .success(function (data) {
                self.blogs(data);
            });
        }
    ko.applyBindings(new vm());

Note that JQuery was used (`$.getJSON(...)`) to perform the request.  vanilla JavaScript can perform the same, albeit with more code.

## "Loading section/notification" during AJAX request
Blog.html

    <div data-bind="visible: isLoading()">
        Loading...
    </div>
    
    <div data-bind="visible: !isLoading(), foreach: blogs">
        <br />
        <span data-bind="text: entryPostedDate"></span>
        <br />
        <h3>
            <a data-bind="attr: { href: blogEntryLink }, text: title"></a>
        </h3>
        <br /><br />
        <span data-bind="html: body"></span>
        <br />
        <hr />
        <br />
    </div>
    
    <!--- include knockout and dependencies (JQuery) --->
    <script type="text/javascript" src="blog.js"></script>

blog.js

    function vm() {
        var self = this;
    
        // Properties
        self.blogs = ko.observableArray([]);
        self.isLoading = ko.observable(true);
    
        // consists of entryPostedDate, blogEntryLink, title, body
        var blogApi = "/api/blog";
    
        // Load data
        $.getJSON(blogApi) 
            .success(function (data) {
                self.blogs(data);
            })
            .complete(function () {
                self.isLoading(false); // on complete, set loading to false, which will hide "Loading..." and show the content.
            });
    }
    ko.applyBindings(new vm());

Note that JQuery was used ($.getJSON(...)) to perform the request. vanilla JavaScript can perform the same, albeit with more code.

