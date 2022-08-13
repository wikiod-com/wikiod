---
title: "Alert"
slug: "alert"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

See more: http://getbootstrap.com/components/#alerts

## Alert basic example
    <div class="container">
      <h2>Alerts</h2>
      <div class="alert alert-success">
        <strong>Success!</strong>
      </div>
      <div class="alert alert-info">
        <strong>Info!</strong>
      </div>
      <div class="alert alert-warning">
        <strong>Warning!</strong> All foelds are required
      </div>
      <div class="alert alert-danger">
        The username is required and can't be empty
      </div>
    </div>

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/w7s9t.png

## Animated Alerts
The `.fade` and `.in` classes adds a fading effect when closing the alert message.

    <div class="alert alert-success fade in">
        <a href="#" class="close" data-dismiss="alert" aria-label="close">&times;</a>
        <strong>Success!</strong> This is a good example!
    </div>


## Alert Types


## Dismissible Alerts
To give an alert close functionality, all we need is to add `data-dismiss="alert"` to our close button. 

    <div class="alert alert-info alert-dismissible" role="alert">
        <button type="button" class="close" data-dismiss="alert" aria-label="Close">
            <span aria-hidden="true">&times;</span>
        </button>
        Sphinx of black quartz, judge my vow
    </div>

[![enter image description here][1]][1]

`.alert-dismissible` and `.close` classes are optional, only useful for styling. 


  [1]: http://i.stack.imgur.com/0NjkC.png

## Link color in Alerts

To quickly provide a matching color for links inside any alert, we can use the `.alert-link` utility class.

    <div class="alert alert-success">
        You have won! Click <a href="#" class="alert-link">here</a> to claim your prize ...
    </div>
    
    <div class="alert alert-info">
        You might want to check <a href="#"  class="alert-link">this</a> instead.
    </div>
    
    <div class="alert alert-warning">
        You are running out of coins. Buy more <a href="#" class="alert-link">here</a>.
    </div>
    
    <div class="alert alert-danger">
        Something went wrong. You can try <a href="#" class="alert-link">again</a> or ...
    </div>

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/o6ICA.png
  [2]: http://i.stack.imgur.com/NSF78.png

