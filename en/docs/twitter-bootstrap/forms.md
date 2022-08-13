---
title: "Forms"
slug: "forms"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Read-only and disabled inputs
Add the `readonly` attribute to prevent user input. A readonly field can't be edited

    <input class="form-control" type="text" placeholder="Readonly input here…" readonly>

Add the `disabled` attribute to disable an input field. A disbled field can't be edited either. The cursor changes to make it more noticeable.

    <input class="form-control" id="disabledInput" type="text" placeholder="Disabled input here..." disabled>

## Basic form
Form controls have some default styling without using any special classes.

However labels and controls can be wrapped in `.form-group` tags for optimum spacing.
 
    <form>
      <div class="form-group">
        <label for="input-email">Email address</label>
        <input type="email" class="form-control" id="input-email" placeholder="Email">
      </div>
      <div class="form-group">
        <label for="input-password">Password</label>
        <input type="password" class="form-control" id="input-password" placeholder="Password">
      </div>
      <button type="submit" class="btn btn-default">Submit</button>
    </form>

