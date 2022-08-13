---
title: "List group"
slug: "list-group"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

You should know how to use bootstrap [Buttons][1] and little information about [Contextual][2] classes.


  [1]: https://getbootstrap.com/css/#buttons
  [2]: https://getbootstrap.com/components/#panels-alternatives

## Basic example
    <ul class="list-group">
      <li class="list-group-item">Cras justo odio</li>
      <li class="list-group-item">Dapibus ac facilisis in</li>
      <li class="list-group-item">Morbi leo risus</li>
      <li class="list-group-item">Porta ac consectetur ac</li>
      <li class="list-group-item">Vestibulum at eros</li>
    </ul>

## Badges
    <ul class="list-group">
      <li class="list-group-item">
        <span class="badge">14</span>
        Cras justo odio
      </li>
    </ul>

## Linked Items
    <div class="list-group">
      <a href="#" class="list-group-item active">
        Cras justo odio
      </a>
      <a href="#" class="list-group-item">Dapibus ac facilisis in</a>
      <a href="#" class="list-group-item">Morbi leo risus</a>
      <a href="#" class="list-group-item">Porta ac consectetur ac</a>
      <a href="#" class="list-group-item">Vestibulum at eros</a>
    </div>

## Button items
    <div class="list-group">
      <button type="button" class="list-group-item">Cras justo odio</button>
      <button type="button" class="list-group-item">Dapibus ac facilisis in</button>
      <button type="button" class="list-group-item">Morbi leo risus</button>
      <button type="button" class="list-group-item">Porta ac consectetur ac</button>
      <button type="button" class="list-group-item">Vestibulum at eros</button>
    </div>

## Disabled Items
    <div class="list-group">
      <a href="#" class="list-group-item disabled">
        Cras justo odio
      </a>
      <a href="#" class="list-group-item">Dapibus ac facilisis in</a>
      <a href="#" class="list-group-item">Morbi leo risus</a>
      <a href="#" class="list-group-item">Porta ac consectetur ac</a>
      <a href="#" class="list-group-item">Vestibulum at eros</a>
    </div>

## Contextual classes
    <ul class="list-group">
      <li class="list-group-item list-group-item-success">Dapibus ac facilisis in</li>
      <li class="list-group-item list-group-item-info">Cras sit amet nibh libero</li>
      <li class="list-group-item list-group-item-warning">Porta ac consectetur ac</li>
      <li class="list-group-item list-group-item-danger">Vestibulum at eros</li>
    </ul>
    <div class="list-group">
      <a href="#" class="list-group-item list-group-item-success">Dapibus ac facilisis in</a>
      <a href="#" class="list-group-item list-group-item-info">Cras sit amet nibh libero</a>
      <a href="#" class="list-group-item list-group-item-warning">Porta ac consectetur ac</a>
      <a href="#" class="list-group-item list-group-item-danger">Vestibulum at eros</a>
    </div>

## Custom content
    <div class="list-group">
      <a href="#" class="list-group-item active">
        <h4 class="list-group-item-heading">List group item heading</h4>
        <p class="list-group-item-text">...</p>
      </a>
    </div>

