---
title: "Cards"
slug: "cards"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Cards are a vital part of the Materialize CSS framework, they provide a semantic and beautiful approach to showing content in an understandable way.

## Syntax
  - A basic card
  - Creating a horizontal card
  - Adding a halfway floating action button
  - Card reveal
  - Tabs in cards
  - Different card sizes
  - Card Panel

## Basic Card
    <div class="container">
      <div class="row">
        <div class="col s6">
          <div class="card">

            <div class="card-image"> <!-- Card Image -->
              <img src="image.png" alt="Image">
            </div>

            <div class="card-content"> <!-- Card Content -->
              <h3>This is a card</h3>
              <p class="flow-text">Cards are really cool for conveying information in a beautiful way.</p>
            </div>

            <div class="card-action"> <!-- Card Action -->
              <a href="#" class="btn orange">Read More</a>
            </div>

          </div>
        </div>
      </div>
    </div>

**Important:**
 - Use `.card` as the main surrounding element.
 - Surround images with `.card-image`
 - Surround content with `.card-content`
 - Surround action buttons with `.card-action`

Here's what it looks like:
![Card](http://i.imgur.com/49OzaMxl.png)

## Horizontal Card
    <div class="container">
      <div class="row">
        <div class="col s12">
          <div class="card horizontal">
            <div class="card-image"><img src="image.png"></div>
            <div class="card-stacked">
              <div class="card-content">
                <h3>This is a card</h3>
                <p class="flow-text">Cards are really cool for conveying information in a beautiful way.</p>
              </div>
              <div class="card-action"><a href="#" class="btn orange">Read More</a></div>
            </div>
          </div>
        </div>
      </div>
    </div>

**Important:**
 - Add `.horizontal` to the card element
 - Surround `.card-content` and `.card-action` with a div with `.card-stacked`

Here's what it looks like:
![Horizontal Card](http://i.imgur.com/ochkTHLl.png)

## Card With A Floating Action Button (FAB)
    <div class="container">
        <div class="row">
            <div class="col s5">
                <div class="card">
                    <div class="card-image">
                      <img src="image.png">
                      <a href="#" class="btn-floating halfway-fab orange btn-large"><i class="material-icons">add</i></a>
                    </div>
                    <div class="card-content">
                        <h3>This is a card</h3>
                        <p class="flow-text">Cards are really cool for conveying information in a beautiful way.</p>
                    </div>
                </div>
            </div>
        </div>
    </div>
**Important:**
 - Put an `a.btn-floating.halfway-fab` inside of the `.card-image` container.

Here's what it looks like:
![Card With FAB](http://i.imgur.com/L5loSC7l.png)

