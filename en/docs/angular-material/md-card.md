---
title: "md-card"
slug: "md-card"
draft: false
images: []
weight: 9874
type: docs
toc: true
---

This topic is about how to create a `<md-card>`, which you can use for blog posts and other things.

API Documentation: [mdCard](https://material.angularjs.org/latest/api/directive/mdCard)

## Basic card with header
    <md-card>
        <md-card-header>
            <md-card-header-text>
                <span class="md-title">This will be the title</span>
                <span class="md-subhead">Here goes the (smaller, lighter) sub-header</span>
            </md-card-header-text>
        </md-card-header>
        <md-card-content>
            <p>
            Your content goes here!
            </p>
        </md-card-content>
    </md-card>

## Card with actions
If you want your card to include buttons, use the `md-card-actions` directive. Buttons can also be formatted differently for icon-only buttons. Search for icons at [here][1] if you're using Google's Material Icons.

    <md-card>
        <!--header-->
        <md-card-content>
            <p>
            Your content goes here!
            </p>
        </md-card-content>
        <md-card-actions>
            <md-button>Save</md-button>
            <md-button>Cancel</md-button>
            <md-card-icon-actions>
              <md-button aria-label="icon" class="md-icon-button">
                    <md-icon>help</md-icon>
              </md-button>
            </md-card-icon-actions>
        </md-card-actions>
    </md-card>


  [1]: https://design.google.com/icons

## Card with Avatar and image
If you want an avatar to appear on the card, use the `<md-card-avatar>` directive, which must be placed within the `<md-card-header>` directive. The `<md-card-avatar>` directive accepts an `<img />` tag.

Optional: `.md-user-avatar`, which makes the `<img />` tag have a circle look.

**index.html**:
<!-- language: lang-html -->    
    <md-card>
        <md-card-header>
            <!--Avatar-->
            <md-card-avatar>
                <img src="/path/to/avatar.svg" class="md-user-avatar"/>
            </md-card-avatar>
            <!--Header text-->
            <md-card-header-text>
                <span class="md-title">Lorem</span>
                <span class="md-subhead">Ipsum</span>
            </md-card-header-text>
        </md-card-header>
        <!--Card image-->
        <img src="/path/to/cardimage.svg" class="md-card-image" alt="Card Image">
        <!--Card title-->
        <md-card-title>
            <md-card-title-text>
                <span class="md-headline">Card header</span>
            </md-card-title-text>
        </md-card-title>
        <md-card-content>
            <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus posuere et risus sed finibus. Nunc vestibulum sagittis enim ut sagittis.</p>
        </md-card-content>
    </md-card>

