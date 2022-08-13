---
title: "Implementing Sections"
slug: "implementing-sections"
draft: false
images: []
weight: 9861
type: docs
toc: true
---

Sections are a feature that allow store owners to add, edit, remove and easily reorder content on a page. There are 2 types of sections: dynamic and fixed. Dynamic sections are able to be reordered in their entirety with other sections on the page (homepage). Fixed sections cannot be reordered, but the content within them can still be added to, edited, removed and reordered.

# Things to remember:
- The schema of a section is scoped to that particular section. This means you can give your variables relatively simple names without worrying about them clashing with variable names from other sections on the same page.

# More Resources:
- https://help.shopify.com/themes/development/theme-editor/sections
- https://help.shopify.com/themes/development/theme-editor/settings-schema

## Including a Section on a Page
Including a Static Section on a page is similar to including a snippet. The only difference is instead of using the word `include` you use the word `section`.

    {% section 'section-file-name' %}

Using this tag will "hardcode" the section into your theme at the place where you put the code and cannot be moved or deleted from the page.

To use Dynamic Sections, you need to use the `preset` control at the bottom of the schema settings for that section.

    {% schema %}
      {
        "presets": [
          {
            "category": "Custom Content",
            "name": "Text",
            "settings": {
              "heading": "Hello World"
            },
            "blocks": [
              {
                "type": "text",
                "settings": {
                  "content": "Once upon a time..."
                }
              }
            ]
          }
        ]
      }
    {% endschema %}

By using presets, this will enable you to move and add your section around your page as you require. It also allows you to fill in the section with dummy content so that you can view exactly how the section will look on your page without actually having to add it to the page.

## In File Schema
For sections, instead of utilizing one big JSON file (like `settings_schema.json`, they instead keep their schema within the file of the section that is using it. To do this `{% schema %}` `{% endschema %}` tags are used and JSON is placed between them. From there, the format is similar to `settings_schema.json`.

    {% schema %}
      {
        "name": "Header Banner",
        "settings": [
          {
            "type": "checkbox",
            "id": "banner_enable",
            "label": "Enable Banner",
            "default": false
          },
          {
            "type": "color",
            "id": "banner_color",
            "label": "Banner Background Color",
            "default": "#000000"
          },
          {
            "type": "color",
            "id": "text_color",
            "label": "Banner Text Color",
            "default": "#ffffff"
          },
          {
            "type": "text",
            "id": "banner_text",
            "label": "Banner Text",
            "default": "Welcome to my Section"
          }
        ]
      }
    {% endschema %}

To use this in your HTML/Liquid code you would pull from `section.settings`. Something like:

    {% if section.settings.banner_enable %}
      <div class="banner">
        <p>{{ section.settings.banner_text }}</p>
      </div>
    {% endif %}

## Using Blocks for Recurring Elements
One of the most useful things that came along with sections is **blocks**. Blocks are basically a blueprint for something that can be created an unlimited amount of times. One of the best examples is the slides of a slider. A block is a top level item in the schema, meaning it is alongside things like `name` and `settings`, but not within them. Within blocks are basically mini schemas.

    {% schema %}
      {
        "name": "Slider",
        "max_blocks": 6,
        "blocks": [
          {
            "type": "slide",
            "name": "Slide",
            "limit": 4,
            "settings": [
              {
                "id": "image",
                "type": "image_picker",
                "label": "Image"
              }
            ]
          }
        ]
      }
    {% endschema %}

Notice the Slide also has a type of `slide`. You can give your blocks their own type that is not one of the Shopify provided types.

To use this you would just loop over each block in your HTML/Liquid code.

    <div class="slider">
      {% for block in section.blocks %}
        <img src="{{ block.settings.image | img_url: 'master' }}" alt="" />
      {% endfor %}
    </div>

## Using JavaScript and Stylesheets in Sections
Each section can can contain JavaScript and Stylesheets, these two languages are used within liquid tags: `{% javascript %}{% endjavascript %}` & `{% stylesheet %}{% endstylesheet %}`.

When placing code inside of these two tags, Shopify compiles each piece into `shopify_compiled.js` & `shopify_compiled.css`. This allows for greater readability within each section's code as you don't have to search through a long document to find each piece.

If you don't want to use CSS, you can alternatively use SCSS by using `{% stylesheet 'scss' %}` instead.

There are downfalls to using these, though.
Liquid cannot be used in any of them, just as it can't be used in normal `*.js`, `*.css`, `*.scss` files without the `.liquid` extension.

Also, when using the SCSS stylesheet tag, this isn't a global stylesheet, so any variables defined within your theme's `.scss` document won't be accessible and will need to be redefined.

**Usage Examples**

**JavaScript**

    {% javascript %}
        $( "p" ).click(function() {
          $( this ).slideUp();
        });
    {% endjavascript %}

**CSS**

    {% stylesheet %}
        .container {
            width: 100%;
        }
        .container p {
            color: #ff0000;
        }
    {% endstylesheet %}

**SASS**

    {% stylesheet 'scss' %}
        $red: #ff0000;
        .container {
            width: 100%;
            p {
                color: $red;
            }
        }
    {% endstylesheet %}

