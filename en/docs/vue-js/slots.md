---
title: "Slots"
slug: "slots"
draft: false
images: []
weight: 9805
type: docs
toc: true
---

Important! Slots after render don't guarantee order for positions for slots. Slot, which was the first, may have a different position after render.

## Using Named Slots
Named slots work similarly to single slots but instead allow you to distribute content to different regions within your child component template.

Take the `page` component from the previous example but modify it's template so it is as follows:

    <html>
        <head>
            <title>Page Title</title>
        </head>
        <body>
            <aside>
                <slot name="sidebar"></slot>
            </aside>
            <main>
                <slot name="content"></slot>
            </main>
        </body>
    </html>

When using the `page` component we can now determine where content is placed via the `slot` attribute:

    <page>
        <p slot="sidebar">This is sidebar content.</p>
        <article slot="content"></article>
    </page>

The resulting page will be:

    <html>
        <head>
            <title>Page Title</title>
        </head>
        <body>
            <aside>
                <p>This is sidebar content.</p>
            </aside>
            <main>
                <article></article>
            </main>
        </body>
    </html>

If a `slot` is defined without a `name` attribute then any content which is placed within component tags not specifying a `slot` attribute will be placed into that slot.

See the [multi insertion](https://vuejs.org/guide/components.html#Named-Slots) example on the Vue.js official docs.

## Using Single Slots
Single slots are used when a child component only defines one `slot` within its template. The `page` component above uses a single slot to distribute content.

An example of the `page` component's template using a single slot is below:

    <html>
        <head>
            <title>Page Title</title>
        </head>
        <body>
            <slot>
                This will only be displayed if there is no content
                to be distributed.
            </slot>
        </body>
    </html>

To illustrate how the slot works we can set up a page as follows.

    <page>
        <p>This content will be displayed within the page component</p>
    </page>

The end result will be:

    <html>
        <head>
            <title>Page Title</title>
        </head>
        <body>
            <p>This content will be displayed within the page component</p>
        </body>
    </html>
    
If we didn't put anything between the `page` tags an instead had `<page></page>` we would instead yield the following result since there is default content between the `slot` tags in the `page` component template.

    <html>
        <head>
            <title>Page Title</title>
        </head>
        <body>
            This will only be displayed if there is no content
            to be distributed.
        </body>
    </html>

## What are slots?
Slots offer a convenient way of distributing content from a parent component to a child component. This content can be anything from text, HTML or even other components.

It can be helpful sometimes to think of slots as a means of injecting content directly into a child component's template.

Slots are especially useful when the component composition underneath the parent component isn't always the same.

Take the following example where we have a `page` component. The content of the page could change based on whether that page displays e.g. an article, blog post or form.

**Article**

    <page>
        <article></article>
        <comments></comments>
    </page>

**Blog Post**

    <page>
        <blog-post></blog-post>
        <comments></comments>
    </page>

**Form**

    <page>
        <form></form>
    </page>

Notice how the content of the `page` component can change. If we didn't use slots this would be more difficult as the inner part of the template would be fixed.

**Remember:** *"Everything in the parent template is compiled in parent scope; everything in the child template is compiled in child scope."*

## Using Slots in Vue JSX with 'babel-plugin-transform-vue-jsx'
If you're Using VueJS2 and like to use JSX along with it. In this case,to use the slot, the solution with example is below.We have to use `this.$slots.default` It's almost like `this.props.children` in React JS.

**Component.js :**
    
    export default {
        render(h) {  //eslint-disable-line
            return (
                <li>
                     { this.$slots.default }
                </li>
            );
        }
    };

**ParentComponent.js**
    
    import Component from './Component';

    export default {
        render(h) {  //eslint-disable-line
            return (
                <ul>
                     <Component>
                         Hello World
                     </Component>
                </ul>
            );
        }
    };


