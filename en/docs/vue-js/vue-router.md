---
title: "vue-router"
slug: "vue-router"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

vue-router is the officially supported routing library for vue.js.

## Syntax
  - `<router-link to="/path">Link Text</router-link> <!-- Creates a link to the route that matches the path  -->`

  - `<router-view></router-view> <!-- Outlet for the currently matched route. It's component will be rendered here. -->`

## Basic Routing
The easiest way to get up and running with vue-router is to use the version provided via CDN.

HTML:

    <script src="https://unpkg.com/vue/dist/vue.js"></script>
    <script src="https://unpkg.com/vue-router/dist/vue-router.js"></script>

    <div id="router-example">
        <router-link to="/foo">Link to Foo route</router-link>
        <router-view></router-view>
    </div>

JavaScript (ES2015):

    const Foo = { template: <div>This is the component for the Foo route</div> }

    const router = new VueRouter({
        routes: [
           { path: '/foo', component: Foo}
        ]
    })

    const routerExample = new Vue({
        router
    }).$mount('#router-example')
    

