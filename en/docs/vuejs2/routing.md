---
title: "Routing"
slug: "routing"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

VueJS Routing with vue-router.

Documentation: https://router.vuejs.org/en/

## Hash Router
**JavaScript:**

    const MainPage = {
      template: '#mainpage-template'
    }
    const Page1 = {
      template: '#page1-template'
    }
    const Page2 = {
      template: '#page2-template'
    }
    const Page3 = {
      template: '#page3-template'
    }

    const router = new VueRouter({
      mode: 'hash',
      routes: [{
          path: '/',
          component: MainPage
        },
        {
          path: '/page1',
          component: Page1
        },
        {
          path: '/page2',
          component: Page2
        },
        {
          path: '/page3',
          component: Page3
        }
      ]
    })

    new Vue({
      router: router,
      el: '#app'
    });



**HTML:**

    <template id="mainpage-template">
      <div>
        mainpage
      </div>
    </template>
    <template id="page1-template">
      <div>
        Page 1
      </div>
    </template>
    <template id="page2-template">
      <div>
        Page 2
      </div>
    </template>
    <template id="page3-template">
      <div>
        Page 3
      </div>
    </template>
    
    
    <div id="app">
      <h1>Router</h1>
      <ul>
        <li><router-link to="/">mainpage</router-link></li>
        <li><router-link to="/page1">/page1</router-link></li>
        <li><router-link to="/page2">/page2</router-link></li>
        <router-link tag="li" to="/page3">/page3</router-link>
      </ul>
      
      <router-view></router-view>
    </div>

## different ways of defining routes

**route/index.js**
      
      import About from '@/components/About'

      const router = new Router({
         
        routes: [
         {
           path: '/',
           name: 'home',
           component: {template: "<div>Home</div>"}
         },

         {
           path: '/about',
           component: About
         },

         require('./work').default,
         
         // - NOT FOUND
         {path: '/404', component: {template: "<div>404 not found.</div>"}},
         {path: "/*", redirect: "/404"}
       ]
     });
     
     export default router

**route/work.js**
     
    import Work from '@/components/Work/Work';
    import Workitem from '@/components/Work/Workitem';

    export default { 
      path: '/work', 
      name: 'work', 
      component: Work, 
      children: [
        {path: '/all', component: {template: '<div>Some text</div>'}},
        {path: ':id', name: 'work.view', component: Workitem},
        {path: ':id/edit', name: 'work.edit', component: Workitemedit},
      ],
      meta: {requiresAuth: true}
    }






