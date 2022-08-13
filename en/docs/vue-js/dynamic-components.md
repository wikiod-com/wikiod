---
title: "Dynamic Components"
slug: "dynamic-components"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

`<component>` is a reserved component element, don't be confused with components instance.

`v-bind` is a directive. Directives are prefixed with *v-* to indicate that they are special attributes provided by Vue.






## Simple Dynamic Components Example
Dynamically switch beetween multiple [components][1] using `<component>` element and pass data to *v-bind:is* attribute:

## **Javascript:** ##

    new Vue({
      el: '#app',
      data: {
        currentPage: 'home'
      },
      components: {
        home: {
          template: "<p>Home</p>"
        },
        about: {
          template: "<p>About</p>"
        },
        contact: {
          template: "<p>Contact</p>"
        }
      }
    })

## **HTML:** ##

    <div id="app">
       <component v-bind:is="currentPage">
           <!-- component changes when currentPage changes! -->
           <!-- output: Home -->
       </component>
    </div>


## **Snippet:** ## 

[Live Demo][2]


  [1]: https://www.wikiod.com/vue-js/components
  [2]: https://jsfiddle.net/elasri/9soyw9ca/

## Pages Navigation with keep-alive
Sometimes you want to keep the switched-out components in memory, to make that happen, you should use `<keep-alive>` element:

## **Javascript:** ##

    new Vue({
      el: '#app',
      data: {
        currentPage: 'home',
      },
      methods: {
         switchTo: function(page) {
                this.currentPage = page;
         }
      },
      components: {
        home: {
          template: `<div>
                     <h2>Home</h2>
                     <p>{{ homeData }}</p>
                     </div>`,
          data: function() {
             return {
                homeData: 'My about data'    
             }
           }
         },
        about: {
          template: `<div>
                     <h2>About</h2>
                     <p>{{ aboutData }}</p>
                     </div>`,
          data: function() {
             return {
                aboutData: 'My about data'
             }
          }
        },
        contact: {
          template: `<div>
                 <h2>Contact</h2>
                 <form method="POST" @submit.prevent>
                 <label>Your Name:</label>
                 <input type="text" v-model="contactData.name" >
                 <label>You message: </label>
                 <textarea v-model="contactData.message"></textarea>
                 <button type="submit">Send</button>
                 </form>
                 </div>`,
           data: function() {
              return {
                contactData: { name:'', message:'' }   
             }
          }
        }
      }
    })

## **HTML:** ##

    <div id="app">
      <div class="navigation">
        <ul>
          <li><a href="#home" @click="switchTo('home')">Home</a></li>
          <li><a href="#about" @click="switchTo('about')">About</a></li>
          <li><a href="#contact" @click="switchTo('contact')">Contact</a></li>
        </ul>
      </div>
    
      <div class="pages">
        <keep-alive>
          <component :is="currentPage"></component>
        </keep-alive>
      </div>
    </div>

## **CSS:** ##

    .navigation {
      margin: 10px 0;
    }
    
    .navigation ul {
      margin: 0;
      padding: 0;
    }
    
    .navigation ul li {
      display: inline-block;
      margin-right: 20px;
    }
    
    input, label, button {
      display: block
    }
    
    input, textarea {
      margin-bottom: 10px;
    }

## **Snippet:** ##

[Live Demo][1]


  [1]: https://jsfiddle.net/elasri/hy99p7p6/

