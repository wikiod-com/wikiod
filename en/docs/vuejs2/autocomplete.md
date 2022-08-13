---
title: "Autocomplete"
slug: "autocomplete"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Autcomplete input
This is a small example of an autocomplete input.

**CSS**

    .autocomplete {
      position: relative;
    }
    .autocomplete-list {
      position: absolute;
      z-index: 2;
      top: 25px;

      min-width: 150px;
      margin: 0;
      padding: 0;

      list-style: none;

      border: 1px solid #eee;
      border-radius: 4px;
      background-color: #fff;
    }
    .autocomplete-list li {
      margin: 0;
      padding: 8px 15px;

      border-bottom: 1px solid #eee;
    }
    .autocomplete-list li:last-child {
      border-bottom: 0;
    }
    .autocomplete-list li:hover,
    .autocomplete-list li.active {
      background-color: #f5f5f5;
    }



**Javascript**

    const Autocomplete = Vue.component('autocomplete', {
      template: '#autocomplete-tpl',
      props: ['items'],
      data: function() {
        return {
          inputValue: '',
          searchMatch: [],
          selectedIndex: -1
        }
      },
      computed: {
        listToSearch() {
          if(typeof this.items !== 'undefined' && this.items.length > 0) {
            return this.items;
          } else {
            return [
              'input',
              'h1',
              'h2',
              'span',
              'div',
              'textarea',
              'margin',
              'padding',
              'background',
              'background-color',
              'background-size',
              'background-repeat'
            ]
          }
        }
      },
      watch: {
        inputValue(val) {
          this.searchMatch = [];
          if(this.inputValue !== '') {
            this.searchMatch = this.listToSearch.filter((el) => el.indexOf(val) >= 0);  
          }
          if (this.searchMatch.length === 1 && this.inputValue === this.searchMatch[0]) {
            this.searchMatch = [];
          }

        }
      },
      methods: {
        moveDown() {
          if(this.selectedIndex < this.searchMatch.length-1) {
            this.selectedIndex++;
          }
        },
        moveUp() {
          if(this.selectedIndex !== -1) {
            this.selectedIndex--;
          }
        },
        selectItem(index) {
          this.selectedIndex = index;
        },
        chooseItem() {
          if(this.selectedIndex !== -1) { 
            this.inputValue = this.searchMatch[this.selectedIndex];
            this.selectedIndex = -1;
          }
        }
      }
    });


    new Vue({
      el: '#app'
    });


**HTML**

    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="UTF-8">
        <title>Autocomplete</title>
        <script type="text/x-template" id="autocomplete-tpl">
      <div class="autocomplete">
        <input @keydown.13="chooseItem" @keydown.40="moveDown" @keydown.38="moveUp" v-model="inputValue" type="text">  


          <ul class="autocomplete-list" v-if="searchMatch.length > 0">
            <li :class="{active: selectedIndex === index}"v-for="(result, index) in searchMatch" @click="selectItem(index), chooseItem()">
              {{result}}
          </li>
          </ul>
          </div>
        </script>
      </head>
      <body>


        <div id="app">
          Own items:<br />
          <autocomplete :items="['all', 'auto', 'complete', 'items']"></autocomplete>
          <br /><br />
          Standard items:<br />
          <autocomplete></autocomplete>
        </div>
        <script src="https://cdnjs.cloudflare.com/ajax/libs/vue/2.3.3/vue.js"></script>

      </body>
    </html>

