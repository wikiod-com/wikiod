---
title: "Cascading Dropdown"
slug: "cascading-dropdown"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

How to build dropdown's that are dependent on each other.

## Car Models of Make
HTML (classes used are based on Semantic-UI)

    <div id="app" class="ui grid">
      <div class="row">
        <div class="column">
          <div class="ui label">Vechicle Make</div>
          <select class="ui dropdown" v-model="make" id="vehicle-makes">
            <option v-for="option in makes_options" v-bind:value="option.id">
              {{ option.text }}
            </option>
          </select>
        </div>
      </div>
      <div class="row">
        <div class="column">
          <div class="ui label">Vechicle Model</div>
          <select class="ui dropdown" id="vehicle-models" v-model="model">
            <option 
              v-for="option in model_options[make]" 
              :value="option.id" 
              :key="option.id"
            >
              {{ option.text }}
            </option>
          </select>
        </div>
      </div>
    </div>

Javascript 

    <script>
    var model_options = {
      1: [{ text: "Accord", id: 1 }, { text: "Civic", id: 2 }],
      2: [{ text: "Corolla", id: 3 }, { text: "Hi Ace", id: 4 }],
      3: [{ text: "Altima", id: 5 }, { text: "Zuke", id: 6 }],
      4: [{ text: "Alto", id: 7 }, { text: "Swift", id: 8 }]
    };
    
    var makes_options = [
      { text: "Honda", id: 1 },
      { text: "Toyota", id: 2 },
      { text: "Nissan", id: 3 },
      { text: "Suzuki", id: 4 }
    ];
    
    var vm_makes = new Vue({
      el: "#app",
      data: {
        make: null,
        model: null,
        makes_options: makes_options,
        model_options: model_options,
      },
      watch: {
        make: function(event) {
          $('#vehicle-models').dropdown('clear');
        }
      }
    });
    
    // Eveything works fine if I remove this...
    $('.ui.dropdown').dropdown();
    </script>

