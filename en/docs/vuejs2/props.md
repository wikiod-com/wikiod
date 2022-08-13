---
title: "Props"
slug: "props"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

This small example shows you how to pass props to a component.

More information: https://vuejs.org/v2/guide/components.html#Passing-Data-with-Props

## VueJS 2 props example
**JavaScript:**        

    Vue.component('props-component', {
        template: '#props-component-template',
        // array of all props
        props: ['myprop', 'calcprop']
    });


    new Vue({
        el: '#app'
    });

**HTML:**

    <div id="app">
        <template id="props-component-template">
            <div>Props: {{myprop}} - {{calcprop}}</div>
        </template>

        <props-component 
            myprop="prop_value" 
            :calcprop="Math.random()">
        </props-component>
    </div>


