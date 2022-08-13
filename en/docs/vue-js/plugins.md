---
title: "Plugins"
slug: "plugins"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

Vue plugins adds global functionality as, global methods, directives, transitions, filters, instance methods, objects and inject some component options using mixins

## Syntax
 - MyPlugin.install = function (Vue, options) {}

## Parameters
| Name| Description |
| --- | --- |
| Vue | Vue constructor, injected by Vue |
| options | Additional options if needed |

In most cases you will need to explicitly tell Vue to use a plugin

    // calls `MyPlugin.install(Vue)`
    Vue.use(MyPlugin)

To pass options 

    Vue.use(MyPlugin, { someOption: true })

## Simple logger


    //myLogger.js
    export default {
    
      install(Vue, options) {
         function log(type, title, text) {
           console.log(`[${type}] ${title} - ${text}`);
         }

         Vue.prototype.$log = {
           error(title, text) { log('danger', title, text) },
           success(title, text) { log('success', title, text) },
           log
         }
      }
    }

Before your main Vue instance tell to register your plugin


    //main.js
    import Logger from './path/to/myLogger';
    
    Vue.use(Logger);

    var vm = new Vue({
      el: '#app',
      template: '<App/>',
      components: { App }
    })

Now you can call `this.$log` on any child component 

    //myComponent.vue
    export default {
      data() {
        return {};
      },
      methods: {
        Save() {
          this.$log.success('Transaction saved!');
        }
      }
    }



