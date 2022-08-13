---
title: "Vue single file components"
slug: "vue-single-file-components"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

Describe how to create single file components in a .vue file.

Specially the design decisions that can be made.

## Sample .vue component file
<!-- component.vue -->

    <template>
      <div class="nice">Component {{title}}</div>
    </template>
    
    <script>
    export default {
        data() {
            return {
                title: "awesome!"
            };
        }
    }
    </script>
    
    <style>
    .nice {
        background-color: red;
        font-size: 48px;
    }
    </style>

