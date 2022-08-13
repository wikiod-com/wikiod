---
title: "Polyfill webpack template"
slug: "polyfill-webpack-template"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Parameters
|Files or packages|Command or configuration to modify|
|--------|-----|
|babel-polyfill|``npm i -save babel-polyfill``|
|karma.conf.js|``files: ['../../node_modules/babel-polyfill/dist/polyfill.js','./index.js'],``|
|webpack.base.conf.js|``app: ['babel-polyfill', './src/main.js']``|

The configurations described above, the example using a non-sstandardised function will work on "internet explorer" and ```npm test``` will pass.

## Usage of functions to polyfill (ex: find)
```
<template>
  <div class="hello">
    <p>{{ filtered() }}</p>
  </div>
</template>

<script>
export default {
  name: 'hello',
  data () {
    return {
      list: ['toto', 'titi', 'tata', 'tete']
    }
  },
  methods: {
    filtered () {
      return this.list.find((el) => el === 'tata')
    }
  }
}
</script> 
```


