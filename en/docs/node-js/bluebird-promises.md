---
title: "Bluebird Promises"
slug: "bluebird-promises"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Converting nodeback library to Promises
    const Promise = require('bluebird'),
          fs = require('fs')
    
    Promise.promisifyAll(fs)
    
    // now you can use promise based methods on 'fs' with the Async suffix
    fs.readFileAsync('file.txt').then(contents => {
      console.log(contents)
    }).catch(err => {
      console.error('error reading', err)
    })

## Functional Promises
Example of map:

    Promise.resolve([ 1, 2, 3 ]).map(el => {
       return Promise.resolve(el * el) // return some async operation in real world
    })

Example of filter:

    Promise.resolve([ 1, 2, 3 ]).filter(el => {
      return Promise.resolve(el % 2 === 0) // return some async operation in real world
    }).then(console.log)

Example of reduce:

    Promise.resolve([ 1, 2, 3 ]).reduce((prev, curr) => {
      return Promise.resolve(prev + curr) // return some async operation in real world
    }).then(console.log)

## Coroutines (Generators)
    const promiseReturningFunction = Promise.coroutine(function* (file) {
      const data = yield fs.readFileAsync(file) // this returns a Promise and resolves to the file contents
    
      return data.toString().toUpperCase()
    })
    
    promiseReturningFunction('file.txt').then(console.log)

## Automatic Resource Disposal (Promise.using)
    function somethingThatReturnsADisposableResource() {
      return getSomeResourceAsync(...).disposer(resource => {
        resource.dispose()
      })
    }
    
    Promise.using(somethingThatReturnsADisposableResource(), resource => {
      // use the resource here, the disposer will automatically close it when Promise.using exits
    })

## Executing in series
    Promise.resolve([1, 2, 3])
      .mapSeries(el => Promise.resolve(el * el)) // in real world, use Promise returning async function
      .then(console.log)

