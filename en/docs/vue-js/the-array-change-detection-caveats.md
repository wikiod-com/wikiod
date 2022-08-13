---
title: "The array change detection caveats"
slug: "the-array-change-detection-caveats"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

When you try to set a value of an item at a particular index of an array initialized in the data option, vue can't detect the change and does not trigger an update to the state. In order to overcome this caveat you should either use vue's Vue.$set or  use Array.prototype.splice method

## For nested array
If yoi have nested array, the following can be done

    new Vue({
        el: '#app',
        data:{
            myArr : [
                ['apple', 'banana'],
                ['grapes', 'orange']
            ]
        },
        methods:{
            changeArrayItem: function(){
                this.$set(this.myArr[1], 1, 'strawberry');
            }
        }
    })
    
  
Here is the [link to the jsfiddle][1]


  [1]: https://jsfiddle.net/r_vamsi_krishna/4gwex560/

## Array of objects containing arrays
    new Vue({
        el: '#app',
        data:{
            myArr : [
                {
                    name: 'object-1',
                    nestedArr: ['apple', 'banana']
                },
                {
                    name: 'object-2',
                    nestedArr: ['grapes', 'orange']
                }
            ]
        },
        methods:{
            changeArrayItem: function(){
                this.$set(this.myArr[1].nestedArr, 1, 'strawberry');
            }
        }
    }) 

Here is the [link to the fiddle][1]


  [1]: https://jsfiddle.net/r_vamsi_krishna/4gwex560/1/

## Using Vue.$set
In your method or any lifecycle hook that changes the array item at particuar index

    new Vue({
        el: '#app',
        data:{
            myArr : ['apple', 'orange', 'banana', 'grapes']
        },
        methods:{
            changeArrayItem: function(){
                //this will not work
                //myArr[2] = 'strawberry';
                
                //Vue.$set(array, index, newValue)
                this.$set(this.myArr, 2, 'strawberry');
            }
        }
    }) 

Here is the [link to the fiddle][1]


  [1]: https://jsfiddle.net/r_vamsi_krishna/4gwex560/2/

## Using Array.prototype.splice
You can perform the same change instead of using `Vue.$set` by using the Array prototype's `splice()`

    new Vue({
        el: '#app',
        data:{
            myArr : ['apple', 'orange', 'banana', 'grapes']
        },
        methods:{
            changeArrayItem: function(){
                //this will not work
                //myArr[2] = 'strawberry';
                
                //Array.splice(index, 1, newValue)
                this.myArr.splice(2, 1, 'strawberry');
            }
        }
    }) 


