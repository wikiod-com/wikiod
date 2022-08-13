---
title: "Filter in vuejs"
slug: "filter-in-vuejs"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Filter Functionality in Vuejs
    <!doctype html>

    <html>

    <head>
        <title>Page Title</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="initial-scale=1.0">
        <script src="vue.js"></script>
        <style>
            * {font-family: sans-serif;}
            #app {margin: 0 auto; width:500px;}
            button {
                background: #ccc;
                color: #fff;
                -webkit-appearance:none;
                background: #000;
                border: 0;
                padding: 10px;
                text-align: center;
                width: 49%;
            }
            .searchText{height: 25px;width: 386px;}
            .addItem {height: 30px;width: 226px; padding: 0 10px }
            button.active {
                background: #94d464;
                color: #fff;
            }
            .itemBox {margin: 0 0 10px 0;padding: 0;border: 1px dashed #000;}
            .itemBox li:first-child{background: #000; color:#fff; text-align: center;}
            .itemBox li:first-child span{background: #fff; color:#000;}
            .itemBox li{background: #f9f9f9; padding:10px; list-style: none;border-bottom: 1px dashed #000;}
            .itemBox li span { float: right;display: block;background: #94d464;height: 35px;margin: -8px -9px 0 0;width: 79px;text-align: center;line-height: 35px;}
        </style>
    </head>

    <body>
        <div id="app">
            <h2 v-bind:title="h1">{{h1}}</h2>
            <div id="tabs">
                <button v-on:click="tabfn(true)" v-bind:class="{active : tab}">Show</button>
                <button v-on:click="tabfn(false)" v-bind:class="{active : !tab}">Hide</button> <br><br>
                <div id="food-item-list" v-if="tab">
                    <label for=""> Search Food : <input type="text" class="searchText" v-model="searchText" @keyup="searchFood"></label>
                    <h3>Food Item</h3>
                    <ul class="itemBox">
                        <!--<food-item v-for="food in list" :foodlist="food" :searchtxt ="searchText"></food-item>-->
                        <li>Food List </li>
                        <li v-for="food in bindList">{{food.item}} <span>₹ {{food.price}}</span></li>
                    </ul>
                    <input type="text" v-model="addItem" class="addItem" placeholder="Enter food item name">
                    <button v-on:click="addItemFn">Add your item</button>
                </div>
                <div v-else>
                    <p>No item have in this section</p>
                </div>
            </div>

        </div>
    </body>
    <script>

        var data = {
            h1: "Vue js five.html (Filters functionality)",
            tab: true,
            list: [{"item": "Kathiyavadi",price:"200"}, {"item": "Gujrati",price:"180"}, {"item": "Panjabi",price:"150"}, {"item": "South-Indian",price:"120"}, {"item": "Bangali",price:"100"}],
            addItem: "",
            searchText: "",
            htmlData: "<p>Your order will surve in just 2 min</p>",
            price:"$",
            total : 1000,
            bindList:[]
        }
        var app = new Vue({
            el: "#app",
            data: data,
            methods: {
                tabfn: function(bolian) {
                    this.tab = bolian
                },
                addItemFn: function() {
                    if (!this.addItem.length) return
                    this.list.push({
                        "item": this.addItem
                    })
                    this.addItem = ""
                    this.searchFood()
                },
                searchFood : function(value){
                    //this.bindList = this.list;
                    this.bindList = []
                    for(var i=0; i < this.list.length; i++){
                        if(this.list[i].item.indexOf(this.searchText) != -1){
                            this.bindList.push(this.list[i])
                        }
                        console.log(this.bindList)
                    }
                }
            },
            computed:{
                reverseH1_:function(){
                    return this.h1.split('').reverse().join('');
                },
                getTotal:{
                    get: function(){
                        return this.price + this.total
                    },
                    set : function(newvalue){
                        this.price = "#"
                    }
                }
            },
            filters:{

            },
        })

        app.searchFood();


    </script>

    </html>


## Color changes
    <!doctype html>

    <html>

    <head>
        <title>Page Title</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="initial-scale=1.0">
        <script src="vue.js"></script>
        <style>
            * {font-family: sans-serif;}
            #app {margin: 0 auto; width:500px;}
            button {
                background: #ccc;
                color: #fff;
                -webkit-appearance:none;
                background: #000;
                border: 0;
                padding: 10px;
                text-align: center;
                width: 49%;
            }
            .searchText{height: 25px;width: 386px;}
            .addItem {height: 30px;width: 226px; padding: 0 10px }
            button.active {
                background: #94d464;
                color: #fff;
            }
            .itemBox {margin: 0 0 10px 0;padding: 0;border: 1px dashed #000;}
            .itemBox li:first-child{background: #000; color:#fff; text-align: center;}
            .itemBox li:first-child span{background: #fff; color:#000;}
            .itemBox li{background: #f9f9f9; padding:10px; list-style: none;border-bottom: 1px dashed #000;}
            .itemBox li span { float: right;display: block;background: #94d464;height: 35px;margin: -8px -9px 0 0;width: 79px;text-align: center;line-height: 35px;}
        </style>
    </head>

    <body>
        <div id="app">
            <h2 v-bind:title="h1">{{h1}}</h2>
            <input type="range" v-model="range" min="10" step="1" max="100" @input="manage">
            <div :style="style"></div>
        </div>
    </body>
    <script>
        var data = {
            h1:"Color manage",
            range:10,
            style:{"height":"100px","width":"130px","background":"rgb(0, 0, 0)"}
        }
        var app = new Vue({
            el: "#app",
            data: data,
            methods: {
                manage:function(value){
                    console.log(this.range)
                    this.style["background"] = "rgb(0, "+this.range+", 0)"
                }
            },
            computed:{

            },
            filters:{

            },
        })


    </script>

    </html>


## Tab functionality in VueJs
    <!doctype html>

    <html>

    <head>
        <title>Page Title</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="initial-scale=1.0">
        <script src="vue.js"></script>
        <style>
            * {
                font-family: sans-serif;
            }

            #app {
                margin: 0 auto;
                width: 500px;
            }

            ul {
                list-style: none;
                margin: 0;
                padding: 0;
            }

            ul li {
                display: inline-block;
                padding: 10px 20px;
                border: 1px solid #000;
                margin-left: 10px;
                border: 1px solid #000;
            }

            ul li.active {
                display: inline-block;
                padding: 10px 20px;
                background: #94d464;
            }
            .tab-title li {cursor: pointer; border-bottom: none;}

        </style>
    </head>

    <body>
        <div id="app">
            <h2 v-bind:title="h1">{{h1}}</h2>
            <div id="tab">
                <ul class="tab-title">
                    <li v-for="(title, index) in tab" @click="activeTab(index)" :class="{active: (index == activeTabIndex)}">{{title.title}}</li>
                </ul>
                <ul>
                    <li v-for="(title,index) in tab" v-if="index == activeTabIndex">{{title.content}}</li>
                </ul>
            </div>
        </div>
    </body>
    <script>
        var data = {
            h1: "Tab system",
            activeTabIndex: 0,
            tab: [{
                title: "one",
                content: "Lorem ipsum dolor sit amet, consectetur adipisicing elit. Inventore aut provident cum rerum! Vero nemo error nesciunt sunt illo ea iste porro pariatur necessitatibus! Quidem unde voluptatem animi cum, adipisci.Lorem ipsum dolor sit amet, consectetur adipisicing elit. Inventore aut provident cum rerum! Vero nemo error nesciunt sunt illo ea iste porro pariatur necessitatibus! Quidem unde voluptatem animi cum, adipisci."
            }, {
                title: "two",
                content: "Lorem ipsum dolor sit amet, consectetur adipisicing elit. Inventore aut provident cum rerum! Vero nemo error nesciunt sunt illo ea iste porro pariatur necessitatibus! Quidem unde voluptatem animi cum, adipisci."
            }, {
                title: "three",
                content: "Lorem ipsum dolor sit amet, consectetur adipisicing elit. Inventore aut provident cum rerum! Vero nemo error nesciunt sunt illo ea iste porro pariatur necessitatibus! Quidem unde voluptatem animi cum, adipisci.Lorem ipsum dolor sit amet, consectetur adipisicing elit. Inventore aut provident cum rerum! Vero nemo error nesciunt sunt illo ea iste porro pariatur necessitatibus! Quidem unde voluptatem animi cum, adipisci.Lorem ipsum dolor sit amet, consectetur adipisicing elit. Inventore aut provident cum rerum! Vero nemo error nesciunt sunt illo ea iste porro pariatur necessitatibus! Quidem unde voluptatem animi cum, adipisci."
            }, {
                title: "four",
                content: "consectetur adipisicing elit. Inventore aut provident cum rerum! Vero nemo error nesciunt sunt illo ea iste porro pariatur necessitatibus! Quidem unde voluptatem animi cum, adipisci."
            }]
        }
        var app = new Vue({
            el: "#app",
            data: data,
            methods: {
                activeTab: function(indx) {
                    this.activeTabIndex = indx
                }
            },
            computed: {

            },
            filters: {

            },
        })

    </script>

    </html>


