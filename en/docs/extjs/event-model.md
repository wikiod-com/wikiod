---
title: "Event Model"
slug: "event-model"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

ExtJS advocates the use of firing of and listening for events between classes. By firing events and listening for fired events, classes require no 'dirty' knowledge of each others' class structure and prevent coupling code together. Additionally, events make it easy to listen to multiple instances of the same component by allowing a generic listener for all objects with the same selector. Finally, other classes may also be able to make use of events that already exist.

## Controllers Listening to Components
    Ext.define('App.Duck', {
        extend: 'Ext.Component',
        alias: 'widget.duck',
        initComponent: function () {
            this.callParent(arguments);
            this._quack();
        },
        _quack: function () {
            console.log('The duck says "Quack!"');
            this.fireEvent('quack');
        },
        feed: function () {
            console.log('The duck looks content.');
        },
        poke: function () {
            this._quack();
        }
    });
        
    var feedController = Ext.create('Ext.app.Controller', {
        listen: {
            components: {
                duck: {
                    quack: 'feedDuck'
                }
            }
        },
        feedDuck: function (duck) {
            duck.feed();
        }
    });

    var countController = Ext.create('Ext.app.Controller', {
        listen: {
            components: {
                duck: {
                    quack: 'addCount'
                }
            }
        },
        quackCount: 0,
        addCount: function (duck) {
            this.quackCount++;
            console.log('There have been this many quacks: ' + this.quackCount);
        }
    });

    var firstDuck = Ext.create('App.Duck');
    // The duck says "Quack!"
    // The duck looks content.
    // There have been this many quacks: 1
    var secondDuck = Ext.create('App.Duck');
    // The duck says "Quack!"
    // The duck looks content.
    // There have been this many quacks: 2
    firstDuck.poke();
    // The duck says "Quack!"
    // The duck looks content.
    // There have been this many quacks: 3

