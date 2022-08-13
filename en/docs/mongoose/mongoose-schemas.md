---
title: "Mongoose Schemas"
slug: "mongoose-schemas"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Creating a Schema
    var mongoose = require('mongoose');
    
    //assume Player and Board schemas are already made
    var Player = mongoose.model('Player'); 
    var Board = mongoose.model('Board');
    
    //Each key in the schema is associated with schema type (ie. String, Number, Date, etc)
    var gameSchema = new mongoose.Schema({
      name: String,
      players: [{
            type: mongoose.Schema.Types.ObjectId, 
            ref: 'Player'
      }],
      host: {
        type: mongoose.Schema.Types.ObjectId,
        ref: 'Player'
      },
      board: {
        type: mongoose.Schema.Types.ObjectId,
        ref: 'Board'
      },
      active: {
        type: Boolean,
        default: true
      },
      state: {
        type: String,
        enum: ['decision', 'run', 'waiting'],
        default: 'waiting'
      },
      numFlags: {
        type: Number,
        enum: [1,2,3,4]
      },
      isWon: {
        type: Boolean,
        default: false
      }
    });

    mongoose.model('Game', gameSchema);



