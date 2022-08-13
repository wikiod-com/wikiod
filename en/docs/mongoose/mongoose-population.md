---
title: "Mongoose Population"
slug: "mongoose-population"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Syntax
 - Query.populate(path, [select], [model], [match], [options])



## Parameters
 Parameter | Explanation 
   ------  |  ------
 path   | <**Object, String**> either the path to populate or an object specifying all parameters   
 [select] | <**Object, String**> Field selection for the population query (can use `'-id'` to include everything but the `id` field)    
 [model]   |   <**Model**> The model you wish to use for population.If not specified, populate will look up the model by the name in the Schema's ref field.    
 [match]   | <**Object**> Conditions for the population    
 [options]   | <**Object**> Options for the population query (sort, etc)

  




## A simple mongoose populate example
`.populate()` in Mongoose allows you to populate a reference you have in your current collection or document with the information from that collection. The previous may sound confusing but I think an example will help clear up any confusion. 

The following code creates two collections, User and Post: 

    var mongoose = require('mongoose'),
      Schema = mongoose.Schema
    
    var userSchema = Schema({
      name: String,
      age: Number,
      posts: [{ type: Schema.Types.ObjectId, ref: 'Post' }]
    });
    
    var PostSchema = Schema({
      user: { type: Schema.Types.ObjectId, ref: 'User' },
      title: String,
      content: String
    });
    
    var User = mongoose.model('User', userSchema);
    var Post = mongoose.model('Post', postSchema);

If we wanted to populate all of the posts for each user when we `.find({})` all of the Users, we could do the following: 

    User
      .find({})
      .populate('posts')
      .exec(function(err, users) {
        if(err) console.log(err);
        //this will log all of the users with each of their posts 
        else console.log(users);
      }) 



