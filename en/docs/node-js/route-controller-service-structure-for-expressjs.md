---
title: "Route-Controller-Service structure for ExpressJS"
slug: "route-controller-service-structure-for-expressjs"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Model-Routes-Controllers-Services Directory Structure
    ├───models
    │   ├───user.model.js
    ├───routes
    │   ├───user.route.js
    ├───services
    │   ├───user.service.js
    ├───controllers
    │   ├───user.controller.js

For modular code structure the logic should be divided into these directories and files.

> **Models** - The schema definition of the Model

> **Routes** - The API routes maps to the Controllers

> **Controllers** - The controllers handles all the logic behind validating
> request parameters, query, Sending Responses with correct codes.

> **Services** - The services contains the database queries and returning
> objects or throwing errors

This coder will end up writing more codes. But at the end the codes will be much more maintainable and seperated.



## Model-Routes-Controllers-Services Code Structure


user.model.js
=============

    var mongoose = require('mongoose')
    
    const UserSchema  = new mongoose.Schema({
        name: String
    })
    
    const User = mongoose.model('User', UserSchema)
    
    module.exports = User;

user.routes.js
=============
    var express = require('express');
    var router = express.Router();
    
    var UserController = require('../controllers/user.controller')
    
    router.get('/', UserController.getUsers)
    
    module.exports = router;
user.controllers.js
=============
    var UserService = require('../services/user.service')    
    
    exports.getUsers = async function (req, res, next) {
        // Validate request parameters, queries using express-validator
        
        var page = req.params.page ? req.params.page : 1;
        var limit = req.params.limit ? req.params.limit : 10;
        try {
            var users = await UserService.getUsers({}, page, limit)
            return res.status(200).json({ status: 200, data: users, message: "Succesfully Users Retrieved" });
        } catch (e) {
            return res.status(400).json({ status: 400, message: e.message });
        }
    }

user.services.js
=============
    var User = require('../models/user.model')
    
    exports.getUsers = async function (query, page, limit) {
    
        try {
            var users = await User.find(query)
            return users;
        } catch (e) {
            // Log Errors
            throw Error('Error while Paginating Users')
        }
    }


