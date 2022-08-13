---
title: "Handling users with socket.io"
slug: "handling-users-with-socketio"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

Handling users within socket.io is as simple or as complex as you decided, though there are some more 'obvious' approaches for doing this, this documentation is going to outline an approach using `map()`.

## Example Server Side code for handling Users
Firstly it's important to note that when a new socket is created it is assigned a unique Id which is retrieved by calling `socket.id`. This `id` can then be stored within a `user` object and we can assign an identifier such as a username which has been used in this example to retrieve `user` objects.

    /**
     * Created by Liam Read on 27/04/2017.
     */
    
    var express = require('express');
    var app = express();
    var http = require('http').Server(app);
    var io = require('socket.io')(http);
    
    
    function User(socketId) {
    
        this.id = socketId;
        this.status = "online";
        this.username = "bob";
    
        this.getId = function () {
            return this.id;
        };
    
        this.getName = function () {
            return this.username;
        };
    
        this.getStatus = function () {
            return this.status;
        };
    
        this.setStatus = function (newStatus) {
            this.status = newStatus;
        }
    }
    
    var userMap = new Map();
    
    /**
     * Once a connection has been opened this will be called.
     */
    io.on('connection', function (socket) {
    
        var user;
    
        /**
         * When a user has entered there username and password we create a new entry within the userMap.
         */
        socket.on('registerUser', function (data) {
    
            userMap.set(data.name, new User(socket.id));
    
            //Lets make the user object available to all other methods to make our code DRY.
            user = userMap.get(data.name);
        });
    
        socket.on('loginUser', function (data) {
            if (userMap.has(data.name)) {
                //user has been found
    
                user = userMap.get(data.name);
            } else {
                //Let the client know that no account was found when attempting to sign in.
                socket.emit('noAccountFound', {
                    msg: "No account was found"
                });
            }
        });
    
        socket.on('disconnect', function () {
            //Let's set this users status to offline.
            user.setStatus("offline");
        });
    
        /**
         * Dummy server event that represents a client looking to send a message to another user.
         */
        socket.on('sendAnotherUserAMessage', function (data) {
    
            //Make note here that by checking to see if the user exists within the map we can be sure that when
            // retrieving the value after && that we won't have any unexpected errors.
            if (userMap.has(data.name) && userMap.get(data.name).getStatus() !== "offline") {
                var OtherUser = userMap.get(data.name);
            } else {
                //We use a return here so further code isn't executed, you could replace this with some for of
                //error handling or a different event back to the user.
                return;
            }
    
            //Lets send our message to the user.
            io.to(OtherUser.getId()).emit('recMessage', {
                msg: "Nice code!"
            })
    });


    });

This is by no means a complete example of even close to what is possible but should give a basic understanding of an approach to handling `users`.

## Handling users accessing modals
This example shows how you might handle users interacting with modals on a 1-1 basis.

    //client side
    function modals(socket) {
    
        this.sendModalOpen = (modalIdentifier) => {
    
            socket.emit('openedModal', {
                modal: modalIdentifier
            });
        };
    
        this.closeModal = () => {
            socket.emit('closedModal', {
                modal: modalIdentifier
            });
        };
    
    }
    
    
    socket.on('recModalInfo', (data) => {
        for (let x = 0; x < data.info.length; x++) {
            console.log(data.info[x][0] + " has open " + data.info[x][1]);
        }
    });
    
    //server side
    let modal = new Map();
    
    io.on('connection', (socket) => {
    
        //Here we are sending any new connections a list of all current modals being viewed with Identifiers.
        //You could send all of the items inside the map() using map.entries
    
        let currentInfo = [];
    
        modal.forEach((value, key) => {
            currentInfo.push([key, value]);
        });
    
        socket.emit('recModalInfo', {
            info: currentInfo
        });
    
        socket.on('openedModal', (data) => {
            modal.set(socket.id, data.modalIdentifier);
        });
    
        socket.on('closedModal', (data) => {
            modal.delete(socket.id);
        });
    
    });

By handling all of the modal interactions here all newly connected users will have all information about which ones are currently being viewed allow us to make decisions based on current users within our system.

## Simple Way To Emit Messages By User Id
On the server:

    var express = require('express');
    var socketio = require('socket.io');

    var app = express();
    var server = http.createServer(app);
    var io = socketio(server);

    io.on('connect', function (socket) {
      socket.on('userConnected', socket.join);
      socket.on('userDisconnected', socket.leave);
    });

    function message (userId, event, data) {
      io.sockets.to(userId).emit(event, data);
    }

On the client:

    var socket = io('http://localhost:9000');  // Server endpoint

    socket.on('connect', connectUser);

    socket.on('message', function (data) {
      console.log(data);
    });

    function connectUser () {  // Called whenever a user signs in
      var userId = ...  // Retrieve userId
      if (!userId) return;
      socket.emit('userConnected', userId);
    }

    function disconnectUser () {  // Called whenever a user signs out
      var userId = ...  // Retrieve userId
      if (!userId) return;
      socket.emit('userDisconnected', userId);
    }

This method allows sending messages to specific users by unique id without holding a reference to all sockets on the server.

