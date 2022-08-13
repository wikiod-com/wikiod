---
title: "Firebase Queue"
slug: "firebase-queue"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## How to use firebase queue as a backend for your application
Firebase provides backend as a service, as applciation developer you do not have an option to have backend code.

This example shows how using firebase queue, create backend which will operate on the top of firebase database and serve as a backend for your frontend application.

Before getting into the code lets understand the architecture, how it will work.
For brevity lets suppose that we are using web site as a frontend and NodeJs server as a backend

## Prerequisites ##
 1. Create firebase application using your google account 

 2. Add firebase to your web page. Use `bower install firebase --save`
 3. Create service account using your new created firebase account (Settings->Permissions -> Service Accounts -> CREATE SERVICE ACCOUNT -> (specify name and check this "Furnish a new private key" checkbox ) -> save the json file, we will need that later.
4. Configure NodeJs server which can be hosted in your prefered environment
5. Create following endpoint inside `queue/specs`

    "request_response": 

        {
            "error_state": "request_error_processing",
            "finished_state": "finished_state",
            "in_progress_state": "request_in_progress",
            "start_state": "request_started"
        }
6. Inside NodeJs server install firebase server side version, `npm install firebase --save`, and intialize your service account using the json file which we got from the step 3, it look like this 

    firebase.initializeApp({
        serviceAccount: './your_file.json',
        databaseURL: 'get_from_firebase_account'
    });

## Architecture ##
Here is the whole cycle how it works. 

On the frontend side you gonna do these steps
1. Using firebase web sdk you are writing your requests directly into firebase database in the endpoint 'queue/tasks', lets call that your request which you are sending to the backend. 
2. after inserting your task you are registering listener on the endpoint `queue/tasks/{taskKey}` which would be called when backend finishes processing your request, writing response inside above task

In the backend side you gonna do these steps
1. Create server which infinitely listens endpoint 'queue/tasks'
2. Processes your tasks and writing back response data inside `queue/tasks/response`
3. Remove the task

First of all create this helper function, which provides a way of handling callbacks and promises together

    function createPromiseCallback() {
        var cb;
        var promise = new Promise(function (resolve, reject) {
            cb = function (err, data) {
                if (err) return reject(err);
                return resolve(data);
            };
        });
        cb.promise = promise;
        return cb;
    }

In the frontend side you gonna have this function

    function sendRequest(kind, params, cb) {

        cb = cb || createPromiseCallback();
        var requestObject = {
            kind: kind,
            params: params
        };
        var tasksRef = firebase.database().ref('queue/tasks');

        var requestKey = tasksRef.push().key;

        var requestRef = tasksRef.child(requestKey);

        function requestHandshake(snap) {
            if (snap && snap.exists() && (snap.val().response || snap.val()._state ===    config.firebase.task.finishState || snap.val()._error_details)) {
                var snapVal = snap.val();
                if (snapVal._error_details) {
                    cb(snapVal._error_details.error);
                } else {
                cb(null, snapVal.response);
            }
            requestRef.off('value', requestHandshake);
        }
       }

       var bulkUpdate = {};
       bulkUpdate['queue/tasks/' + requestKey + '/request'] = requestObject;
       bulkUpdate['queue/tasks/' + requestKey + '/_state'] = config.firebase.task.startState;

       firebase.database().ref().update(bulkUpdate)
        .then(function (snap) {
            requestRef.on('value', requestHandshake);
        }).catch(function (err) {
            cb(err);
        });

       return cb.promise;
      }

you can use this function like `sendRequest('CreateHouseFacade', {houseName:'Test'})`.

Kind parameter is for backend, to know what method to call for processing request.
Params is for passing additional parameter information.

And here is the backend code

    const database = firebase.database();
    const queueRef = database.ref('queue');

    const queueOptions = {
        'specId': 'request_response',
        'sanitize': false,
        'suppressStack': false,
        'numWorkers': 3
    };

    function removeTask(task) {
        var taskRef = queueRef.child(`tasks/${task._id}`);
        return taskRef.remove();
    }

    function processTask(data, progress, resolve, reject) {
        try {
            requestHandler(data.request).then(response => {
                data.response = response || null;
                return resolve(data);
            }).catch(err => {
                return reject(err);
            }).then(snap => {
                removeTask(data);
            });
        } catch (err) {
            reject(err).then(snap => removeTask(data));
        }
    }

    function requestHandler(request) {
        if (!request || !request.kind) throw new Error('Absent Request or Kind');
        var deferredResponse = requestHandlerFactory(request.kind, request.params);
        return deferredResponse;
    }

    function requestHandlerFactory(kind, params) {
        // It includes mapping all your backend services
        switch (kind) {
            case 'CreateHouseFacade': return myService(params)
            default: throw new Error(`Invalid kind ${kind} was specified`);
        }
    }
The function `myService` contains your business logic code which gonna accomplishing `CreateHouseFacade` request.









