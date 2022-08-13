---
title: "JSON web token authentication with Sails"
slug: "json-web-token-authentication-with-sails"
draft: false
images: []
weight: 9783
type: docs
toc: true
---

## Configuration
Step one
============

We need to create a service called *jwToken*.
Go to `api/services` directory and create `jwToken.js`.

    'use strict';

    const jwt = require('jsonwebtoken'),
          tokenSecret = "secretissecret";

    module.exports = {
      // Generates a token from supplied payload
      issue(payload) {
        return jwt.sign(
          payload,
          tokenSecret, // Token Secret that we sign it with
          {
            expiresIn: "30 days" // Token Expire time
          });
      },

      // Verifies token on a request
      verify(token, callback) {
        return jwt.verify(
          token, // The token to be verified
          tokenSecret, // Same token we used to sign
          {}, // No Option, for more see https://github.com/auth0/node-jsonwebtoken#jwtverifytoken-secretorpublickey-options-callback
          callback //Pass errors or decoded token to callback
        );
      }
    };


Step two
=================

Encrypt our password using `bcrypt`.
Go to `api/models/User.js`.

    'use strict';
    const bcrypt = require('bcrypt');

    module.exports = {

        attributes: {
            // your code...
        },

        // Here we encrypt password before creating a User
        beforeCreate(values, next) {
            bcrypt.genSalt(10, (err, salt) => {
                if (err) {
                    sails.log.error(err);
                    return next();
                }

                bcrypt.hash(values.password, salt, (err, hash) => {
                    if (err) {
                        sails.log.error(err);
                        return next();
                    }
                    values.encryptedPassword = hash; // Here is our encrypted password
                    return next();
                });
            });
        },

        comparePassword(password, encryptedPassword) {

            return new Promise(function(resolve, reject) {
                bcrypt.compare(password, encryptedPassword, (err, match) => {
                    if (err) {
                        sails.log.error(err);
                        return reject("Something went wrong!");
                    }
                    if (match) return resolve();
                    else return reject("Mismatch passwords");
                });
            });
        }
    };

Step three
===============
Create *isAuthorized* policy to check if a user has valid token in the request header.
Go to `api/policies` and create `isAuthorized.js`.

    'use strict';

    module.exports = (req, res, next) => {
      let token;


      if (req.headers && req.headers.token) {
        token = req.headers.token;
        if (token.length <= 0) return res.json(401, {err: 'Format is Authorization: Bearer [token]'});

      } else if (req.param('token')) {
        token = req.param('token');
        // We delete the token from param to not mess with blueprints
        delete req.query.token;
      } else {
        return res.json(401, {err: 'No Authorization header was found'});
      }

      jwToken.verify(token, function (err, token) {
        if (err) return res.json(401, {err: 'Invalid Token!'});
        req.token = token; // This is the decrypted token or the payload you provided
        next();
      });
    };

Step four 
=================
We use config/policies.js to protect our controllers

    module.exports.policies = {

      '*': ['isAuthorized'], // Everything resctricted here
      'UserController': { // Name of your controller
        'create': true // We dont need authorization here, allowing public access
      }
    };

Step five
===============
Let's test our implementation. Go to `api/controllers` and create `UserController.js`

    'use strict';

    module.exports = {
        create(req, res) {
            const data = req.body;
            if (data.password !== data.confirmPassword) return res.badRequest("Password not the same");

            User.create({
                    email: data.email,
                    password: data.password,
                    name: data.name
                        //etc...
                })
                .then((user) => {
                    res.send({ token: jwToken.issue({ id: user.id }) }); // payload is { id: user.id}
                })
                .catch((err) => {
                    sails.log.error(err);
                    return res.serverError("Something went wrong");
                });
        },

        login(req, res) {
            const data = req.body;

            if (!data.email || !data.password) return res.badRequest('Email and password required');

            User.findOne({ email: email })
                .then((user) => {
                    if (!user) return res.notFound();

                    User.comparePassword(password, user.password)
                        .then(() => {
                            return res.send({ token: jwToken.issue({ id: user.id }) })
                        })
                        .catch((err) => {
                            return res.forbidden();
                        });
                })
                .catch((err) => {
                    sails.log.error(err);
                    return res.serverError();
                });
        }
    };







## Installation
We need two dependencies:

 1. **bcrypt** for encryption
    `npm install bcrypt --save`
 2. **JSON Web token** `npm install jsonwebtoken --save`

