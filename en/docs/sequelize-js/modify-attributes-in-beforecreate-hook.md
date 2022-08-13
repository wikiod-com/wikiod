---
title: "Modify attributes in beforeCreate hook"
slug: "modify-attributes-in-beforecreate-hook"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

We might need to modify the attributes before record is created. My use case was encrypting password when user is created.

Hooks doc is here http://docs.sequelizejs.com/en/v3/docs/hooks/#instance-hooks. It documents the way to use it with a library / function that returns a `Promise`.
But the use case with a callback is not clearly documented.

## Syntax
- beforeCreate(instance)
- beforeCreate(instance, options, fn)

## Example working with a library that doesn't use Promise
    function cryptPassword(password, callback) {
      bcrypt.genSalt(SALT_WORK_FACTOR, function(err, salt) {
        if (err)
          return callback(err);

        bcrypt.hash(password, salt, null, function(err, hash) {
          return callback(err, hash);
        });
      });
    }

    User.beforeCreate((user, options, cb) => {
      cryptPassword(user.password, (err, hash) => {
        if (err) return cb(err);

        user.password = hash;
        // invoking the finish callback is important!
        return cb(null, options);
      });    
    });

## Example working with a library that doesn't use Promise
    User.beforeCreate(function(user, options) {
      return hashPassword(user.password).then(function (hashedPw) {
      user.password = hashedPw;
      });
    })

