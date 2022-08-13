---
title: "Dependency Injection"
slug: "dependency-injection"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

If injecting more than one class, the order you put them in the @inject() statement does not matter. However, the order they appear in the @inject() statement must match the order of the parameters in the constructor.

## Get and Display Username by Id
    import {User} from 'backend/user'; // import custom class
    import {inject} from 'aurelia-framework'; // allows us to inject

    @inject(User) // inject custom class
    export class ProfileView {
      constructor(user) { // use instance of custom class as a parameter to the constructor
        this.user = user; // save instance as a an instance variable
        this.username = '';
      }
    
      activate(params) {
        // call function from custom class, then save the result as another instance variable
        return this.user.getUsernameById(param.user_id)
          .then(user => this.username = user.username);
      }
    }

