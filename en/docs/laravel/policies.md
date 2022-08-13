---
title: "Policies"
slug: "policies"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Creating Policies
Since defining all of the authorization logic in the `AuthServiceProvider` could become cumbersome in large applications, Laravel allows you to split your authorization logic into "Policy" classes. Policies are plain PHP classes that group authorization logic based on the resource they authorize.

You may generate a policy using the `make:policy` artisan command. The generated policy will be placed in the  `app/Policies` directory:

    php artisan make:policy PostPolicy

