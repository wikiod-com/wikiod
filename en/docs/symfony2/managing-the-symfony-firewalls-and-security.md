---
title: "Managing the Symfony firewalls and security"
slug: "managing-the-symfony-firewalls-and-security"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Managing Security
Security was a part of the dark side of the symfony documentation, it has a dedicated component named **Security Component**.

This component is configured in the **security.yml** file of the main application project.

The default configuration is like this one :

    # app/config/security.yml
    security:
        providers:
            in_memory:
                memory: ~
        
        firewalls:
            dev:
                pattern: ^/(_(profiler|wdt)|css|images|js)/
                security: false
    
            default:
                anonymous: ~

You can define specific **Firewalls** to restrict access to some URL to specific **Roles** based on a hierarchy for your **Users** that are defined by a **Provider** and **Encoders** that manage the password security.

For example, if you want to create a custom **Provider**, from your database engine, you can define you **security.yml** like this :

    providers:
        your_db_provider:
            entity:
                class: AppBundle:User
                property: apiKey

This is detailled in the symfony Documentation : [How to define a custom UserProvider][1] and [from the database][2] or [against LDAP][3] for example.

After that, you can defined **firewall** to restrict some URL based on your custom user provider (security.yml) explicitely like this :

    firewalls:
        secured_area:
            pattern: ^/admin

Or with **access control** :

    access_control:
     - { path: ^/admin/users, roles: ROLE_SUPER_ADMIN }
     - { path: ^/admin, roles: ROLE_ADMIN }
 
See more detailled documentation [here][4].

The best way to manage user is to use [FosUserBundle][5] that extends some framework functionnalities.


  [1]: http://symfony.com/doc/2.8/security/custom_provider.html
  [2]: http://symfony.com/doc/2.8/security/firewall_restriction.html
  [3]: http://symfony.com/doc/2.8/security/ldap.html
  [4]: http://symfony.com/doc/2.8/security.html
  [5]: http://symfony.com/doc/current/bundles/FOSUserBundle/index.html

