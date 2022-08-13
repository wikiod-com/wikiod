---
title: "Getting started with ejabberd"
slug: "getting-started-with-ejabberd"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Allow multiple user registration from different devices so quickly on Ejabberd server.
To allow multiple user registration on `Ejabberd` server, we need to configure file `ejabberd.yml` in `Ejabberd` latest versions. Configure   `ejabberd.yml` like:
in `access_rules:` add 

    register:
        - allow
      register_from:
        - allow
      registration_timeout:
        - infinity
    
      mod_register:
        access_from:
          register_from
        access: 
          register
and in `modules:` add
 

     mod_register:
        access_from: register_from
        access: register
After adding these rules in `ejabberd.yml` file save it and run `Ejabberd` server and for checking purpose of these rules are really add or not so you can check it on `Ejabberd` server console, access_rules in raw file will looks like:

    [{access, announce, [{allow, [{acl, admin}]}]},
     {access, c2s,
      [{deny, [{acl, blocked}]}, {allow, [all]}]},
     {access, c2s_shaper,
      [{none, [{acl, admin}]}, {normal, [all]}]},
     {access, configure, [{allow, [{acl, admin}]}]},
     {access, local, [{allow, [{acl, local}]}]},
     {access, max_user_offline_messages,
      [{5000, [{acl, admin}]}, {10000, [all]}]},
     {access, max_user_sessions, [{infinity, [all]}]},
     {access, mod_register,
      [{access_from, [{acl, register_from}]},
       {access, [{acl, register}]}]},
     {access, muc_create, [{allow, [{acl, local}]}]},
     {access, pubsub_createnode, [{allow, [{acl, local}]}]},
     {access, register, [{allow, [all]}]},
     {access, register_from, [{allow, [all]}]},
     {access, registration_timeout, [{infinity, [all]}]},
     {access, s2s_shaper, [{fast, [all]}]},
     {access, trusted_network, [{allow, [all]}]}].


and modules  will look like:

    mod_register:[{access_from, register_from},
     {access, register},
     {welcome_message,
      [{subject,
        <<87, 101, 108, 99, 111, 109, 101,
          33>>},
       {body,
        <<72, 105, 46, 10, 87, 101, 108, 99,
          111, 109, 101, 32, 116, 111, 32,
          116, 104, 105, 115, 32, 88, 77,
          80, 80, 32, 115, 101, 114, 118,
          101, 114, 46>>}]},
     {ip_access, trusted_network},
     {access, register}]


now your server is ready to register multiple users from different devices so quickly. Thank you. 

