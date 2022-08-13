---
title: "Add Admin Panel"
slug: "add-admin-panel"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

If you want to add an Admin panel to your rails application, it's just matter of minutes.

## Syntax
 1. Open gem file and writer gem 'rails_admin', '~> 1.0'
 2. bundle install
 3. rails g rails_admin:install
 4. it will ask you about the admin route if you want to go with the default press Enter.
 5. Now go app/config/initializers/rails_admin.rb and paste this code:     config.authorize_with do
    redirect_to main_app.root_path unless current_user.try(:admin?)
  end
This code will allow only admin user to access the yoursite.com/admin other users will be redirected to the rootpath.
 6. For more details checkout the documentation of this gem. https://github.com/sferik/rails_admin/wiki

Use it if you want to have Admin to your website otherwise there is no need for this. It is more easy and powerful than active_admin gem. You can add this at any stage after creating users and don't forget to make any user admin before the 4th step. Use cancan for granting roles. 

## So here are few screen shots from the admin panel using rails_admin gem.
As you can see the layout of this gem is very catching and user friendly.

[![enter image description here][1]][1]


[![enter image description here][2]][2]


[![enter image description here][3]][3]


  [1]: https://i.stack.imgur.com/xuqoG.png
  [2]: https://i.stack.imgur.com/SQKX2.png
  [3]: https://i.stack.imgur.com/KrU7B.png

