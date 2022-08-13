---
title: "Permissions for storage"
slug: "permissions-for-storage"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Laravel requires some folders to be writable for the web server user.

## Example
We also need to set correct permissions for `storage` files in the `server`. So, we need to give a write permission in the storage directory as follows:

    $ chmod -R 777 ./storage ./bootstrap

or you may use

    $ sudo chmod -R 777 ./storage ./bootstrap

For windows

Make sure you are an admin user on that computer with writeable access

    xampp\htdocs\laravel\app\storage needs to be writable

The NORMAL way to set permissions is to have your files owned by the webserver:

    sudo chown -R www-data:www-data /path/to/your/root/directory



