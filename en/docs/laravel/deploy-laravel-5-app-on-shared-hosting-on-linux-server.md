---
title: "Deploy Laravel 5 App on Shared Hosting on Linux Server"
slug: "deploy-laravel-5-app-on-shared-hosting-on-linux-server"
draft: false
images: []
weight: 9551
type: docs
toc: true
---

To get more information on deploying Laravel project on shared hosting, [visit this Github repo.][1]


  [1]: https://github.com/petehouston/laravel-deploy-on-shared-hosting

## Laravel 5 App on Shared Hosting on Linux Server
By default Laravel project's `public` folder exposes the content of the app which can be requested from anywhere by anyone, the rest of the app code is invisible or inaccessible to anyone without proper permissions. 
 
After developing the application on your development machine, it needs to be pushed to a production server so that it can be accessed through the internet from anywhere - right?  

For most apps/websites the first choice is to use shared hosting package from hosting service providers like GoDaddy, HostGator etc. mainly due to low cost.

>**note**: you may ask your provider to manually change **document_root**, so all you have to do is upload your Laravel application to server (via FTP), request change of root to **{app}/public** and you should be good.
 
Such shared hosting packages, however do have limitations in terms of terminal access and file permissions. By default one has to upload their app/code to the `public_html` folder on their shared hosting account.  

So if you want to upload a Laravel project to a shared hosting account how would you go about it? Should you upload the entire app (folder) to the `public_html` folder on your shared hosting account? - **Certainly NO**  

Because everything in the `public_html` folder is accessible "publically i.e. by anyone" which would be a big security risk.  
  
Steps to upload a project to shared hosting account - the Laravel way  

**Step 1**  
Create a folder called laravel (or anything you like) on the same level as the `public_html` folder.  
    
    Eg:  
    /
    |--var  
        |---www
            |----laravel       //create this folder in your shared hosting account
            |----public_html  
            |----log  
**Step 2**  
 Copy every thing except the `public` folder from your laravel project (on development machine) in the `laravel` folder (on server host - shared hosting account).  
You can use:  
 - C-panel : which would be the slowest option  
 - FTP Client: like ***FileZilla*** to connect to you shared hosting account and transfer your files and folders through FTP upload  
 - Map Network Drive: you can also create a mapped network drive on your development machine to connect to your shared hosting account's root folder using "ftp://your-domain-name" as the network address.    

**Step 3**  
 Open the `public` folder of your laravel project (on development machine), copy everything and paste in the `public_html` folder (on server host - shared hosting account).  
**Step 4**  
Now open the `index.php` file in the `public_html` folder on the shared hosting account (in cpanel editor or any other connected editor) and:  
    
***Change:***  

    require __DIR__.'/../bootstrap/autoload.php';   
 
***To:***  
  
    require __DIR__.'/../laravel/bootstrap/autoload.php';  

 
***And Change:***  

    $app = require_once __DIR__.'/../bootstrap/app.php';
 
***To:***  
  
    $app = require_once __DIR__.'/../laravel/bootstrap/app.php';
 
***Save and close.*** 
 
**Step 5**  
 Now go to the `laravel` folder (on shared hosting account -server) and open `server.php` file  
***Change***  

    require_once __DIR__.'/public/index.php';
  
***To:***  
  
    require_once __DIR__.'../public_html/index.php';  


***Save and close.***  
  
**Step 6**  
Set file permissions for the `laravel/storage` folder (recursively) and all files, sub-folders and file within them on shared hosting account - server to `777`.  
***Note:*** Be careful with the file permissions in linux, they are like double edged sword, if not used correctly, they may make your app vulnerable to attacks. For understanding Linux file permissions you can read https://www.linux.com/learn/tutorials/309527-understanding-linux-file-permissions

**Step 7**

As `.env` file of local/development server is Ignored by git and it should be ignored as it has all the environment variables including the APP_KEY and it should not be exposed to public by pushing it into the repositories'. You can also see that `.gitignore` file has `.env` mentioned thus it will not upload it to repositories.
 
After following all the above steps make a `.env` file in the laravel folder and add all the environment variable which you have used from the local/development server's `.env` file to the `.env` file of production server. 
 
Even there are configuration files like `app.php`, `database.php` in config folder of laravel application which defines this variables as by default in second parameter of `env()` but don't hard-code the values in these files as it will affect the configuration files of the users who pulls your repository. So it is recommended to create `.env` file manually! 
 
 
Also laravel gives `.env-example` file that you can use as a reference. 


That's it.

Now when you visit the url which you configured as the domain with your server, your laravel app should work just as it worked on your localhost - development machine, while still the application code is safe and not accessible by anyone without proper file permissions.


