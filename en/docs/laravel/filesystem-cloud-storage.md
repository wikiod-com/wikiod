---
title: "Filesystem  Cloud Storage"
slug: "filesystem--cloud-storage"
draft: false
images: []
weight: 9884
type: docs
toc: true
---

## Basic Usage
The `Storage` facade may be used to interact with any of your configured disks. Alternatively, you may type-hint the `Illuminate\Contracts\Filesystem\Factory` contract on any class that is resolved via the Laravel service container.

**Retrieving A Particular Disk**

    $disk = Storage::disk('s3');

    $disk = Storage::disk('local');

**Determining If A File Exists**

    $exists = Storage::disk('s3')->exists('file.jpg');

**Calling Methods On The Default Disk**

    if (Storage::exists('file.jpg'))
    {
        //
    }

**Retrieving A File's Contents**

    $contents = Storage::get('file.jpg');

**Setting A File's Contents**

    Storage::put('file.jpg', $contents);

**Prepend To A File**

    Storage::prepend('file.log', 'Prepended Text');

**Append To A File**

    Storage::append('file.log', 'Appended Text');

**Delete A File**

    Storage::delete('file.jpg');

    Storage::delete(['file1.jpg', 'file2.jpg']);

**Copy A File To A New Location**

    Storage::copy('old/file1.jpg', 'new/file1.jpg');

**Move A File To A New Location**

    Storage::move('old/file1.jpg', 'new/file1.jpg');

**Get File Size**

    $size = Storage::size('file1.jpg');

**Get The Last Modification Time (UNIX)**

    $time = Storage::lastModified('file1.jpg');

**Get All Files Within A Directory**

    $files = Storage::files($directory);

    // Recursive...
    $files = Storage::allFiles($directory);

**Get All Directories Within A Directory**

    $directories = Storage::directories($directory);

    // Recursive...
    $directories = Storage::allDirectories($directory);

**Create A Directory**

    Storage::makeDirectory($directory);

**Delete A Directory**

    Storage::deleteDirectory($directory);

## Configuration
The filesystem configuration file is located at `config/filesystems.php`. Within this file you may configure all of your "disks". Each disk represents a particular storage driver and storage location. Example configurations for each supported driver is included in the configuration file. So, simply modify the configuration to reflect your storage preferences and credentials!

Before using the S3 or Rackspace drivers, you will need to install the appropriate package via Composer:

 - Amazon S3: `league/flysystem-aws-s3-v2 ~1.0`
 - Rackspace: `league/flysystem-rackspace ~1.0`

Of course, you may configure as many disks as you like, and may even have multiple disks that use the same driver.

When using the local driver, note that all file operations are relative to the root directory defined in your configuration file. By default, this value is set to the `storage/app directory`. Therefore, the following method would store a file in `storage/app/file.txt`:

    Storage::disk('local')->put('file.txt', 'Contents');

## Custom Filesystems
Laravel's Flysystem integration provides drivers for several "drivers" out of the box; however, Flysystem is not limited to these and has adapters for many other storage systems. You can create a custom driver if you want to use one of these additional adapters in your Laravel application. Don't worry, it's not too hard!

In order to set up the custom filesystem you will need to create a service provider such as `DropboxFilesystemServiceProvider`. In the provider's `boot` method, you can inject an instance of the `Illuminate\Contracts\Filesystem\Factory` contract and call the `extend` method of the injected instance. Alternatively, you may use the `Disk` facade's `extend` method.

The first argument of the `extend` method is the name of the driver and the second is a Closure that receives the `$app` and `$config` variables. The resolver Closure must return an instance of `League\Flysystem\Filesystem`.

> Note: The $config variable will already contain the values defined in `config/filesystems.php` for the specified disk.
Dropbox Example



    <?php namespace App\Providers;

    use Storage;
    use League\Flysystem\Filesystem;
    use Dropbox\Client as DropboxClient;
    use League\Flysystem\Dropbox\DropboxAdapter;
    use Illuminate\Support\ServiceProvider;
    
    class DropboxFilesystemServiceProvider extends ServiceProvider {
    
        public function boot()
        {
            Storage::extend('dropbox', function($app, $config)
            {
                $client = new DropboxClient($config['accessToken'], $config['clientIdentifier']);
    
                return new Filesystem(new DropboxAdapter($client));
            });
        }
    
        public function register()
        {
            //
        }
    
    }

## Creating symbolic link  in a  web server using SSH
In Laravel documentation, a symbolic link (symlink or soft link) from public/storage to storage/app/public
 should be created to make files accessible from the web.

*(THIS PROCEDURE WILL CREATE SYMBOLIC LINK WITHIN THE LARAVEL PROJECT DIRECTORY)*

Here are the steps on how you can create symbolic link in your Linux web server using SSH client: 
 1. Connect and login to your web server using SSH client (e.g. PUTTY).
 2. Link **storage/app/public** to **public/storage** using the syntax

    `ln -s target_path link_path`

    Example (in CPanel File Directory)

    `ln -s /home/cpanel_username/project_name/storage/app/public /home/cpanel_sername/project_name/public/storage`

*(A folder named **storage** will be created to link path with an indicator **>>>** on the folder icon.)*

    
    


