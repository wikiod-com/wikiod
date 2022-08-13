---
title: "Getting started with symfony3"
slug: "getting-started-with-symfony3"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## 3. Windows Systems
You must add php to your path environment variable. Follow theses steps :

**Windows 7 :**

 - Right-click on a My Computer icon
 - Click Properties
 - Click Advanced system settings from the left nav
 - Click Advanced tab
 - Click Environment Variables button
 - In the System Variables section, select Path (case-insensitive) and click Edit button
 - Add a semi-colon (;) to the end of the string, then add the full file system path of your PHP installation (e.g. `C:\Program Files\PHP`)
 - Keep clicking OK etc until all dialog boxes have disappeared
 - Close your command prompt and open it again
 - Sorted

**Windows 8 & 10**

 - In Search, search for and then select: System (Control Panel)
 - Click the Advanced system settings link.
 - Click Environment Variables.
 - In the section System Variables, find the PATH environment variable and select it. Click Edit. If the PATH environment variable does not exist, click New.
 - Add the full file system path of your PHP installation (e.g. `C:\Program Files\PHP`)


After this, open your command console and execute the following command:

    c:\> php -r "readfile('https://symfony.com/installer');" > symfony

Then, move the downloaded symfony file to your project's directory and execute it as follows:

    c:\> move symfony c:\projects
    c:\projects\> php symfony

## 1. Installing the Symfony Installer
> The installer requires PHP 5.4 or higher. If you still use the legacy PHP 5.3 version, you cannot use the Symfony Installer. Read the Creating Symfony Applications without the Installer section to learn how to proceed. - source: http://symfony.com/doc/current/book/installation.html

## 5. Basing your Project on a Specific Symfony Version
In case your project needs to be based on a specific Symfony version, use the optional second argument of the new command:

    # use the most recent version in any Symfony branch
    $ symfony new my_project_name 2.8
    $ symfony new my_project_name 3.1
    
    # use a specific Symfony version
    $ symfony new my_project_name 2.8.1
    $ symfony new my_project_name 3.0.2
    
    # use a beta or RC version (useful for testing new Symfony versions)
    $ symfony new my_project 3.0.0-BETA1
    $ symfony new my_project 3.1.0-RC1

The installer also supports a special version called lts which installs the most recent Symfony LTS version available:

    $ symfony new my_project_name lts

Read the Symfony Release process to better understand why there are several Symfony versions and which one to use for your projects.

You can also create symfony applications without the installer, but it was not an good idea. If you want anyway, follow the original tutorial on this link:

[Oficial Symfony Docs, Configuring Symfony without the installer][1]


  [1]: http://symfony.com/doc/current/book/installation.html#creating-symfony-applications-without-the-installer

## 2. Linux and Mac OS X Systems
Open your command console and execute the following commands:

    $ sudo curl -LsS https://symfony.com/installer -o /usr/local/bin/symfony
    $ sudo chmod a+x /usr/local/bin/symfony

## 4. Creating the Symfony Application
Once the Symfony Installer is available, create your first Symfony application with the new command:

    # Linux, Mac OS X
    $ symfony new my_project_name
    
    # Windows
    c:\> cd projects/
    c:\projects\> php symfony new my_project_name

This command can be run from anywhere, not necessarily from the <code>htdocs</code> folder.

This command creates a new directory called `my_project_name/` that contains a fresh new project based on the most recent stable Symfony version available. In addition, the installer checks if your system meets the technical requirements to execute Symfony applications. If not, you'll see the list of changes needed to meet those requirements.

## Simplest example in Symfony

1) Install symfony correctly as guided above.
2) Start symfony server if you are not installed in www directory.
3) Ensure http://localhost:8000 is working if symfony server is used.
4) Now it is ready to play with simplest example.
5) Add following code in a new file /src/AppBundle/Controller/MyController.php in symfony installation dir.
6) Test the example by visiting http://localhost:8000/hello
7) That's all. Next: use twig to render the response.

  

    <?php
    // src/AppBundle/Controller/MyController.php
    
    namespace AppBundle\Controller;
    
    use Sensio\Bundle\FrameworkExtraBundle\Configuration\Route;
    use Symfony\Component\HttpFoundation\Response;
    
    class MyController
    {
        /**
         * @Route("/hello")
         */
        public function myHelloAction()
        {
            return new Response(
                '<html><body>
                       I\'m the response for request <b>/hello</b>
                 </body></html>'
            );
        }
    }

## Creating page
> Before continuing, make sure you've read the [Installation][1] chapter and can access your new Symfony app in the browser.

Suppose you want to create a page - /lucky/number - that generates a lucky (well, random) number and prints it. To do that, create a "Controller class" and a "controller" method inside of it that will be executed when someone goes to /lucky/number

```php
// src/AppBundle/Controller/LuckyController.php
namespace AppBundle\Controller;

use Sensio\Bundle\FrameworkExtraBundle\Configuration\Route;
use Symfony\Component\HttpFoundation\Response;

class LuckyController
{
    /**
     * @Route("/lucky/number")
     */
    public function numberAction()
    {
        $number = rand(0, 100);

        return new Response(
            '<html><body>Lucky number: '.$number.'</body></html>'
        );
    }
}
```


  [1]: http://symfony.com/doc/current/book/installation.html

