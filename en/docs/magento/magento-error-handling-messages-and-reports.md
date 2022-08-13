---
title: "Magento error handling, messages and reports"
slug: "magento-error-handling-messages-and-reports"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Error Log Locations
===================

 `/var/log/`
-----------

> Typically the system.log and exception.log file will exist in the `/var/log/` folder. These contain most of the information you will need. You can check to see if these are enabled and what the names of the exception and system log are by going to `System > Configuration > System > Developer > Log Settings`.
>
> [![log settings area in Magento 1.x admin dashboard][1]][1]
<br/>

`/var/report/`
--------------
> Report files are generated in this folder after a user has encountered an error. Each file only includes the details for one error. These are used in order to hide the error details from the public. On the error page there will be a report number which is a reference to the corresponding file with the same name in the `/var/report/` folder.
>
> [![example of Magento error page][2]][2]


  [1]: https://i.stack.imgur.com/e4BrH.png
  [2]: https://i.stack.imgur.com/r1h6w.png

## Enable displaying of error reporting
In Index page change the following:

    error_reporting(E_ALL | E_STRICT);

to

    error_reporting(E_ALL);

Set `$_SERVER['MAGE_IS_DEVELOPER_MODE'] = true`

and uncomment this line (remove the `#`)

    #ini_set('display_errors', 1);

You can also Set Dev Mode using `SetEnv` in your `.htaccess` file

To make the error readable and easier to understand, do the following:
1. Open your Magento installation directory. Go to the errors folder.
2. Rename `local.xml.sample` file to `local.xml`.
3. Refresh the error page in browser.

