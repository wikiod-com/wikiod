---
title: "Update WordPress Manually"
slug: "update-wordpress-manually"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## VIA FTP
 1. Download the desired version of WordPress from www.wordpress.org to your local computer and unzip the file.
    - Also keep a backup of your current version... just in case.

 2. Connect to your website with your favorite FTP client (FileZilla is popular and easy, but any FTP client will be fine). 
    - *Instructions for this are outside the scope of WordPress, but may be found at a future date in the proposed FTP topic*.

 3. Upload the folders (and their contents) titled "wp-admin" and "wp-includes" to their matching directories on your server. Be sure to overwrite the current folders.

    - You can omit uploading the "wp-content" folder unless you choose to use one of the themes included. If you wish to update/upload the default themes included with your chosen version you should upload this folder as well.

 4. Upload the individual files in the home folder (index.php, wp-*.php, etc). 
    - You can omit the files titled "liscense.txt" and "readme.html" as they are not required to function and they can be used as methods of determining your WP version for security exploits.

 5. Visit and log into your website in order to perform any required database updates. 
    - Not all WP updates have DB changes, but some do.

**Note**: This method will create orphaned files that can/will build up over time and may present security risks. Be sure to do a file comparison after completion and delete old files off your server from previous WP versions which are no longer in use.

