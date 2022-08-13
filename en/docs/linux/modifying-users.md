---
title: "Modifying Users"
slug: "modifying-users"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Parameters
|Parameter | Details |
| -------  |  -----  |
| username | The name of the user. Do not use capital letters, do not use dots, do not end it in dash, it must not include colons, no special characters. Cannot start with a number. |

 - You cannot remove a logged in user
 - To modify any user but your own, you need root privileges

## Setting your own password
````
passwd
````

## Setting another user's password
Run the following as root:
````
passwd username
````

## Adding a user
Run the following as root:
````
useradd username
````

## Removing a user
Run the following as root:
````
userdel username
````

## Removing a user and its home folder
Run the following as root:
````
userdel -r username
````

## Listing groups the current user is in
````
groups
````
More detailed information about user and group numerical IDs can be found with the `id` command.

## Listing groups a user is in
````
groups username
````
More detailed information about user and group numerical IDs can be found with `id username`.

