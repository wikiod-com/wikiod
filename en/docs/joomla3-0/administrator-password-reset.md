---
title: "Administrator Password Reset"
slug: "administrator-password-reset"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Set password for admin
Here is a workaround to temporary reset your password for your account. 

1. Login to your hosting panel and use the database tool that is available (probably PHPmyAdmin). 

2. Load the appropriate database and then go to the `jos_users` table. (The database prefix might be different for your case).
3. Go to the browse view of the PHPmyAdmin, so you get view of the users records. 
4. Locate your account and click to edit, or click directly in the 'password' field (Usually PHPmyAdmin, will make the field editable with this). 
5. In the password field paste the value below: 

> $2y$10$JszAMznv7U2Q4VETQdw7n.CX/HPekafC8sxa9.n0V2gp/t/.xvHYi

6. Do not apply any special functions for saving the field and it will set your account password to admin. Save the record.



You can now login with your account, using as password: ***admin***

After login, make sure you set your desired password.

