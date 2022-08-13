---
title: "VBA Security"
slug: "vba-security"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Password Protect your VBA
Sometimes you have sensitive information in your VBA (e.g., passwords) that you don't want users to have access to. You can achieve basic security on this information by password-protecting your VBA project.

Follow these steps:

1. Open your Visual Basic Editor (Alt + F11)
2. Navigate to Tools -> VBAProject Properties...
3. Navigate to the Protection tab
4. Check off the "Lock project for viewing" checkbox
5. Enter your desired password in the Password and Confirm Password textboxes

Now when someone wants to access your code within an Office application, they will first need to enter the password. Be aware, however, that even a strong VBA project password is trivial to break.

