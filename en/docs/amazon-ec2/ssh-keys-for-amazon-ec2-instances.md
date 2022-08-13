---
title: "SSH Keys for Amazon EC2 instances"
slug: "ssh-keys-for-amazon-ec2-instances"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Securing your SSH private key
An SSH key has two pieces, the public key and the private key.

The private key:
  - Is usually in a file named `id_rsa`, but it can be given any name.
  - **CANNOT BE REGENERATED IF LOST!!!! Do not lose this file!**
    -  If you lose it, you will not be able to get back into your instance.  (StackOverflow is littered with questions by people who have done this.)
  - **KEEP THIS FILE SECURE.**
    - On Unix/Linux systems, you are required to give it secure permissions or most clients will complain.  `chmod 600 id_rsa`  Its parent directories should also not be world-writable.    
    - Do not share it with anyone.
    - Do not check it into a shared GitHub repo.

The public key:
  - Is usually in a file named `id_rsa.pub`, but it can be given any name.
  - Can be shared
  - Can be regenerated from the private key.  `ssh-keygen -y -f ~/.ssh/id_rsa`
  - Needs to be added to the `$HOME/.ssh/authorized_keys` on the remote system to enable passwordless login with the private key.  (AWS does this for you at instance creation for the keypair you select.  They cannot update this file for you after instance creation.)

