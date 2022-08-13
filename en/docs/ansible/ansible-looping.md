---
title: "Ansible Looping"
slug: "ansible-looping"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Nested loops
You can create nested loops using `with_nested`.


from vars:

    keys:
      - key1
      - key2
      - key3
      - key4

then the loop:

    - name: Distribute SSH keys among multiple users
      lineinfile: dest=/home/{{ item[0] }}/.ssh/authorized_keys line={{ item[1] }} state=present
      with_nested:
        - [ 'calvin', 'josh', 'alice' ]
        - '{{ keys }}'

This task will loop over each user and populate their `authorized_keys` file with the 4 keys defined in the list.

## with_items - simple list
A `with_items` loop in ansible can be used to easily loop over values.

    - name: Add lines to this file
      lineinfile: dest=/etc/file line={{ item }} state=present
      with_items:
        - Line 1
        - Line 2
        - Line 3

## with_items - predefined list
You can also loop over a variable list. 

From vars:

    favorite_snacks:
      - hotdog
      - ice cream
      - chips
and then the loop:

    - name: create directories for storing my snacks
      file: path=/etc/snacks/{{ item }} state=directory
      with_items: '{{ favorite_snacks }}'

If you are using Ansible 2.0+ you must use quotes around the call to the variable.
      

## with_items - dictionary
You can use a dictionary for a slightly more complex loop.

    - name: manage packages
      package: name={{ item.name }} state={{ item.state }}
      with_items:
        - { name: tree, state: present }
        - { name: nmap, state: present }
        - { name: apache2, state: absent }

## with_items - predefined dictionary
It is possible to create more complex loops with dictionaries.

From vars:

    packages:
      - present: tree
      - present: nmap
      - absent: apache2

then the loop:

    - name: manage packages
      package: name={{ item.value }} state={{ item.key }}
      with_items: '{{ packages }}'

Or, if you don't like to use the key value:

vars:

    packages:
      - name: tree
        state: present
      - name: nmap
        state: present
      - name: apache2
        state: absent
then the loop:

    - name: manage packages
      package: name={{ item.name }} state={{ item.state }}
      with_items: '{{ packages }}'

