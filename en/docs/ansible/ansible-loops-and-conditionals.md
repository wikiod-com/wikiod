---
title: "Ansible Loops and Conditionals"
slug: "ansible-loops-and-conditionals"
draft: false
images: []
weight: 9938
type: docs
toc: true
---

Official docs explains playbook conditionals.
- http://docs.ansible.com/ansible/playbooks_conditionals.html

Ansible (github)
- https://github.com/marxwang/ansible-learn-resources

## What kinds of conditionals to use?

Use Conditionals via (syntax is in `[brackets]`): 

- when [**when:**]
        
      Task:
         - name: run if operating system is debian
           command: echo "I am a Debian Computer"
           when: ansible_os_family == "Debian"
- loops [**with_items:**]
- loops [**with_dicts:**]
- Custom Facts [ **when:** my_custom_facts == '1234']
- Conditional imports
- Select files and Templates based on variables


## [When] Condition:  `ansible_os_family` Lists

# Common use 
 - when: ansible_os_family == "CentOS"
 - when: ansible_os_family == "Redhat"
 - when: ansible_os_family == "Darwin"
 - when: ansible_os_family == "Debian"
 - when: ansible_os_family == "Windows"

# All Lists
based on discuss here http://comments.gmane.org/gmane.comp.sysutils.ansible/4685

 

    OS_FAMILY = dict(
                RedHat = 'RedHat',
                Fedora = 'RedHat', 
                CentOS = 'RedHat', 
                Scientific = 'RedHat',
                SLC = 'RedHat', 
                Ascendos = 'RedHat', 
                CloudLinux = 'RedHat', 
                PSBM = 'RedHat',
                OracleLinux = 'RedHat', 
                OVS = 'RedHat', 
                OEL = 'RedHat', 
                Amazon = 'RedHat',
                XenServer = 'RedHat', 
                Ubuntu = 'Debian', 
                Debian = 'Debian', 
                SLES = 'Suse',
                SLED = 'Suse', 
                OpenSuSE = 'Suse', 
                SuSE = 'Suse', 
                Gentoo = 'Gentoo',
                Archlinux = 'Archlinux', 
                Mandriva = 'Mandrake', 
                Mandrake = 'Mandrake',
                Solaris = 'Solaris', 
                Nexenta = 'Solaris',  
                OmniOS = 'Solaris', 
                OpenIndiana = 'Solaris',
                SmartOS = 'Solaris', 
                AIX = 'AIX', 
                Alpine = 'Alpine', 
                MacOSX = 'Darwin',
                FreeBSD = 'FreeBSD', 
                HPUX = 'HP-UX'
            )

## When Condition
# Basic Usage
Use the when condition to control whether a task or role runs or is skipped. This is normally used to change play behavior based on facts from the destination system. Consider this playbook:
    
    - hosts: all
      tasks:
        - include: Ubuntu.yml
          when: ansible_os_family == "Ubuntu"
        
        - include: RHEL.yml
          when: ansible_os_family == "RedHat"

Where `Ubuntu.yml` and `RHEL.yml` include some distribution-specific logic. 

Another common usage is to limit results to those in certain Ansible inventory groups. Consider this inventory file:

    [dbs]
    mydb01

    [webservers]
    myweb01

And this playbook:
    
    - hosts: all
      tasks:
        - name: Restart Apache on webservers
          become: yes
          service:
            name: apache2
            state: restarted
          when: webservers in group_names
This is using the `group_names` [magic variable][1].

# Conditional Syntax and Logic

## Single condition

**Syntax**

`when: (condition)`

**Example**  

 - `when: ansible_os_family == "Debian"`
 - `when: ansible_pkg_mgr == "apt"`
 - `when: myvariablename is defined`

## Boolean Filter

**Example**

`when: result|failed`

## Multiple Conditions
**Syntax**

`When: condition1 and/or condition2`

**Example (simple)**  

`when: ansible_os_family == "Debian" and ansible_pkg_mgr == "apt"`

**Example (complex)**

Use parentheses for clarity or to control precedence. "AND" has a higher precedence than "OR".

Clauses can span lines:

    when:
      ansible_distribution in ['RedHat', 'CentOS', 'ScientificLinux'] and
      (ansible_distribution_version|version_compare('7', '<') or
      ansible_distribution_version|version_compare('8', '>='))
      or
      ansible_distribution == 'Fedora'
      or
      ansible_distribution == 'Ubuntu' and
      ansible_distribution_version|version_compare('15.04', '>=')
  Note the use of parentheses to group the "or" in the first distribution check.


  [1]: http://docs.ansible.com/ansible/playbooks_variables.html#magic-variables-and-how-to-access-information-about-other-hosts

## Get `ansible_os_family` and `ansible_pkg_mgr` with setup
We can get facts (`ansible_os_family`, `ansible_pkg_mgr`) with Ad-Hoc command of setup module and filter.

* ansible_os_family:

        $ ansible all -m setup -a 'filter=ansible_os_family'
        ra.local | SUCCESS => {
            "ansible_facts": {
                "ansible_os_family": "Debian"
            },
            "changed": false
        }

* ansible_pkg_mgr:

        $ ansible all -m setup -a 'filter=ansible_pkg_mgr'
        debian.local | SUCCESS => {
            "ansible_facts": {
                "ansible_pkg_mgr": "apt"
            },
            "changed": false
        }




## Simple "When" Example(s)
Given:

    ---
    variable_name: True

Then, these tasks with always run.

    - name: This is a conditional task
      module: src=/example/ dest=/example
      when: variable_name 

    - name: This is a conditional task
      module: src=/example/ dest=/example
      when: True

This task will never run.

    - name: This is a conditional task
      module: src=/example/ dest=/example
      when: False

## Using until for a retry looping alive check
This is an example of using until/retries/delay to implement an alive check for a webapp that is starting up. It assumes that there will be some period of time (up to 3 minutes) where the webapp is refusing socket connections. After that, it checks the /alive page for the word "OK". It also delegates the retrieval of the URL to the localhost running ansible. This makes sense as the final task in a deployment playbook. 

    ---
    - hosts: my-hosts
      tasks:
      - action: uri url=http://{{ ansible_all_ipv4_addresses }}:8080/alive return_content=yes
        delegate_to: localhost
        register: result
        until: "'failed' not in result and result.content.find('OK') != -1"
        retries: 18
        delay: 10

The until retry pattern can be used with any action; Ansible documentation provides an example of waiting until a certain shell command returns a desired result: http://docs.ansible.com/ansible/playbooks_loops.html#do-until-loops. 

