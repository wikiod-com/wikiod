---
title: "Roles"
slug: "roles"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Role dependencies
Roles also enable you to define other roles as a dependency by creating a `meta/main.yml` file with a `dependencies` block:

    dependencies:
      - role: common

It's also possible to pass a value to a parameter/variable in the dependent role:

    dependencies:
      - { role: common, some_parameter: 3 }

Or even execute the dependent role conditionally:

    dependencies:
      - { role: common, some_parameter: 3 }
      - { role: sshd, enable_sshd: false,
                             when: environment == 'production' }

Dependent roles are always executed before the roles that depend on them. Also, they are only executed once. If two roles state the same one as their dependency, it is only executed the first time. 

Imagine the roles role1, role2 and role3 with the folling `meta/main.yml`'s:    
role1/meta/main.yml:
    
    dependencies:
        - role: role3

role2/meta/main.yml:
    
    dependencies:
        - role: role3

When executing role1 and role2 in the same playbook (with role1 called before role2), the execution order would be the following:

    role3 -> role1 -> role2

You may override this behaviour by specifying `allow_duplicates: yes` in `meta/main.yml` of role1 and role2. The resulting execution order would the be:
    
    role3 -> role1 -> role3 -> role2

 
    

## Using roles
Ansible uses the concept of [roles][1] to better allow modular code and avoid repeating yourself.

A role is simply a folder structure that Ansible knows where to load vars files, tasks and handlers from. An example might look something like this:

    apache/
    ├── defaults
    │   └── main.yml
    ├── files
    │   ├── mod-pagespeed-stable_current_i386.deb
    │   ├── mod-pagespeed-stable_current_i386.rpm
    │   ├── mod-pagespeed-stable_current_amd64.deb
    |   └── mod-pagespeed-stable_current_x86_64.rpm
    ├── tasks
    │   ├── debian.yml
    │   ├── main.yml
    │   └── redhat.yml
    ├── templates
    │   ├── httpd.conf.j2
    │   └── sites-available
    │       └── virthualhost.conf.j2
    └── vars
        ├── debian
        └── redhat

You can then use the role with a basic playbook that just looks like this:

    - hosts: webservers
      roles:
         - apache

When you run Ansible against this playbook it will target all the hosts in the `webservers` group and run the `apache` role defined above against it, automatically loading any default variables for the role and running all the tasks included in `tasks/main.yml`. Ansible also knows to look for certain types of files in role friendly locations:

- If roles/x/tasks/main.yml exists, tasks listed therein will be added to the play
- If roles/x/handlers/main.yml exists, handlers listed therein will be added to the play
- If roles/x/vars/main.yml exists, variables listed therein will be added to the play
- If roles/x/meta/main.yml exists, any role dependencies listed therein will be added to the list of roles (1.3 and later)
- Any copy, script, template or include tasks (in the role) can reference files in roles/x/{files,templates,tasks}/ (dir depends on task) without having to path them relatively or absolutely

  [1]: http://docs.ansible.com/ansible/playbooks_roles.html

## Separating distribution specific tasks and variables inside a role
We can easily separate distribution specific tasks and variables into different dedicated .yml files. 
Ansible helps us to automatically identify the target hosts distribution via `{{ ansible_distribution }}` and `{{ ansible_distribution_version }}`, so we just have to name the distribution dedicated .yml files accordingly.

For Ubuntu Xenial the basic role dir tree would then look something like that:

```
role
├── tasks
│   ├── main.yml
│   └── Ubuntu16.04.yml
└── vars
    └── Ubuntu16.04.yml
```

Inside the `tasks/main.yml` we can now automatically include the proper variables and tasks for the target hosts distribution.

**tasks/main.yml**
```
---

- name: include distribution specific vars
  include_vars: "{{ ansible_distribution }}{{ ansible_distribution_version }}.yml"

- name: include distribution specific install
  include: "{{ ansible_distribution }}{{ ansible_distribution_version }}.yml"
```

Inside `tasks/Ubuntu16.06.yml` and `vars/Ubuntu16.04.yml` we can now define tasks and variables for Ubuntu Xenial respectively.

