---
title: "Using Ansible with OpenStack"
slug: "using-ansible-with-openstack"
draft: false
images: []
weight: 9909
type: docs
toc: true
---

OpenStack is an open-source software platform for cloud computing. Linux instances can be launched/stopped manualy using the graphical web interface or automated thanks to ansible's openstack cloud module.

Configuring ansible can be tricky, but once well configured using it is really easy and powerfull for testing and Continuous Integration environment.



## Parameters
| parameters | Comments |
| ------ | ------ |
| hosts: localhost| OpenStack commands are launched from our localhost  |
| gather_facts: False| We dont need to gather information on our localhost  |
| auth_url: https://openstack-identity.mycompany.com/v2.0| use V2.0 URL  |
| state: present| 'present' / 'absent' to create/delete the instance  |
| validate_certs: False| usefull if https uses self signed certificates  |
| network: "{{ NetworkName }}"| (optional)  |
| auto_ip: yes| (optional)  |

- We put the authentication URL directly in the playbook, not in a variable.
URL used in in vars must be escaped.
- Be carefull with authentication URL version use V2.0 instead of V3 in https://openstack-identity.mycompany.com/v2.0.
- In yml files, be very carefull when copy/paste from browser. Check twice the spaces as they are taken into account.
- More details at:  http://docs.ansible.com/ansible/list_of_cloud_modules.html#openstack

## Check your Ansible version
Check the right software versions are installed:

- ansible >=2.0 
- python >=2.6
- shade module for python

```
$ansible --version
ansible 2.2.0.0

$python --version
Python 2.7.5
```

Install 'shade' the python component used to pilot openstack.
```
$pip install shade
```
Note : if you use a company proxy, it's always useful to know the right pip synthax
```
$pip install --proxy proxy_ip:proxy_port shade
```

## Gather informations from OpenStack GUI to configure Ansible
****

Our openstack tenant is already set:
- a virtual lan gives instances private IP
- a virtual router map public IP to private IP
- a security key has been generated
- we have default firewall configuration for ssh and port 80
- we are able to launch an instance thanks to the OpenStack web interface

Let gather all needed informations from this web interface.

Authentication informations can be found in the openstack.rc file. this file can be downloaded using the OpenStack webinterface in [access and security/API Access].

```
$cat openstack.rc
#!/bin/bash
 
# To use an OpenStack cloud you need to authenticate against the Identity
# service named keystone, which returns a **Token** and **Service Catalog**.
# The catalog contains the endpoints for all services the user/tenant has
# access to - such as Compute, Image Service, Identity, Object Storage, Block
# Storage, and Networking (code-named nova, glance, keystone, swift,
# cinder, and neutron).
#
# *NOTE*: Using the 2.0 *Identity API* does not necessarily mean any other
# OpenStack API is version 2.0. For example, your cloud provider may implement
# Image API v1.1, Block Storage API v2, and Compute API v2.0. OS_AUTH_URL is
# only for the Identity API served through keystone.
export OS_AUTH_URL=https://openstack-identity.mycompany.com/v3

# With the addition of Keystone we have standardized on the term **tenant**
# as the entity that owns the resources.
export OS_TENANT_ID=1ac99fef77ee40148d7d5ba3e070caae
export OS_TENANT_NAME="TrainingIC"
export OS_PROJECT_NAME="TrainingIC"

# In addition to the owning entity (tenant), OpenStack stores the entity
# performing the action as the **user**.
export OS_USERNAME="UserTrainingIC"

# With Keystone you pass the keystone password.
echo "Please enter your OpenStack Password: "
read -sr OS_PASSWORD_INPUT
export OS_PASSWORD=$OS_PASSWORD_INPUT

# If your configuration has multiple regions, we set that information here.
# OS_REGION_NAME is optional and only valid in certain environments.
export OS_REGION_NAME="fr"
# Don't leave a blank variable, unset it if it was empty
if [ -z "$OS_REGION_NAME" ]; then unset OS_REGION_NAME; fi 
```  
We get OS_AUTH_URL, OS_TENANT_NAME, OS_USERNAME.

**Authentication API version : OS_AUTH_URL**

Beware of authentication API version. By default v3 is activated, but ansible needs the v2.0. We get the url and set V2.0 instead of V3 : https://openstack-identity.mycompany.com/v2.0

**VM informations**

Create an instance using the OpenStack web interface and get the name for image, flavor, key, network, security group.

Create a ./group_vars/all file with all the needed informations.
```
$vi ./group_vars/all
# Authentication
AuthUserName: UserTrainingIC
AuthPassword: PasswordTrainingIC
TenantName: TrainingIC

# VM infos
ImageName: CentOS-7-x86_64-GenericCloud-1607
FlavorName: m1.1cpu.1gb
InfraKey: KeyTrainingIC
NetworkName: NetPrivateTrainingIC
SecurityGroup: default
```




## Write the ansible playbook to create the instance
Let use 'os_server' command from module 'Cloud' [http://docs.ansible.com/ansible/os_server_module.html].
Variables are defined in ./group_vars/all.
```
$vi launch_compute.yml
- name: launch a compute instance
  hosts: localhost
  gather_facts: False
  tasks:
  - name: Create and launch the VM
    os_server:
      auth:
        auth_url: https://openstack-identity.mycompany.com/v2.0
        username: "{{ AuthUserName }}"
        password: "{{ AuthPassword }}"
        project_name: "{{ TenantName }}"
      state: present
      validate_certs: False
      name: "MyOwnPersonalInstance"
      image: "{{ ImageName }}"
      key_name: "{{ InfraKey }}"
      timeout: 200
      flavor:   "{{ FlavorName }}"
      security_groups: "{{ SecurityGroup }}"
      network: "{{ NetworkName }}"
      auto_ip: yes
```

```
$ ansible-playbook  -s launch_compute.yml
[WARNING]: provided hosts list is empty, only localhost is available
PLAY [launch a compute instance] ***********************************************
TASK [Create and launch the VM] ************************************************
changed: [localhost]
PLAY RECAP *********************************************************************
localhost                  : ok=1    changed=1    unreachable=0    failed=0
```


## Gather informations about our new instance
Use the 'os_server_facts' command from module 'Cloud' [http://docs.ansible.com/ansible/os_server_module.html]. 
Variables are defined in ./group_vars/all and the instance name is in server: "MyOwnPersonalInstance".


```
$vi get_compute_info.yml
- name: Get and print instance IP
  hosts: localhost
  gather_facts: False
  tasks:
  - name: Get VM infos
    os_server_facts:
      auth:
        auth_url: https://openstack-identity.mygroup/v2.0
        username: "{{ AuthUserName }}"
        password: "{{ AuthPassword }}"
        project_name: "{{ TenantName }}"
      validate_certs: False
      server: "MyOwnPersonalInstance"

  - name: Dump all
    debug:
      var: openstack_servers
```
```
$ansible-playbook  -s get_compute_info.yml
[WARNING]: provided hosts list is empty, only localhost is available
PLAY [Get and print instance IP] ***********************************************
TASK [Get VM IP] ***************************************************************
ok: [localhost]
TASK [Affichage] ***************************************************************
ok: [localhost] => {
    "openstack_servers": [
        {
            "OS-DCF:diskConfig": "MANUAL",
            "OS-EXT-AZ:availability_zone": "fr",
            "OS-EXT-STS:power_state": 1,
            "OS-EXT-STS:task_state": null,
[...]
            "volumes": []
        }
    ]
}

PLAY RECAP *********************************************************************
localhost                  : ok=2    changed=0    unreachable=0    failed=0
```
This is very verbose. Lots of information is displayed.
Usually only the IP address is needed to access the new instance via SSH.


## Get your new instance public IP
Instead of printing all the informations, we print only IP address of the first instance whose name is "MyOwnPersonalInstance".
It's usually all we need.
```
$vi get_compute_ip.yml
- name: Get and print instance IP
  hosts: localhost
  gather_facts: False
  tasks:
  - name: Get VM infos
    os_server_facts:
      auth:
        auth_url: https://openstack-identity.mycompany.com/v2.0
        username: "{{ AuthUserName }}"
        password: "{{ AuthPassword }}"
        project_name: "{{ TenantName }}"
      validate_certs: False
      server: "MyOwnPersonalInstance"

  - name: Dump IP
    debug:
      var: openstack_servers[0].interface_ip
```

## Delete our instance
To delete our instance, reuse the os_server command with all authentication information and simply replace ' state: present' by ' state: absent'.
```
$vi stop_compute.yml
- name: launch a compute instance
  hosts: localhost
  gather_facts: False
  tasks:
  - name: Create and launch the VM
    os_server:
      auth:
        auth_url: https://openstack-identity.mygroup/v2.0
        username: "{{ AuthUserName }}"
        password: "{{ AuthPassword }}"
        project_name: "{{ ProjectName }}"
      state: absent
      validate_certs: False
      name: "{{ TPUser }}"
      timeout: 200
```

