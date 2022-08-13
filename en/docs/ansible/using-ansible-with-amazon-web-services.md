---
title: "Using Ansible with Amazon Web Services"
slug: "using-ansible-with-amazon-web-services"
draft: false
images: []
weight: 9956
type: docs
toc: true
---

example-2:
This is serves as an example so just don't copy/past it. Instead, to suit your needs, you should customize its variables; ansible_key, security group rules etc..

example-1: To disable the ssh strict host key checking, a behavior we don't want when automating tasks, we set it to `no` in `ansible.cfg` file. ie: `StrictHostKeyChecking=no`

The `ec2.py` file is a python script that executes and returns your AWS ressources based on the `ec2.ini` which is the configuration file that you need to customize if you want to limit the scope of your project to some particular regions, specific tags etc...

## How to properly configure Ansible to connect to Amazon Web Services
Managing AWS resources that scale up and down runs into the limits of the static inventory host file, that's why we need something dynamic. And that's what [the dynamic inventories][1] are for. Let's start:

Download these [ec2.ini][2] and [ec2.py][3] files to the your project folder:

    cd my_ansible_project 
    wget https://raw.githubusercontent.com/ansible/ansible/devel/contrib/inventory/ec2.py    
    wget https://raw.githubusercontent.com/ansible/ansible/devel/contrib/inventory/ec2.ini

Once done, make the `ec2.py` file executable: 

    chmod +x ec2.py

Now, export your AWS Secret and Access key as environnment variables:

    export AWS_ACCESS_KEY_ID='ABCDEFGHIJKLM'
    export AWS_SECRET_ACCESS_KEY='NOPQRSTUVWXYZ'

To use the `ec2.py` script we need the Python AWS SDK, [`boto`][4] so you need to install it:

    sudo pip install boto

To test if everything is good, try executing the `ec2.py` by listing your resources:

    ./ec2.py --list

you should see something similar to:

    {
      "_meta": {
        "hostvars": {}
      }
    }


Now we want to use the dynamic inventory along with our static hosts file. First, create a folder called `inventory`, add `ec2.py`, `ec2.ini` and our `hosts` file to it then tell Ansible to use that folder as an inventory file:

    mkdir inventory 
    mv ec2.py inventory/ec2.py
    mv ec2.ini inventory/ec2.ini
    mv hosts inventory/hosts

Next we should define project level configuration for Ansible by creating an Ansible config file in your project folder called `ansible.cfg` and adding this:

    [defaults]
    hostfile = inventory
    [ssh_connection]
    pipelining = False
    ssh_args = -o ControlMaster=auto -o ControlPersist=30m -o StrictHostKeyChecking=no

Next we need to configure Ansible to use an SSH key to authenticate access to our EC2 instances. Using an SSH agent is the best way to authenticate with resources, as this makes it easier to manage keys:

    ssh-agent bash 
    ssh-add ~/.ssh/keypair.pem  

That's it! If you followed this, you can test it by using the [`ping` module][5] and then, you will see your running instances that have been configured to use your key responding with pong:

    ansible -m ping all
    11.22.33.44 | success >> {
        "changed": false, 
        "ping": "pong"
    }


  [1]: http://docs.ansible.com/ansible/intro_dynamic_inventory.html#id6
  [2]: https://raw.githubusercontent.com/ansible/ansible/devel/contrib/inventory/ec2.ini
  [3]: https://raw.githubusercontent.com/ansible/ansible/devel/contrib/inventory/ec2.py
  [4]: http://boto.cloudhackers.com/en/latest/
  [5]: http://docs.ansible.com/ansible/ping_module.html

## How to start EC2 instance from official Amazon AMIs, modify it and store it as new AMI
This is a very common workflow when using Ansible for provisioning an AWS EC2 instance. This post assumes a basic understand of Ansible and most importantly, assumes you've properly configured it to connect to AWS.

As [Ansible official documentation insists][1], we are going to use four roles:

1- **ami_find** to get the ami id based on which we will launch our EC2 instance.

2- **ec2_ami_creation** to effectively launch the EC2 instance.

3- **code_deploy** for modifying the instance; this could be anything so we will simply transfer a file to the target machine.

4- **build_ami** to build our new image based on the running ec2 instance.
This post assumes you are at the top level of your Ansible project: `my_ansible_project`

The first role: **ami_find**

    cd my_ansible_project/roles && ansible-galaxy init ami_find

In this role we are going to use the [ec2_ami_find][2] module and as an example, we will search for the an Ubuntu machine and get its **ami_id** ( ami-xxxxxxxx ).  Now edit `my_ansible_project/roles/ami_find/tasks/main.yml` file:

    ---
    - ec2_ami_find:
        name: "ubuntu/images/hvm-ssd/ubuntu-trusty-14.04-amd64-server-*"
        sort: name
        sort_order: descending
        sort_end: 1
        region: "{{ aws_region }}"
      register: ami_find
    - set_fact: ami_ubuntu="{{ ami_find.results[0].ami_id }}"

The second role: **ec2_ami_creation**

Here, we will use the `ami_id` we got from the first role and then launch our new instance based on it:

    cd my_ansible_project/roles && ansible-galaxy init ec2_ami_creation

In this role we are going to use most importantly the [ec2_module][3] to launch our instance. Now edit `my_ansible_project/roles/ec2_ami_creation/tasks/main.yml` file:

    ---
    - ec2_vpc_subnet_facts:
        region: "{{aws_region}}"
      register: vpc
    - name: creation of security group of the ec2 instance
      ec2_group:
        name: example
        description: an example EC2 group
        region: "{{ aws_region }}"
        rules:
          - proto: tcp
            from_port: 22
            to_port: 22
            cidr_ip: 0.0.0.0/0
        state: present
      register: ec2_sg
    
    - name: create instance using Ansible
      ec2:
        key_name: "{{ ansible_key }}"
        group: example
        vpc_subnet_id: "{{vpc.subnets[0].id}}"
        instance_type: "{{ instance_type }}"
        ec2_region: "{{ aws_region }}"
        image: "{{ base_image }}"
        assign_public_ip: yes
        wait: yes
      register: ec2
    
    - set_fact: id={{ec2.instances[0].id}}
    
    - name: adding the newly created instance to a temporary group in order to access it later from another play
      add_host: name={{ item.public_ip }} groups=just_created
      with_items: ec2.instances
    
    - name: Wait for SSH to come up
      wait_for: host={{ item.public_dns_name }} port=22 delay=10 timeout=640 state=started
      with_items: ec2.instances

The third role: **code_deploy**

Here, we will provision this instance, which was added to a group called `just_created`

    cd my_ansible_project/roles && ansible-galaxy init code_deploy

In this role we are going to use the [template_module][4] to transfer a file & write the machine hostname in it. Now edit `my_ansible_project/roles/code_deploy/tasks/main.yml` file:

    ---
    - template: src=my_file.txt.j2 dest=/etc/my_file.txt

then move to templates folder inside your role:

`cd my_ansible_project/roles/templates` and add a file called `my_file.txt.j2` containing:

    my name is {{ ansible_hostname }}` 

**The fourth role:** `build_ami`

We will now create an image of the running instance using the [ec2_ami module][5]. Move to you project folder and:

     cd my_ansible_project/roles && ansible-galaxy init build_ami

Now edit `my_ansible_project/roles/build_ami/tasks/main.yml` file:

    ---
    - ec2_ami:
        instance_id: "{{ instance_id }}"
        wait: yes
        name: Base_Image

Now, I think you have been wondering how to orchestrate all of these roles. Am I right? If so, continue reading.

We will write a playbook, composed of three plays: first play applicable on `localhost` will call our first two roles, second play applicable on our **just_created** group. last role will be applicable on `localhost`. Why `localhost`? When we want to **manage** some AWS ressources, we use our local machine, as simple as that. Next, we will use a vars file in which we will put our variables: `ansible_key`, `aws_region`, etc...

create infrastructure folder at the top of your project and add a file inside it called `aws.yml`:

    ---
    aws_region: ap-southeast-2
    ansible_key: ansible
    instance_type: t2.small

So at the top of your project create `build_base_image.yml` and add this:

    ---
       - hosts: localhost
         connection: local
         gather_facts: False
         vars_files:
           - infrastructure/aws.yml
         roles:
           - ami_find
           - { role: ec2_creation, base_image: "{{ ami_ubuntu }}"}
    
       - hosts: just_created
         connection: ssh
         gather_facts: True
         become: yes
         become_method: sudo
         roles:
           - code_deploy
    
       - hosts: localhost
         connection: local
         gather_facts: False
         vars_files:
           - infrastructure/aws.yml
         roles:
           - { role: new_image, instance_id: "{{ id }}"}

That's it, Dont forget to delete your ressources after testing this, or why not create a role to delete the running instance :-)

  [1]: http://docs.ansible.com/ansible/playbooks_roles.html#roles
  [2]: http://docs.ansible.com/ansible/ec2_ami_find_module.html
  [3]: http://docs.ansible.com/ansible/ec2_module.html
  [4]: http://docs.ansible.com/ansible/template_module.html
  [5]: http://docs.ansible.com/ansible/ec2_ami_module.html

