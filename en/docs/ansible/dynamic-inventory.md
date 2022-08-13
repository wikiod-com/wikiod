---
title: "Dynamic inventory"
slug: "dynamic-inventory"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Environment variables in dynamic inventory won't work, f.e.

    "ansible_ssh_private_key_file": $HOME/.ssh/key.pem"

If the dynamic inventory server side passes `$HOME` for example, replace the variable in the client code (Python):

    json_input.replace("$HOME", os.environ.get("HOME"))

## Dynamic inventory with login credentials
Pass dynamic inventory to `ansible-playbook`:

    ansible-playbook -i inventory/dyn.py -l targethost my_playbook.yml

`python inventory/dyn.py` should print out something like this:

    {
      "_meta": {
        "hostvars": {
          "10.1.0.10": {
            "ansible_user": "vagrant",
            "ansible_ssh_private_key_file": "/home/mrtuovinen/.ssh/id_rsa",
            "ansible_port": 22
          },
          "10.1.0.11": {
            "ansible_user": "ubuntu",
            "ansible_ssh_private_key_file": "/home/mrtuovinen/.ssh/id_rsa",
            "ansible_port": 22
          },
          "10.1.0.12": {
            "ansible_user": "steve",
            "ansible_ssh_private_key_file": "/home/mrtuovinen/.ssh/key.pem",
            "ansible_port": 2222
          }
        }
      },
      "vagrantbox": [
        "10.1.0.10"
      ],
      "ubuntubox": [
        "10.1.0.11"
      ],
      "osxbox": [
        "10.1.0.12"
      ]
    }


