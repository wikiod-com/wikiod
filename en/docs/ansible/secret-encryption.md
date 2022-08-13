---
title: "Secret encryption"
slug: "secret-encryption"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

Ansible offers [Vault](http://docs.ansible.com/ansible/playbooks_vault.html) (not to be mistaken with [HashiCorp Vault](https://www.vaultproject.io/)!) to handle sensitive data encryption.
Vault primarily targets to encrypt any structured data such as variables, tasks, handlers. 

## Encrypting sensitive structured data
First, create a key file, e.g., `vault_pass_file`, which ideally contains a long sequence of random characters.
In linux systems you could use `pwgen` to create a random password file:

```
pwgen 256 1 > vault_pass_file
```

Then, use this file to encrypt sensitive data, e.g., `groups_vars/group.yml`:

```
ANSIBLE_VAULT_PASSWORD_FILE=vault_pass_file ansible-vault encrypt group_vars/group.yml
```

From now on, in order to run a playbook you need the `vault_pass_file`:

```
ANSIBLE_VAULT_PASSWORD_FILE=vault_pass_file ansible-playbook -i inventories/nodes my-playbook.yml 
```

Note, you could also use the flag `--vault-password-file vault_pass_file` instead of setting the `ANSIBLE_VAULT_PASSWORD_FILE` environment variable.

In order to edit or decrypt the secret on disk you can use `ansible-vault edit` and `ansible-vault decrypt` respectively.

## Using lookup pipes to decrypt non-structured vault-encrypted data
With Vault you can also encrypt non-structured data, such as private key files and still be able to decrypt them in your play with the `lookup` module. 

```
---

- name: Copy private key to destination
  copy:
    dest=/home/user/.ssh/id_rsa
    mode=0600
    content=lookup('pipe', 'ANSIBLE_VAULT_PASSWORD_FILE=vault_pass_file ansible-vault view keys/private_key.enc')
```

## Using local_action to decrypt vault-encrypted templates
You can run a play which relies on vault-encrypted templates by using the `local_action` module.

```
---

- name: Decrypt template
  local_action: "shell {{ view_encrypted_file_cmd }} {{ role_path }}/templates/template.enc > {{ role_path }}/templates/template"
  changed_when: False

- name: Deploy template
  template:
    src=templates/template
    dest=/home/user/file

- name: Remove decrypted template
  local_action: "file path={{ role_path }}/templates/template state=absent"
  changed_when: False
```

Please note the `changed_when: False`. 
This is important in case you run idempotence tests with your ansible roles - otherwise each time you run the playbook a change is signaled. 
In `group_vars/all.yml` you could set a global decrypt command for reuse, e.g., as `view_encrypted_file_cmd`.


**group_vars/all.yml**
```
---

view_encrypted_file_cmd: "ansible-vault --vault-password-file {{ lookup('env', 'ANSIBLE_VAULT_PASSWORD_FILE') }} view"
```

Now, when running a play you need to set the `ANSIBLE_VAULT_PASSWORD_FILE` environment variable to point to your vault password file (ideally with an absolute path).

