---
title: "Ansible install mysql"
slug: "ansible-install-mysql"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

How use ansible to install mysql binary file


## How use ansible to install mysql binary file
---
- hosts: mysql
  tasks:

    - name: Add mysql user
      user:
        name: mysql
        shell: /sbin/nologin
    
    - name: install the latest version of libselinux-python
      yum:
        name: libselinux-python
        state: latest
    - name: install perl
      yum:
        name: perl
        state: latest
    
    - name: remove the mysql-libs package
      yum:
        name: mysql-libs
        state: absent
    
    
    - name: download and unarchive tar
      unarchive:
         src=/tmp/mysql-5.6.35-linux-glibc2.5-x86_64.tar.gz
         dest=/tmp
         copy=yes
    
    
    - name: Move mysql paceage to specified directory
      command: creates="/usr/local/mysql" mv /tmp/mysql-5.6.35-linux-glibc2.5-x86_64 /usr/local/mysql
    
    - name: chown mysql mysql /usr/local/mysql
      file: path=/usr/local/mysql owner=mysql group=mysql recurse=yes
    
    
    - name: Add lib to ld.so.conf
      lineinfile: dest=/etc/ld.so.conf line="/usr/local/mysql/lib/"
    
    - name: ldconfig
      command: /sbin/ldconfig
    
    - name: Mkdir mysql_data_dir
      file: path=/data/mysql/3306/{{ item }} state=directory owner=mysql group=mysql
      with_items:
        - data
        - logs
        - tmp
    
    - name: Copy mysql my.cnf
      copy: src=/etc/my.cnf dest=/etc/my.cnf
    
    - name: Copy mysql my.cnf
      copy: src=/etc/my.cnf dest=/usr/local/mysql/my.cnf
    
    
    - name: Init mysql db
      command: /usr/local/mysql/scripts/mysql_install_db  \
        --user=mysql  \
        --basedir=/usr/local/mysql \
        --datadir=/data/mysql/3306/data
    
    - name: Add mysql bin to profile
      lineinfile: dest=/etc/profile line="export PATH=$PATH:/usr/local/mysql/bin/"
    
    - name: Source profile
      shell: executable=/bin/bash source /etc/profile
    
    - name: Copy mysqld to init when system start
      command: cp -f /usr/local/mysql/support-files/mysql.server /etc/init.d/mysqld
    
    - name: Add mysqld to system start
      command: /sbin/chkconfig --add mysqld
    
    - name: Add mysql to system start when init 345
      command: /sbin/chkconfig --level 345 mysqld on
    
    - name: Retart mysql
      service: name=mysqld state=restarted
    

