---
title: "How to Setup Three Node Mongo Replica using Docker Image and Provisioned using Chef"
slug: "how-to-setup-three-node-mongo-replica-using-docker-image-and-provisioned-using-chef"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

This Documentation describe how to build a three node Mongo replica set using Docker Image  and auto provisioned using Chef. 




## Build Step
Steps:

1. Generate a Base 64 keyfile for Mongo node authentication. Place this file in chef data_bags

2. Go to chef suppermarket and download docker cookbook. Generate a custom cookbook (e.g custom_mongo) and add depends 'docker', '~> 2.0' to your cookbook's metadata.rb

3. Create an attributes and recipe in your custom cookbook

4. Initialise Mongo to form Rep Set cluster


**Step 1: Create Key file** 

create data_bag called mongo-keyfile and item called keyfile. This will be in the data_bags directory in chef. Item content will be as below

    openssl rand -base64 756 > <path-to-keyfile>

keyfile item content

    {
      "id": "keyfile",
      "comment": "Mongo Repset keyfile",
      "key-file": "generated base 64 key above"
    }
**Step 2: Download docker cookbook from chef supper market and then create custom_mongo cookbook** 

    knife cookbook site download docker 
    knife cookbook create custom_mongo

in metadat.rb of custom_mongo add

    depends          'docker', '~> 2.0'

**Step 3: create attribute and recipe**

 Attributes

    default['custom_mongo']['mongo_keyfile'] = '/data/keyfile' 
    default['custom_mongo']['mongo_datadir'] = '/data/db'
    default['custom_mongo']['mongo_datapath'] = '/data'
    default['custom_mongo']['keyfilename'] = 'mongodb-keyfile'

Recipe

    #
    # Cookbook Name:: custom_mongo
    # Recipe:: default
    #
    # Copyright 2017, Innocent Anigbo
    #
    # All rights reserved - Do Not Redistribute
    #
    
    data_path = "#{node['custom_mongo']['mongo_datapath']}"
    data_dir = "#{node['custom_mongo']['mongo_datadir']}"
    key_dir = "#{node['custom_mongo']['mongo_keyfile']}"
    keyfile_content = data_bag_item('mongo-keyfile', 'keyfile')
    keyfile_name = "#{node['custom_mongo']['keyfilename']}"
    
    #chown of keyfile to docker user
    execute 'assign-user' do
     command "chown 999 #{key_dir}/#{keyfile_name}"
     action :nothing
    end
    
    #Declaration to create Mongo data DIR and Keyfile DIR
    %W[ #{data_path} #{data_dir} #{key_dir} ].each do |path|
    directory path do
      mode '0755'
      end
    end
    
    #declaration to copy keyfile from data_bag to keyfile DIR on your mongo server
    file "#{key_dir}/#{keyfile_name}" do
      content keyfile_content['key-file']
      group 'root'
      mode '0400'
      notifies :run, 'execute[assign-user]', :immediately
    end
    
    #Install docker
    docker_service 'default' do
      action [:create, :start]
    end
    
    #Install mongo 3.4.2
    docker_image 'mongo' do
      tag '3.4.2'
      action :pull
    end

Create Role called mongo-role in role directory

    {
      "name": "mongo-role",
      "description": "mongo DB Role",
      "run_list": [
        "recipe[custom_mongo]"
      ]
    }

Add role above to the three mongo nodes run list

    knife node run_list add FQDN_of_node_01 'role[mongo-role]'
    knife node run_list add FQDN_of_node_02 'role[mongo-role]'
    knife node run_list add FQDN_of_node_03 'role[mongo-role]'

**Step 4: Initialise the three node Mongo to form repset**

   I'm assuming that the above role has already been applied on all three Mongo nodes.
On node 01 only, Start Mongo with --auth to enable authentication

 
    docker run --name mongo -v /data/db:/data/db -v /data/keyfile:/opt/keyfile --hostname="mongo-01.example.com" -p 27017:27017 -d mongo:3.4.2 --keyFile /opt/keyfile/mongodb-keyfile --auth

Access the interactive shell of running docker container on node 01 and Create admin user

 

    docker exec -it mongo /bin/sh
        mongo
        use admin
        db.createUser( {
             user: "admin-user",
             pwd: "password",
             roles: [ { role: "userAdminAnyDatabase", db: "admin" } ]
           });

Create root user

    db.createUser( {
             user: "RootAdmin",
             pwd: "password",
             roles: [ { role: "root", db: "admin" } ]
           });

Stop and Delete the Docker container created above on node 01. This will not affect the data and keyfile in the host DIR. After deleting start Mongo again on node 01 but this time with with repset flag

    docker rm -fv mongo
    docker run --name mongo-uat -v /data/db:/data/db -v /data/keyfile:/opt/keyfile --hostname="mongo-01.example.com" -p 27017:27017 -d mongo:3.4.2 --keyFile /opt/keyfile/mongodb-keyfile --replSet "rs0"
    
now start mongo on Node 02 and 03 with the rep set flag

    docker run --name mongo -v /data/db:/data/db -v /data/keyfile:/opt/keyfile --hostname="mongo-02.example.com" -p 27017:27017 -d mongo:3.4.2 --keyFile /opt/keyfile/mongodb-keyfile --replSet "rs0"
    docker run --name mongo -v /data/db:/data/db -v /data/keyfile:/opt/keyfile --hostname="mongo-03.example.com" -p 27017:27017 -d mongo:3.4.2 --keyFile /opt/keyfile/mongodb-keyfile --replSet "rs0"
    
Authenticate with the root user on Node 01 and initiate the replica set

    use admin
    db.auth("RootAdmin", "password");
    rs.initiate()
    
On node 01 add Node 2 and 3 to the Replica Set to form repset0 cluster
    
    rs.add("mongo-02.example.com")
    rs.add("mongo-03.example.com")

**Testing**

On the primary run db.printSlaveReplicationInfo() and observe the SyncedTo and Behind the primary time. The later should be 0 sec as below

Output

   

     rs0:PRIMARY> db.printSlaveReplicationInfo()
        source: mongo-02.example.com:27017
                syncedTo: Mon Mar 27 2017 15:01:04 GMT+0000 (UTC)
                0 secs (0 hrs) behind the primary
        source: mongo-03.example.com:27017
                syncedTo: Mon Mar 27 2017 15:01:04 GMT+0000 (UTC)
                0 secs (0 hrs) behind the primary


I hope this helps someone

