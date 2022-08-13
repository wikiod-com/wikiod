---
title: "Deploy a docker container image using ECS"
slug: "deploy-a-docker-container-image-using-ecs"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

Before you can add ECS instances to a cluster you must first go to the EC2 Management Console and create `ecs-optimized` instances with an IAM role that has the `AmazonEC2ContainerServiceforEC2Role` policy attached.

1. Go to your [EC2 Dashboard][1], and click the `Launch Instance` button.
2. Under `Community AMIs`, search for `ecs-optimized`, and select the one that best fits your project needs. Any will work. Click next.
3. When you get to `Configure Instance Details`, click on the `create new IAM role link` and create a new role called `ecsInstanceRole`.
4. Attach the `AmazonEC2ContainerServiceforEC2Role` policy to that role.
5. By default, your container instance launches into your `default` cluster. If you want to launch into your own cluster instead of the default, choose the `Advanced Details` list and paste the following script into the `User data` field, replacing `your_cluster_name` with the name of your cluster.


    #!/bin/bash
    echo ECS_CLUSTER=your_cluster_name >> /etc/ecs/ecs.config

6. Then, finish configuring your ECS Instance. 

*NOTE: If you a creating a web server you will want to create a `securityGroup` to allow access to port 80.*

7. Create a Repository: `aws ecr create-repository --repository-name example-repository`
8. Authenticate your Docker client to your registry: `aws ecr get-login --region us-east-1 | sh`
9. Build your Docker image: `docker build -t example-image .`
10. Tag your image so you can push the image to this repository: `docker tag example-image:latest example-namespace/example-image:latest`
11. Push this image to your newly created AWS repository: `docker push example-namespace/example-image:latest`
12. Register an ECS Task Definition: `aws ecs register-task-definition --cli-input-json example-task.json`
13. Run the task: `aws ecs run-task --task-definition example-task`


  [1]: https://console.aws.amazon.com/ec2/

## Deploy a sample application on AWS ECS service as a proof of concept
Follow following steps to try out a sample application on AWS ECS service as a proof of concept.
------------------------------------------------------------------------

1.    Login to AWS management console and go to **AWS service catalog - > Compute - > Ec2**
2.    Create a VM(EC2 instance) using amazon linux 64 bit OS, this we will use to configure docker, git, AWS ECS agent tool and other tools. We will also use the same VM as a node in ECS cluster to deploy container based applications. Follow below steps to create a VM.  
a)    Follow usual steps to create a EC2 instance, give special embhasic on subsequent steps during EC2 instance creation.  
b)    Select a IAM role with least following permissions –   
AmazonEC2ContainerServiceforEC2Role  
c)    Make sure java is installed on the VM  
 [![enter image description here][1]][1]
3.    Installing docker  [execute below commands]  
first update the yum package repository 

    sudo yum update –y
now to install docker execute yum install  

    sudo yum install -y docker

4.    Start docker service  

    sudo service docker start

5.    Add the ec2-user to the docker group so you can execute Docker commands without using sudo.  

    sudo usermod -a -G docker ec2-user

6.    Log out from the EC2 and log back in again to pick up the new docker group permissions.
7.    Verify that the ec2-user can run Docker commands without sudo.  

    docker info

8.    Installing Git  

    sudo yum install -y git

9.    Clone the sample PHP application on the Ec2 instance from git. We will use this application for our POC.    

    git clone https://github.com/awslabs/ecs-demo-php-simple-app  

    cd ecs-demo-php-simple-app  

 verify that Dockerfile exists by listing the directory contents
  

    ls

10.    Go to AWS service catalog -> Compute -> Ec2 Container Service  
11.    Click on Get Started  
 [![enter image description here][2]][2]  
12.    Click on cancel  
 [![enter image description here][3]][3]  
13.    Click repositories from Repositories menu in left  
 [![enter image description here][4]][4]  
14.    Click on Get Started  

 [![enter image description here][5]][5]
15.    Enter repository name and click next  
 [![enter image description here][6]][6]
16.    Configure Ec2 tools  

    aws configure

provide AWS Access Key ID, Secret Access key, default region name as per your account
17.    Build, tag, and push Docker image  
a)    Retrieve the docker login command that you can use to authenticate your Docker client to your registry:  

    aws ecr get-login --region us-east-1

b)    Run the  command return as output of previous step  
18.    Build the Docker image from your Dockerfile. (Recall Step 9, where you downloaded a sample docker app)  
a)    

    docker build -t amazon-ecs-sample .

 (Note the “.” stands for current directory)  
b)    Run docker images to verify that the image was created correctly and that the image name contains a repository that you can push your changes to the docker image  

    docker images
[![enter image description here][7]][7]
 
c)    Run the newly built image. The -p 80:80 option maps the exposed port 80 on the container to port 80 on the host system(Ec2 instance in this case).  
    

    docker run -p 80:80 amazon-ecs-sample

   
  Ignore the warning “apache2: Could not reliably determine the server's fully qualified domain name, using 172.17.0.2 for ServerName”
19.    Try to access the sample application webpage on browser, make sure port 80 is open in security groups associated with the instance   

    http://<ec2-instance-dns-address>

  
   [![enter image description here][8]][8]

20.    Press ctrl + c key, this will stop the docker image. The sample application should not be accessible.  
21.    Now after successfully verifying our sample docker application, we will try to configure a cluster to run the sample application automatically. Also, for the demo purpose we will try to use the existing ec2 instance as a node in the cluster. This can be achieved by installing a agent program on the ec2 instance.  
22.    Installing Amazon ECS Container Agent on the ec2 instance  
a)    

    sudo yum install -y ecs-init

  
b)    Restart the docker daemon  

    sudo service docker restart

  
c)    Start the ecs-init upstart job  

    sudo start ecs

  
d)    (Optional) You can verify that the agent is running and see some information on your new container instance with the agent introspection API. Make sure the port 51678 is open in security group.

    curl http://localhost:51678/v1/metadata

23.    Go to AWS service catalog -> Compute -> Ec2 Container Service -> Cluster and verify a default cluster is created   
 [![enter image description here][9]][9]
24.    Now we proceed with creating a task group and adding our docker image as task to run on the cluster  
a)    Examine the simple-app-task-def.json file in the ecs-demo-php-simple-app folder.  
b)    Edit the simple-app-task-def.json and redue the momeory, so that it can run on free tier eligible instance(i assume one is using free tier eligible ec2 instance for this POC, otherwise no need to reduce the memory limit)    
c)    Update **memory=250** in all the occurrence on the simple-app-task-def.json file  
d)    Register a task definition with the simple-app-task-def.json file.  

    aws ecs register-task-definition --cli-input-json file://simple-app-task-def.json

e)    Go to task definition in ec2 container service page, you ll find the registered task definition
 
f)    Use the following AWS CLI command to run a task with the console-sample-app task definition.

    aws ecs run-task --task-definition console-sample-app

g)    Open the sample web app in browser, it should be accessible(refer step 19)

Thanks for reading, do share your comments and queries for follow up discussion.


  [1]: https://i.stack.imgur.com/92vtR.png
  [2]: https://i.stack.imgur.com/O4y5J.png
  [3]: https://i.stack.imgur.com/4hW1c.png
  [4]: https://i.stack.imgur.com/dfbIw.png
  [5]: https://i.stack.imgur.com/MQ6Kx.png
  [6]: https://i.stack.imgur.com/LCdU5.png
  [7]: https://i.stack.imgur.com/nL6rv.png
  [8]: https://i.stack.imgur.com/zk5Ra.png
  [9]: https://i.stack.imgur.com/lTp8x.png

## example-task.json
    {
      "family": "example-task",
      "containerDefinitions": [
        {
            "environment": [],
            "name": "example-container",
            "image": "example-namespace/example-image:latest",
            "cpu": 10,
            "memory": 500,
            "portMappings": [
                {
                    "containerPort": 8080,
                    "hostPort": 80
                }
            ],
            "entryPoint": [],
            "essential": true
        }
      ]
    }

