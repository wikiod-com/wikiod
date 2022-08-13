---
title: "Introduction to AWS CLI"
slug: "introduction-to-aws-cli"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Installing the aws cli
**On linux**: If you don't have pip installed, install it first:

    curl "https://bootstrap.pypa.io/get-pip.py" -o "get-pip.py"
    sudo python get-pip.py
Then install awscli:

    sudo pip install awscli

**On Windows**: Download the latest installers from [here][1]


  [1]: http://docs.aws.amazon.com/cli/latest/userguide/tutorial-ec2-ubuntu.html "awc cli installer"

## Configuring the aws cli
Now you have aws cli installed, you'll have to configure it access your AWS resources. You can have multiple profiles like *test*, *dev*, *prod*, etc profiles. So let's assume you want to configure it for your test environment.

    aws configure --profile=test
It will ask for following information:

    AWS Access Key ID [None]: XXXXXXXXXXXXXX
    AWS Secret Access Key [None]: XXXXXXXXXXXXXXXXXXXX
    Default region name [None]: us-west-2
    Default output format [None]: json

You will get the above information from IAM management in AWS Console.



## Working with aws cli
The best part about aws cli is that you can embed the commands into a script and can trigger them based on some criteria. Like auto deployment on production (in Elastic Beanstalk), no need to go to AWS Console to select and deploy.
<br/>You'll get all the available commands by running:

    # This will give all the available commands
    aws help
You can even go further, like:

    # This will give all the available options for ec2
    aws ec2 help
and further

    # This will output all the operations you can do with ec2 instances
    aws ec2 describe-instances help 

You can list/manipulate all the aws resources (S3, EC2, EBS, RDS, etc) using aws cli. Here's the complete [documentation][1].

  [1]: http://docs.aws.amazon.com/cli/latest/reference/index.html#cli-aws

