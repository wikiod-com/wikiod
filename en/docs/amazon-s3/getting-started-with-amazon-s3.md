---
title: "Getting started with amazon-s3"
slug: "getting-started-with-amazon-s3"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation of AWS CLI for accessing S3
**Installing aws cli in Ubuntu / Debian Instance** 

    sudo apt-get install -y python-dev python-pip
    sudo pip install awscli
    aws --version
    aws configure

**Installing aws cli using python** 

Using pip you can install aws cli in windows, OS X and Linux 

    sudo pip install awscli

**Configuring the AWS Command Line Interface**

This section explains how to configure settings that the AWS Command Line Interface uses when interacting with AWS, such as your security credentials and the default region.

    $ aws configure
    AWS Access Key ID [None]: <Your access key >
    AWS Secret Access Key [None]: <Your secret key>
    Default region name [None]: us-west-2
    Default output format [None]: json

Get the Access key and Secret key from the account page in AWS

**Creating Buckets**

Use the aws s3 mb command to create a new bucket. Bucket names must be unique and should be DNS compliant. Bucket names can contain lowercase letters, numbers, hyphens and periods

    aws s3 mb s3://bucket-name

**Removing Buckets**

To remove a bucket, use the aws s3 rb command.By default bucket should be empty.

    aws s3 rb s3://bucket-name
To remove a non-empty bucket, you need to include the --force option.

    aws s3 rb s3://bucket-name --force

**Listing Buckets**

To list all buckets or their contents, use the aws s3 ls command

    aws s3 ls
    aws s3 ls s3://bucket-name
The following command lists the objects in bucket-name/path

    aws s3 ls s3://bucket-name/path

**Synchronize files between local file system and S3** 

    aws s3 sync . s3://my-bucket/path 
It will upload all the files in the current directory to S3. To download the files from S3 to the current directory execute 

    aws s3 sync s3://my-bucket/path .





## Hello World Example using Java
This example attempts to create a bucket called 'hello-world' and, as the bucket hello-world has already been created by someone else in S3's global namespace, throws the following exception.  Change 'hello-world' to something else to avoid the exception by creating a uniquely named bucket.  [The new bucket so created can be deleted using the AWS console][1]


>Exception in thread "main" com.amazonaws.services.s3.model.AmazonS3Exception: The requested bucket name is not available. The bucket namespace is shared by all users of the system. Please select a different name and try again. (Service: Amazon S3; Status Code: 409; Error Code: BucketAlreadyExists; Request ID: ...

```java
               import com.amazonaws.services.s3.AmazonS3;
               import com.amazonaws.services.s3.AmazonS3ClientBuilder;
               import com.amazonaws.services.s3.model.CreateBucketRequest;
               import com.amazonaws.services.s3.model.Bucket;

                /** S3 "hello world" example. */
                public class S3Hello {

                        /** Name of hello-world bucket -- must be globally unique.  The
                         *  bucket namespace is shared by all users of the system.
                         */
                        static final String BUCKET_NAME = "hello-world";

                        /** Creates bucket
                         *  @param args Command line arguments
                         */
                        public static void main(final String[] args) {

                                AmazonS3 s3 = AmazonS3ClientBuilder.defaultClient();

                                CreateBucketRequest request
                                     = new CreateBucketRequest(BUCKET_NAME);

                                Bucket bucket = s3.createBucket(request);
                                System.out.println("S3 Hello World completed.");
                        }
                }
```

This example requires the following dependencies:

* Java installed with console working.
* AWS Java SDK installed. https://aws.amazon.com/sdk-for-java/
* Credentials file `credentials` set up in .aws under your home directory. https://aws.amazon.com/developers/getting-started/java/
* Credential to have admin or 'create bucket' rights in S3. http://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-buckets


  [1]: http://docs.aws.amazon.com/AmazonS3/latest/dev/delete-or-empty-bucket.html

## Hello World Using PowerShell
This example expects an error, as the hello-world bucket already exists and S3 uses a global namespace.

        New-S3Bucket -BucketName "hello-world"

> New-S3Bucket : The requested bucket name is not available. The bucket namespace is shared by all users of the system. Please select a different name and try again.

If you replace hello-world with something else that is unique, the bucket will be created without error, and you will get the following result:

>         CreationDate                                      BucketName
>         ------------                                      ----------
>         3/30/2017 11:43:03 PM                             hello-world-832jklsdJF


This example requires the following dependencies:

* PowerShell. See http://docs.aws.amazon.com/powershell/latest/userguide/pstools-getting-set-up.html
* Credentials.  These can be created using the AWS console.  There are several options for managing these using PowerShell.  Below is a simple example for setup.  See http://docs.aws.amazon.com/powershell/latest/userguide/specifying-your-aws-credentials.html

        PS C:\> Set-AWSCredentials -AccessKey AKIAIOSFODNN7LAJD8A 
               -SecretKey  "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEjw9JFKS3" -StoreAs default



## AWS CLI S3 Commands List
List of commonly used S3 AWS CLI Commands 

**Create Bucket** 

    aws s3 mb s3://bucket-name
**Remove Bucket** 

    aws s3 rb s3://bucket-name

**List Buckets**

    aws s3 ls

**List contents inside the bucket** 

    aws s3 ls s3://bucket-name

**List Bucket with a path**

    aws s3 ls s3://bucket-name/path

**Copy file** 

    aws s3 cp file.txt s3://my-bucket/ 

**Synchronize files**

    aws s3 sync . s3://my-bucket/path

**Delete local file**

    rm ./MyFile1.txt

**Attempt sync without --delete option - nothing happens**

    aws s3 sync . s3://my-bucket/path

**Sync with deletion - object is deleted from bucket**

    aws s3 sync . s3://my-bucket/path --delete

**Delete object from bucket**

    aws s3 rm s3://my-bucket/path/MySubdirectory/MyFile3.txt

**Sync with deletion - local file is deleted**

    aws s3 sync s3://my-bucket/path . --delete

**Sync with Infrequent Access storage class**

    aws s3 sync . s3://my-bucket/path --storage-class STANDARD_IA

**Copy MyFile.txt in current directory to s3://my-bucket/path**

    aws s3 cp MyFile.txt s3://my-bucket/path/

**Move all .jpg files in s3://my-bucket/path to ./MyDirectory**

    aws s3 mv s3://my-bucket/path ./MyDirectory --exclude '*' --include '*.jpg' --recursive

**List the contents of my-bucket**

    aws s3 ls s3://my-bucket

**List the contents of path in my-bucket**

    aws s3 ls s3://my-bucket/path

**Delete s3://my-bucket/path/MyFile.txt**

    aws s3 rm s3://my-bucket/path/MyFile.txt

 **Delete s3://my-bucket/path and all of its contents**

    aws s3 rm s3://my-bucket/path --recursive

















