---
title : amazon-s3 Tutorial
slug : amazon-s3-tutorial
weight : 9947
draft : false
images : []
type : docs
---

Summary from the Documentation
==============================

From http://docs.aws.amazon.com/AmazonS3/latest/dev/Welcome.html

> Amazon Simple Storage Service is storage for the Internet. It is
> designed to make web-scale computing easier for developers.
> 
> Amazon S3 has a simple web services interface that you can use to
> store and retrieve any amount of data, at any time, from anywhere on
> the web. It gives any developer access to the same highly scalable,
> reliable, fast, inexpensive data storage infrastructure that Amazon
> uses to run its own global network of web sites. The service aims to
> maximize benefits of scale and to pass those benefits on to
> developers.

Language and Scripting Support
==============================
S3 is not a development language as such, but a cloud platform that supports web service requests.  There is an assortment of [tools and SDK's][tools and sdks] that are published by Amazon.  The language SDK's provide transparent access to S3 by handling tasks such as web services requests, authentication, session management, token renewal, etc.  There are also command line interfaces for [bash/windows/ios][aws cli] and [powershell][powershell cli].

The S3 API
==========
The S3 web services API has been [supported by competing vendors][competitors].  This topic does not currently cover the API directly, so the examples in here would not be useful for building applications that connect to competing systems that leverage the S3 API.

Versions
========
As of 28 March 2017, the AWS CLI has 174 versions, which are cleanly documented in the [CLI Release Notes][aws cli versions].  Amazon S3 has 66 versions, of which some are to announce addition of a new region, and others are to add functionality.    These are documented in  the [S3 Release Notes][aws s3 versions].

The Examples
============
With respect to the examples shown so far in this "Getting Started" section, Amazon S3 is useful to developers for the following use cases:

* **Store or back up files** in a high-performing, durable system, thus offloading this task from non-cloud architectures:  linux and windows file systems.  It is expensive to recreate the durability and performance levels of S3 using on premise servers or EC2 instances.
* When network bandwidth is an issue, for example, in cases where multiple simultaneous users must download large files, moving data to S3 can be used as a way for an application to **mitigate bandwidth shortages to a datacenter or on-premises server**.  This is a way of distributing a large code repository, virtual machine images, video, or software installers.  User upload times and user download times can be improved.  [For additional performance in very large user base scenarios, a content delivery system such as cloudfront can be used to cache files closer to the users.]
* **Your application needs to create or consume a big file** and you need a way to allow users to access or deposit it.
* **Your application distribution is very big** and you need to share it with users.
* **You are putting together a continuous delivery pipeline** and for example hosting portions of your website on Amazon S3.

At this point the examples do not show how to do the following:
* The examples, although faster and  clearer than typing `aws s3 help, do not mention some of the commands covered in help, such as `aws s3 website`.
* How to share or restrict user access.   Without explicit restriction, the examples would work only for users sharing the same AWS account.   
* How to secure data via encryption.  Note that AWS does position S3 as having a higher level of security than data stored in EC2.  [AWS Security Best Practices, August 2016, p. 27][best practices]

Security
==============
AWS recommends viewing S3 as a secure platform:

>>Unless you have more stringent business or compliance requirements, you don’t need to introduce additional layers of protection beyond those provided by the AWS secure global infrastructure. [ibid. p.2][best practices]

In their [security guide][best practices], AWS recommends using AWS authentication as suitable for S3.  [ibid. p. 27][best practices]

Additionally, S3 provides server-side encryption or client-side encryption.  Client side encryption is provided transparently by the AWS Java SDK; keys need not be stored on AWS. [ibid. p. 28][best practices]


[aws cli]: https://aws.amazon.com/cli/ "AWS command line interface"

[powershell cli]: https://aws.amazon.com/powershell/ "AWS PowerShell interface"

[best practices]: https://d0.awsstatic.com/whitepapers/Security/AWS_Security_Best_Practices.pdf "security best practices"

[aws cli versions]: https://aws.amazon.com/releasenotes/CLI "cli versions"

[aws S3 versions]: https://aws.amazon.com/releasenotes/Amazon-S3 "s3 versions"

[tools and sdks]: https://aws.amazon.com/tools/ "tools and sdks"

[competitors]: https://en.wikipedia.org/wiki/Amazon_S3#S3_API_and_competing_services "competing services"

