---
title: "Using aws-cli for Amazon EC2"
slug: "using-aws-cli-for-amazon-ec2"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Getting information about EC2 instances
You can obtain information about EC2 instances using:

    aws ec2 describe-instances

You can obtain information about specific EC2 instances using:

    aws ec2 describe-instances --instance-ids ...

where `...` contains one or more instance identifiers. For example:

    aws ec2 describe-instances --instance-ids i-abcdefgh i-ijklmnop

The output of `aws ec2 describe-instances` uses pagination by default. If the response contains the key `"NextToken"` then you'll need to use that token to obtain the next page of information:

    aws ec2 describe-instances --starting-token <token from previous response>

