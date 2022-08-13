---
title: "AWS CloudFormation"
slug: "aws-cloudformation"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## CloudFormation sample script to create an EC2 instance along with a Security Group to associate with.
This example will create an EC2 instance of **t2.micro** type in **N.Virginia** region running **Amazon Linux**. During the execution, it will ask to select the KeyPair to use and an I.P. CIDR from where you can SSH to the instance, use default to make SSH open to the internet 
<pre><code>
{
  "AWSTemplateFormatVersion" : "2010-09-09",

  "Description" : "AWS CloudFormation Sample Template EC2InstanceWithSecurityGroupSample: Create an Amazon EC2 instance running the Amazon Linux AMI. This example creates an EC2 security group for the instance to give you SSH access. ",

  "Parameters" : {
    "KeyName": {
      "Description" : "Name of an existing EC2 KeyPair to enable SSH access to the instance",
      "Type": "AWS::EC2::KeyPair::KeyName",
      "ConstraintDescription" : "must be the name of an existing EC2 KeyPair."
    },
    "SSHLocation" : {
      "Description" : "The IP address range that can be used to SSH to the EC2 instances",
      "Type": "String",
      "MinLength": "9",
      "MaxLength": "18",
      "Default": "0.0.0.0/0",
      "AllowedPattern": "(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})/(\\d{1,2})",
      "ConstraintDescription": "must be a valid IP CIDR range of the form x.x.x.x/x."
   }
  },

  "Resources" : {
    "EC2Instance" : {
      "Type" : "AWS::EC2::Instance",
      "Properties" : {
        "InstanceType" : “t2.micro”,
        "SecurityGroups" : [ { "Ref" : "InstanceSecurityGroup" } ],
        "KeyName" : { "Ref" : "KeyName" },
        "ImageId" : “ami-6869aa05”
      }
    },

    "InstanceSecurityGroup" : {
      "Type" : "AWS::EC2::SecurityGroup",
      "Properties" : {
        "GroupDescription" : "Enable SSH access via port 22",
        "SecurityGroupIngress" : [ {
          "IpProtocol" : "tcp",
          "FromPort" : "22",
          "ToPort" : "22",
          "CidrIp" : { "Ref" : "SSHLocation"}
        } ]
      }
    }
  },

  "Outputs" : {
    "InstanceId" : {
      "Description" : "InstanceId of the newly created EC2 instance",
      "Value" : { "Ref" : "EC2Instance" }
    },
    "AZ" : {
      "Description" : "Availability Zone of the newly created EC2 instance",
      "Value" : { "Fn::GetAtt" : [ "EC2Instance", "AvailabilityZone" ] }
    },
    "PublicDNS" : {
      "Description" : "Public DNSName of the newly created EC2 instance",
      "Value" : { "Fn::GetAtt" : [ "EC2Instance", "PublicDnsName" ] }
    },
    "PublicIP" : {
      "Description" : "Public IP address of the newly created EC2 instance",
      "Value" : { "Fn::GetAtt" : [ "EC2Instance", "PublicIp" ] }
    }
  }
}
</code></pre>

## AWS CloudFormer in VPC
CloudFormer template translates the existing AWS entities to a generate new CloudFormation template to accelerate the time to recreate the environment. The [CloudFormer][1] launches in a default VPC which might not be suitable in the cases where the default VPC is deleted. This code base is fork from the original CloudFormer which would be launched inside a new VPC.

    {
      "AWSTemplateFormatVersion" : "2010-09-09",
    
      "Description" : "A custom CloudFormer template forked from AWS provided to extend the capability to provide the ability to specify the VPC. Creates a Separate Stand-Alone VPC, Subnet - 10.0.0.0/16 to launch the CloudFormer Instance. AWS CloudFormer Beta - template creation prototype application. This tool allows you to create an AWS CloudFormation template from the AWS resources in your AWS account. **Warning** This template creates a single EC2 instance in your account to run the application - you will be billed for the instance at normal AWS EC2 rates.",
    
      "Parameters" : {
    
        "Username" : {
          "Description" : "Username to log in to CloudFormer",
          "Type" : "String"
        },
        "Password" : {
          "Description" : "Password to log in to CloudFormer",
          "Type" : "String",
          "NoEcho" : "true"
        },
        "CommonNameTag" : {
            "Description" : "Common Identifier / Friendly Name for the Stack",
            "Type" : "String",
            "Default" : "CloudFormer - Non Default VPC"
        }
      },
    
      "Mappings" : {
        "NetworkValues" : {
            "VPC" : {"CIDR" : "10.0.0.0/16"},
            "Subnet" : {"CIDR" : "10.0.10.0/16"}
        },
        
        "Region2Examples" : {
          "us-east-1"      : { "Examples" : "https://s3.amazonaws.com/cloudformation-examples-us-east-1" },
          "us-west-2"      : { "Examples" : "https://s3-us-west-2.amazonaws.com/cloudformation-examples-us-west-2" },
          "us-west-1"      : { "Examples" : "https://s3-us-west-1.amazonaws.com/cloudformation-examples-us-west-1" },
          "eu-west-1"      : { "Examples" : "https://s3-eu-west-1.amazonaws.com/cloudformation-examples-eu-west-1" },
          "eu-central-1"   : { "Examples" : "https://s3-eu-central-1.amazonaws.com/cloudformation-examples-eu-central-1" },
          "ap-southeast-1" : { "Examples" : "https://s3-ap-southeast-1.amazonaws.com/cloudformation-examples-ap-southeast-1" },
          "ap-northeast-1" : { "Examples" : "https://s3-ap-northeast-1.amazonaws.com/cloudformation-examples-ap-northeast-1" },
          "ap-southeast-2" : { "Examples" : "https://s3-ap-southeast-2.amazonaws.com/cloudformation-examples-ap-southeast-2" },
          "ap-northeast-2" : { "Examples" : "https://s3-ap-northeast-2.amazonaws.com/cloudformation-examples-ap-northeast-2" },
          "sa-east-1"      : { "Examples" : "https://s3-sa-east-1.amazonaws.com/cloudformation-examples-sa-east-1" },
          "cn-north-1"     : { "Examples" : "https://s3.cn-north-1.amazonaws.com.cn/cloudformation-examples-cn-north-1" }
        },
    
        "Region2Principal" : {
          "us-east-1"      : { "EC2Principal" : "ec2.amazonaws.com", "OpsWorksPrincipal" : "opsworks.amazonaws.com" },
          "us-west-2"      : { "EC2Principal" : "ec2.amazonaws.com", "OpsWorksPrincipal" : "opsworks.amazonaws.com" },
          "us-west-1"      : { "EC2Principal" : "ec2.amazonaws.com", "OpsWorksPrincipal" : "opsworks.amazonaws.com" },
          "eu-west-1"      : { "EC2Principal" : "ec2.amazonaws.com", "OpsWorksPrincipal" : "opsworks.amazonaws.com" },
          "ap-southeast-1" : { "EC2Principal" : "ec2.amazonaws.com", "OpsWorksPrincipal" : "opsworks.amazonaws.com" },
          "ap-northeast-1" : { "EC2Principal" : "ec2.amazonaws.com", "OpsWorksPrincipal" : "opsworks.amazonaws.com" },
          "ap-southeast-2" : { "EC2Principal" : "ec2.amazonaws.com", "OpsWorksPrincipal" : "opsworks.amazonaws.com" },
          "ap-northeast-2" : { "EC2Principal" : "ec2.amazonaws.com", "OpsWorksPrincipal" : "opsworks.amazonaws.com" },
          "sa-east-1"      : { "EC2Principal" : "ec2.amazonaws.com", "OpsWorksPrincipal" : "opsworks.amazonaws.com" },
          "cn-north-1"     : { "EC2Principal" : "ec2.amazonaws.com.cn", "OpsWorksPrincipal" : "opsworks.amazonaws.com.cn" },
          "eu-central-1"   : { "EC2Principal" : "ec2.amazonaws.com", "OpsWorksPrincipal" : "opsworks.amazonaws.com" }
        },
        
        "AWSInstanceType2Arch" : {
          "t1.micro"    : { "Arch" : "PV64"   },
          "t2.nano"     : { "Arch" : "HVM64"  },
          "t2.micro"    : { "Arch" : "HVM64"  },
          "t2.small"    : { "Arch" : "HVM64"  },
          "t2.medium"   : { "Arch" : "HVM64"  },
          "t2.large"    : { "Arch" : "HVM64"  },
          "m1.small"    : { "Arch" : "PV64"   },
          "m1.medium"   : { "Arch" : "PV64"   },
          "m1.large"    : { "Arch" : "PV64"   },
          "m1.xlarge"   : { "Arch" : "PV64"   },
          "m2.xlarge"   : { "Arch" : "PV64"   },
          "m2.2xlarge"  : { "Arch" : "PV64"   },
          "m2.4xlarge"  : { "Arch" : "PV64"   },
          "m3.medium"   : { "Arch" : "HVM64"  },
          "m3.large"    : { "Arch" : "HVM64"  },
          "m3.xlarge"   : { "Arch" : "HVM64"  },
          "m3.2xlarge"  : { "Arch" : "HVM64"  },
          "m4.large"    : { "Arch" : "HVM64"  },
          "m4.xlarge"   : { "Arch" : "HVM64"  },
          "m4.2xlarge"  : { "Arch" : "HVM64"  },
          "m4.4xlarge"  : { "Arch" : "HVM64"  },
          "m4.10xlarge" : { "Arch" : "HVM64"  },
          "c1.medium"   : { "Arch" : "PV64"   },
          "c1.xlarge"   : { "Arch" : "PV64"   },
          "c3.large"    : { "Arch" : "HVM64"  },
          "c3.xlarge"   : { "Arch" : "HVM64"  },
          "c3.2xlarge"  : { "Arch" : "HVM64"  },
          "c3.4xlarge"  : { "Arch" : "HVM64"  },
          "c3.8xlarge"  : { "Arch" : "HVM64"  },
          "c4.large"    : { "Arch" : "HVM64"  },
          "c4.xlarge"   : { "Arch" : "HVM64"  },
          "c4.2xlarge"  : { "Arch" : "HVM64"  },
          "c4.4xlarge"  : { "Arch" : "HVM64"  },
          "c4.8xlarge"  : { "Arch" : "HVM64"  },
          "g2.2xlarge"  : { "Arch" : "HVMG2"  },
          "g2.8xlarge"  : { "Arch" : "HVMG2"  },
          "r3.large"    : { "Arch" : "HVM64"  },
          "r3.xlarge"   : { "Arch" : "HVM64"  },
          "r3.2xlarge"  : { "Arch" : "HVM64"  },
          "r3.4xlarge"  : { "Arch" : "HVM64"  },
          "r3.8xlarge"  : { "Arch" : "HVM64"  },
          "i2.xlarge"   : { "Arch" : "HVM64"  },
          "i2.2xlarge"  : { "Arch" : "HVM64"  },
          "i2.4xlarge"  : { "Arch" : "HVM64"  },
          "i2.8xlarge"  : { "Arch" : "HVM64"  },
          "d2.xlarge"   : { "Arch" : "HVM64"  },
          "d2.2xlarge"  : { "Arch" : "HVM64"  },
          "d2.4xlarge"  : { "Arch" : "HVM64"  },
          "d2.8xlarge"  : { "Arch" : "HVM64"  },
          "hi1.4xlarge" : { "Arch" : "HVM64"  },
          "hs1.8xlarge" : { "Arch" : "HVM64"  },
          "cr1.8xlarge" : { "Arch" : "HVM64"  },
          "cc2.8xlarge" : { "Arch" : "HVM64"  }
        },
    
        "AWSInstanceType2NATArch" : {
          "t1.micro"    : { "Arch" : "NATPV64"   },
          "t2.nano"     : { "Arch" : "NATHVM64"  },
          "t2.micro"    : { "Arch" : "NATHVM64"  },
          "t2.small"    : { "Arch" : "NATHVM64"  },
          "t2.medium"   : { "Arch" : "NATHVM64"  },
          "t2.large"    : { "Arch" : "NATHVM64"  },
          "m1.small"    : { "Arch" : "NATPV64"   },
          "m1.medium"   : { "Arch" : "NATPV64"   },
          "m1.large"    : { "Arch" : "NATPV64"   },
          "m1.xlarge"   : { "Arch" : "NATPV64"   },
          "m2.xlarge"   : { "Arch" : "NATPV64"   },
          "m2.2xlarge"  : { "Arch" : "NATPV64"   },
          "m2.4xlarge"  : { "Arch" : "NATPV64"   },
          "m3.medium"   : { "Arch" : "NATHVM64"  },
          "m3.large"    : { "Arch" : "NATHVM64"  },
          "m3.xlarge"   : { "Arch" : "NATHVM64"  },
          "m3.2xlarge"  : { "Arch" : "NATHVM64"  },
          "m4.large"    : { "Arch" : "NATHVM64"  },
          "m4.xlarge"   : { "Arch" : "NATHVM64"  },
          "m4.2xlarge"  : { "Arch" : "NATHVM64"  },
          "m4.4xlarge"  : { "Arch" : "NATHVM64"  },
          "m4.10xlarge" : { "Arch" : "NATHVM64"  },
          "c1.medium"   : { "Arch" : "NATPV64"   },
          "c1.xlarge"   : { "Arch" : "NATPV64"   },
          "c3.large"    : { "Arch" : "NATHVM64"  },
          "c3.xlarge"   : { "Arch" : "NATHVM64"  },
          "c3.2xlarge"  : { "Arch" : "NATHVM64"  },
          "c3.4xlarge"  : { "Arch" : "NATHVM64"  },
          "c3.8xlarge"  : { "Arch" : "NATHVM64"  },
          "c4.large"    : { "Arch" : "NATHVM64"  },
          "c4.xlarge"   : { "Arch" : "NATHVM64"  },
          "c4.2xlarge"  : { "Arch" : "NATHVM64"  },
          "c4.4xlarge"  : { "Arch" : "NATHVM64"  },
          "c4.8xlarge"  : { "Arch" : "NATHVM64"  },
          "g2.2xlarge"  : { "Arch" : "NATHVMG2"  },
          "g2.8xlarge"  : { "Arch" : "NATHVMG2"  },
          "r3.large"    : { "Arch" : "NATHVM64"  },
          "r3.xlarge"   : { "Arch" : "NATHVM64"  },
          "r3.2xlarge"  : { "Arch" : "NATHVM64"  },
          "r3.4xlarge"  : { "Arch" : "NATHVM64"  },
          "r3.8xlarge"  : { "Arch" : "NATHVM64"  },
          "i2.xlarge"   : { "Arch" : "NATHVM64"  },
          "i2.2xlarge"  : { "Arch" : "NATHVM64"  },
          "i2.4xlarge"  : { "Arch" : "NATHVM64"  },
          "i2.8xlarge"  : { "Arch" : "NATHVM64"  },
          "d2.xlarge"   : { "Arch" : "NATHVM64"  },
          "d2.2xlarge"  : { "Arch" : "NATHVM64"  },
          "d2.4xlarge"  : { "Arch" : "NATHVM64"  },
          "d2.8xlarge"  : { "Arch" : "NATHVM64"  },
          "hi1.4xlarge" : { "Arch" : "NATHVM64"  },
          "hs1.8xlarge" : { "Arch" : "NATHVM64"  },
          "cr1.8xlarge" : { "Arch" : "NATHVM64"  },
          "cc2.8xlarge" : { "Arch" : "NATHVM64"  }
        },
    
        "AWSRegionArch2AMI" : {
          "us-east-1"        : {"PV64" : "ami-d4f7ddbe", "HVM64" : "ami-2df5df47", "HVMG2" : "ami-95f7c0ff"},
          "us-west-2"        : {"PV64" : "ami-a9ae4ec9", "HVM64" : "ami-42b15122", "HVMG2" : "ami-83a744e3"},
          "us-west-1"        : {"PV64" : "ami-14f68074", "HVM64" : "ami-f7f58397", "HVMG2" : "ami-ee62138e"},
          "eu-west-1"        : {"PV64" : "ami-a93484da", "HVM64" : "ami-3c38884f", "HVMG2" : "ami-25d76556"},
          "eu-central-1"     : {"PV64" : "ami-e8233884", "HVM64" : "ami-d8203bb4", "HVMG2" : "ami-8fadb6e3"},
          "ap-northeast-1"   : {"PV64" : "ami-c8aca8a6", "HVM64" : "ami-eeabaf80", "HVMG2" : "ami-71e6e01f"},
          "ap-northeast-2"   : {"PV64" : "NOT_SUPPORTED", "HVM64" : "ami-431fd12d", "HVMG2" : "NOT_SUPPORTED"},
          "ap-southeast-1"   : {"PV64" : "ami-6702cc04", "HVM64" : "ami-8504cae6", "HVMG2" : "ami-1e7ab47d"},
          "ap-southeast-2"   : {"PV64" : "ami-4f04232c", "HVM64" : "ami-a30126c0", "HVMG2" : "ami-68a1860b"},
          "sa-east-1"        : {"PV64" : "ami-daf477b6", "HVM64" : "ami-e2f4778e", "HVMG2" : "NOT_SUPPORTED"},
          "cn-north-1"       : {"PV64" : "ami-0534fc68", "HVM64" : "ami-3f36fe52", "HVMG2" : "NOT_SUPPORTED"}
        }
    
      },
    
      "Resources" : {
        "VPC" : {
          "Type" : "AWS::EC2::VPC",
          "Properties" : {
              "CidrBlock" : { "Fn::FindInMap" : ["NetworkValues", "VPC", "CIDR"] },
              "EnableDnsHostnames" : "true",
              "Tags" : [
                  {"Key": "Name", "Value": {"Ref":"CommonNameTag"} }
              ]
          }
        },
        "Subnet" : {
           "Type" : "AWS::EC2::Subnet",
           "Properties" : {
               "VpcId" : { "Ref" : "VPC" },
               "CidrBlock" : { "Fn::FindInMap" : ["NetworkValues", "Subnet", "CIDR"] },
                "MapPublicIpOnLaunch" : "true",
               "Tags" : [
                  {"Key": "Name", "Value": {"Ref":"CommonNameTag"} }
              ]
           } 
        },
    
        "RouteTable" : {
           "Type" : "AWS::EC2::RouteTable",
           "Properties" : {
               "VpcId" : { "Ref" : "VPC" }
           }
        },
        
        "InternetGateway" : {
            "Type" : "AWS::EC2::InternetGateway",
            "Properties" : {
                "Tags" : [
                    {"Key": "Name", "Value": {"Ref":"CommonNameTag"} }
                ]
            }
        },
        
        "InternetGatewayAttachment" : {
           "Type" : "AWS::EC2::VPCGatewayAttachment",
           "Properties" : {
               "InternetGatewayId" : { "Ref" : "InternetGateway"},
               "VpcId" : { "Ref" : "VPC"}
           }
        },  
    
        "Route" : {
            "Type" : "AWS::EC2::Route",
            "Properties" : {
                "DestinationCidrBlock" : "0.0.0.0/0",
                "GatewayId" : { "Ref" : "InternetGateway" },
                "RouteTableId" : { "Ref" : "RouteTable" }
            }
        },
        
        "SubnetRouteTableAttachment" : {
             "Type" : "AWS::EC2::SubnetRouteTableAssociation",
             "Properties" : {
                "RouteTableId" : { "Ref" : "RouteTable" },
                "SubnetId" : { "Ref" : "Subnet" }
             }  
        },
    
        "CFNRole": {
          "Type": "AWS::IAM::Role",
          "Properties": {
            "AssumeRolePolicyDocument": {
              "Statement": [{
                "Effect": "Allow",
                "Principal": { "Service": { "Fn::FindInMap" : [ "Region2Principal", {"Ref" : "AWS::Region"}, "EC2Principal"]}},
                "Action": [ "sts:AssumeRole" ]
              }]
            },
            "Path": "/"
          }
        },
    
        "CFNRolePolicy": {
          "Type": "AWS::IAM::Policy",
          "Properties": {
            "PolicyName": "CloudFormerPolicy",
            "PolicyDocument": {
              "Statement": [ {
                "Effect": "Allow",
                "Action"   : [
                  "autoscaling:Describe*",
                  "cloudformation:Describe*",
                  "cloudformation:List*",
                  "cloudfront:List*",
                  "cloudFront:Get*",
                  "cloudtrail:Describe*",
                  "cloudtrail:Get*",
                  "cloudwatch:Describe*",
                  "dynamodb:List*",
                  "dynamodb:Describe*",
                  "elasticbeanstalk:Describe*",
                  "ec2:Describe*",
                  "elasticloadbalancing:Describe*",
                  "elasticache:Describe*",
                  "rds:Describe*",
                  "rds:List*",
                  "route53:List*",
                  "route53:Get*",
                  "s3:List*",
                  "s3:Get*",
                  "s3:PutObject",
                  "sdb:Get*",
                  "sdb:List*",
                  "sns:Get*",
                  "sns:List*",
                  "sqs:Get*",
                  "sqs:List*",
                  "opsworks:Describe*",
                  "redshift:Describe*",
                  "kinesis:Describe*",
                  "kinesis:List*"
                ],
                "Resource": "*"
              } ]
            },
            "Roles": [ { "Ref": "CFNRole" } ]
          }
        },
    
        "CFNInstanceProfile": {
          "Type": "AWS::IAM::InstanceProfile",
          "Properties": {
            "Path": "/",
            "Roles": [ { "Ref": "CFNRole" } ]
          }
        },
    
        "WebServer": {
          "Type": "AWS::EC2::Instance",
          "Metadata" : {
            "AWS::CloudFormation::Init" : {
              "configSets" : {
                "full_install" : ["base", "cloudformer"]
              },
              "base" : {
                "packages" : {
                  "yum" : {
                    "gcc"              : [],
                    "gcc-c++"          : [],
                    "make"             : [],
                    "libxml2-devel"    : [],
                    "libxslt-devel"    : [],
                    "sqlite-devel"     : [],
                    "patch"            : [],
                    "readline"         : [],
                    "readline-devel"   : [],
                    "zlib"             : [],
                    "zlib-devel"       : [],
                    "libyaml-devel"    : [],
                    "libffi-devel"     : [],
                    "openssl-devel"    : [],
                    "bzip2"            : [],
                    "autoconf"         : [],
                    "automake"         : [],
                    "libtool"          : [],
                    "bison"            : [],
                    "ruby-devel"       : []
                  }
                }
              },
              "cloudformer" : {
                "sources" : {
                  "/home/ec2-user/cloudformer" : {"Fn::Join" : ["/", [
                                                  {"Fn::FindInMap" : ["Region2Examples", {"Ref" : "AWS::Region"}, "Examples"]},
                                                  "AWSCloudFormer041.zip" ]]}
                },
                "files" : {
                  "/home/ec2-user/setup_cloudformer" : {
                    "content" : { "Fn::Join" : ["", [
                      "#!/usr/bin/env bash\n",
                      "cd /home/ec2-user/cloudformer\n",
                      "# Setup the CloudFormer service\n",
                      "mkdir -p vendor/bundle\n",
                      "gem install --local /home/ec2-user/cloudformer/vendor/cache/rake-10.4.2.gem\n",
                      "gem install --local /home/ec2-user/cloudformer/vendor/cache/bundler-1.7.11.gem\n",
                      "gem install --local /home/ec2-user/cloudformer/vendor/cache/bundle-0.0.1.gem\n",
                      "/usr/local/bin/bundle install --local --path /home/ec2-user/cloudformer/vendor/bundle\n",
                      "/usr/local/bin/rake RAILS_ENV=production db:migrate\n",
                      "gem install --local /home/ec2-user/cloudformer/vendor/cache/rack-1.6.0.gem\n",
                      "gem install --local /home/ec2-user/cloudformer/vendor/cache/eventmachine-1.0.4.gem\n",
                      "gem install --local /home/ec2-user/cloudformer/vendor/cache/daemons-1.1.9.gem\n",
                      "gem install --local /home/ec2-user/cloudformer/vendor/cache/thin-1.6.3.gem\n",
                      "# Create certificate and private key for SSL\n",
                      "mkdir -p /home/ec2-user/cloudformer/.ssl\n",
                      "cd /home/ec2-user/cloudformer/.ssl\n",
                      "openssl genrsa -des3 -passout pass:\"" , { "Ref" : "Password" }, "\" -out server.pass.key 1024\n",
                      "openssl rsa -passin pass:\"", { "Ref" : "Password" }, "\" -in server.pass.key -out server.key\n",
                      "openssl req -new -key server.key -out server.csr -subj \"/C=US/ST=Washington/L=Seattle/O=Amazon Web Services/OU=CloudFormer\"\n",
                      "openssl x509 -req -days 365 -in server.csr -signkey server.key -out server.crt\n",
                      "rm server.pass.key server.csr\n"
                    ]]},
                    "mode"  : "000755",
                    "owner" : "root",
                    "group" : "root"
                  },
                  "/home/ec2-user/cloudformer/config/initializers/user.rb" : {
                    "content" : { "Fn::Join" : ["", [
                      "USER_NAME = \"", { "Ref" : "Username" }, "\"\n",
                      "PASSWORD = \"", { "Ref" : "Password" }, "\"\n"
                    ]]},
                    "mode"  : "000400",
                    "owner" : "root",
                    "group" : "root"
                  },
                  "/usr/bin/cloudformer" : {
                    "content" : { "Fn::Join" : ["", [
                      "#!/usr/bin/env bash\n",
                      "cd /home/ec2-user/cloudformer\n",
                      "/usr/local/bin/thin start -p 443 -e production -d --ssl --ssl-key-file /home/ec2-user/cloudformer/.ssl/server.key --ssl-cert-file /home/ec2-user/cloudformer/.ssl/server.crt\n"
                    ]]},
                    "mode"  : "000755",
                    "owner" : "root",
                    "group" : "root"
                  }
                },
                "commands" : {
                  "01_install_cloudformer" : {
                    "command" : "/home/ec2-user/setup_cloudformer &> /var/log/setup_cloudformer.log",
                    "cwd" : "/home/ec2-user/cloudformer"
                  },
                  "02_setup_boot" : {
                    "command" : "echo '/usr/bin/cloudformer' >> /etc/rc.local",
                    "cwd" : "/"
                  }
                }
              }
            }
          },
          "Properties": {
            "ImageId" : { "Fn::FindInMap" : [ "AWSRegionArch2AMI", { "Ref" : "AWS::Region" },
                              { "Fn::FindInMap" : [ "AWSInstanceType2Arch", "t2.medium", "Arch" ] } ] },
            "InstanceType"       : "t2.medium",
            "SecurityGroupIds"     : [ {"Ref" : "WebServerSecurityGroup"} ],
            "SubnetId" : { "Ref" : "Subnet" },
            "IamInstanceProfile" : { "Ref" : "CFNInstanceProfile" },
            "UserData"           : { "Fn::Base64" : { "Fn::Join" : ["", [
              "#!/bin/bash -xe\n",
              "yum update -y aws-cfn-bootstrap\n",
    
              "/opt/aws/bin/cfn-init -v ",
              "         --stack ", { "Ref" : "AWS::StackId" },
              "         --resource WebServer ",
              "         --configsets full_install ",
              "         --region ", { "Ref" : "AWS::Region" }, "\n",
    
              "/opt/aws/bin/cfn-signal -e $? ",
              "         --stack ", { "Ref" : "AWS::StackId" },
              "         --resource WebServer ",
              "         --region ", { "Ref" : "AWS::Region" }, "\n"
            ]]}}
          },
          "CreationPolicy" : {
            "ResourceSignal" : {
              "Timeout" : "PT30M"
            }
          }
        },
    
        "WebServerSecurityGroup" : {
          "Type" : "AWS::EC2::SecurityGroup",
          "Properties" : {
            "GroupDescription" : "Enable HTTPS access via port 443",
            "VpcId" : { "Ref" : "VPC" },
            "SecurityGroupIngress" : [
              {"IpProtocol" : "tcp", "FromPort" : "443", "ToPort" : "443", "CidrIp" : "0.0.0.0/0"}
            ]
          }
        }
      },
    
      "Outputs" : {
        "WebsiteURL" : {
          "Value" : { "Fn::Join" : ["", ["https://", { "Fn::GetAtt" : [ "WebServer", "PublicDnsName" ]} ]] },
          "Description" : "URL for CloudFormer"
        }
      }
    }


  [1]: https://aws.amazon.com/developertools/6460180344805680

