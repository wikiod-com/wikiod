---
title: "Getting started with amazon-ec2"
slug: "getting-started-with-amazon-ec2"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## EC2 Instances
Detailed instructions on launching an EC2 instance.

## Launching an EC2 Instance with the AWS Management concole
In this example, we will launch a basic EC2 Instance with Amazon Linux in the quickest manner possible via the AWS Management Console.  Amazon frequently improves the user experience of the AWM Management console, so you might experience some changes to the screens below.

**Important:** launching an instance in this manner is not considered secure and can incur cost if the instance is left running.  Please terminate any instances created with these steps that you do not intend to use and pay for.

Amazon offers new users the [AWS Free Tier][1] account that allows you to test drive AWS features at very low cost.

First, sign into the [AWS Management console][2].  Create an account if you don't have one already (and take advantage of the Free Tier).

Scroll down to the compute section and click EC2
[![Scroll down to the compute section and click EC2][3]][3]

In the middle of the EC2 main screen, click the blue **Launch Instance** button.

[![click the blue **Launch Instance** button][4]][4]

For the Step 1 screen, chose **Amazon Linux** by clicking on the top **Select** button.

[![Click the Amazon Linux Select button.][5]][5]

For Step 2, select **t2.micro** instance type and click the **Next: Configure Instance Details** button.

[![Select Instance Type][6]][6]

On Step 3, keep all of the defaults and click the **Review and Launch** button.

[![Click the Review and Launch button.][7]][7]

This takes you to Step 7 screen - Review and Launch.  Click the blue launch button at the bottom of this screen.

[![Final Launch Screen][8]][8]

A dialog window will pop up asking you to create a new key for your instance.  Please select **Create new Pair** and provide a name for your **Key Pair Name**.  Click the **Download Key Pair** button to download the key pair to your computer.  This will enable the blue **Launch Instances** button.  

  If you plan to keep your EC2 instance, then you need to safeguard this Key Pair file.  *This is the only time you will be offered the Key Pair*.  If you plan to terminate this EC2 after completion of this example, you can safely ignore the Key Pair file.

Click **Launch Instances** to launch your test EC2 instance.
[![create key pair][9]][9]

The next screen Launch Status contains a link to view the status of the launch.  Click the instance name to view the launch status.

[![Launch Status][10]][10]

As AWS brings up the instance, the status will show **Initializing** for a few minutes.

[![Initializing][11]][11]

When the instance is fully launched, your EC2 status should be **Running** and your instance screen should be similar to the following:

[![Running][12]][12]

The last step of this example is to terminate this instance.  Select Actions -> Instance State -> Terminate.  Then click the blue button on the dialog screen(not shown):  **Yes Terminate**.

[![Terminate][13]][13]


  [1]: https://aws.amazon.com/free/
  [2]: https://aws.amazon.com/console/
  [3]: https://i.stack.imgur.com/dK2bg.png
  [4]: https://i.stack.imgur.com/bXOtz.png
  [5]: https://i.stack.imgur.com/AP6n7.png
  [6]: https://i.stack.imgur.com/X6Li3.png
  [7]: https://i.stack.imgur.com/loWSt.png
  [8]: https://i.stack.imgur.com/91wzx.png
  [9]: https://i.stack.imgur.com/6bgaC.png
  [10]: https://i.stack.imgur.com/0d1fi.png
  [11]: https://i.stack.imgur.com/MqM94.png
  [12]: https://i.stack.imgur.com/h8C1o.png
  [13]: https://i.stack.imgur.com/zJLzC.png

