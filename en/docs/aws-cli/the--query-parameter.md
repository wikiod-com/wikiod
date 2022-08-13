---
title: "The --query Parameter"
slug: "the---query-parameter"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

The `--query` parameter is often overlooked, but it is incredibly powerful. It uses the [JMESPath](http://jmespath.org/) query language to filter service responses down to precisely what you want.

## Listing Instances in an Easy to Read Way
Instances have a lot of metadata that gets returned from a call to `describe-instances`, but often times you just want to see the basics. You can use a JMESPath query combined with table output to show concise instance information in an easily readable way.

```
aws ec2 describe-instances --output table --query "Reservations[].Instances[].{Name: Tags[?Key == 'Name'].Value | [0], Id: InstanceId, State: State.Name, Type: InstanceType}"
```

```
-----------------------------------------------------
|                 DescribeInstances                 |
+-----------+---------------+----------+------------+
|    Id     |     Name      |  State   |   Type     |
+-----------+---------------+----------+------------+
|  i-abc123 |  None         |  stopped |  m3.large  |
|  i-def456 |  amazon linux |  stopped |  t2.micro  |
|  i-ghi789 |  proxy        |  running |  t2.micro  |
+-----------+---------------+----------+------------+
```

Now lets break that up piece by piece. First, we have `--output table`. This produces a colorized table representation of the response. This is generally most useful with commands that return small sets of data or where you have filtered the data down.

Now onto the `--query`. This one looks long, but it is actually quite simple. The first part is `Reservations[].Instances[]`. This returns a flattened list of all the returned instances.

The next part of the query is encapsulated with `.{}`. What this is doing is creating a new json object for each item in the list where each value is a JMESPath query to be applied to the source object (in this case, an Instance). Most of these are very simple, but `Name` is a bit more complex.

The full query to get `Name` is `Tags[?Key == 'Name'].Value | [0]`. The first part of that, `Tags[?Key == 'Name']` is searching the instance's tags for a tag whose key is `Name`. The second half `.Value | [0]` is selecting the values of each of those tags and then taking the first item from the list (in this case, there will only ever be one).

Exactly what you want in that table is completely up to you. If you wanted to add DNS information, for instance, you could easily add a new key `DNS: PublicDnsName`:

```
aws ec2 describe-instances --output table --query "Reservations[].Instances[].{Name: Tags[?Key == 'Name'].Value | [0], Id: InstanceId, State: State.Name, Type: InstanceType, DNS: PublicDnsName}"
```

```
--------------------------------------------------------------------------------------------------------
|                                           DescribeInstances                                          |
+--------------------------------------------------+-----------+---------------+----------+------------+
|                        DNS                       |    Id     |     Name      |  State   |   Type     |
+--------------------------------------------------+-----------+---------------+----------+------------+
|                                                  |  i-abc123 |  None         |  stopped |  m3.large  |
|                                                  |  i-def456 |  amazon linux |  stopped |  t2.micro  |
|  ec2-192-168-1-1.us-west-2.compute.amazonaws.com |  i-ghi789 |  proxy        |  running |  t2.micro  |
+--------------------------------------------------+-----------+---------------+----------+------------+
```

