---
title: "Repairs in Cassandra"
slug: "repairs-in-cassandra"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## Parameters
| Option/Flag | Description |
| ----------- | ----------- |
| *option* | *option description*|
| -h | hostname.  Defaults to "localhost."  If you do not specify a host repair is run on the same host that the command is executed from. |
| -p | JMX port.  The default is 7199. |
| -u | username. Only required if JMX security is enabled. |
| -pw | password. Only required if JMX security is enabled. |
| *flag* | *flag description*|
| -local | Only compare and stream data from nodes in the "local" data center. |
| -pr | "Partitioner Range" repair.  Only repair the primary token range for a replica.  Faster than repairing all ranges of your replicas, as it prevents repairing the same data multiple times.  Note that if you use this option for repairing one node, you must also use it for the rest of your cluster, as well. |
| -par | Run repairs in parallel.  Gets repairs done faster, but significantly restricts the cluster's ability to handle requests.|
| -hosts | Allows you to specify a comma-delimited list of nodes to stream your data from.  Useful if you have nodes that are known to be "good."  While it is documented as a valid option for Cassandra 2.1+, it also works with Cassandra 2.0. |

**Cassandra Anti-Entropy Repairs:**

Anti-entropy repair in Cassandra has two distinct phases.  To run successful, performant repairs, it is important to understand both of them.

 - **Merkle Tree calculations**: This computes the differences between the nodes and their replicas.

 - **Data streaming**: Based on the outcome of the Merkle Tree calculations, data is scheduled to be streamed from one node to another.  This is an attempt to synchronize the data between replicas.

**Stopping a Repair**:

You can stop a repair by issuing a STOP VALIDATION command from nodetool:

    $ nodetool stop validation

**How do I know when repair is completed?**

You can check for the first phase of repair (Merkle Tree calculations) by checking `nodetool compactionstats`.

You can check for repair streams using `nodetool netstats`.  Repair streams will also be visible in your logs.  You can `grep` for them in your system logs like this:

    $ grep Entropy system.log

    INFO [AntiEntropyStage:1] 2016-07-25 07:32:47,077 RepairSession.java (line 164) [repair #70c35af0-526e-11e6-8646-8102d8573519] Received merkle tree for test_users from /192.168.14.3
    INFO [AntiEntropyStage:1] 2016-07-25 07:32:47,081 RepairSession.java (line 164) [repair #70c35af0-526e-11e6-8646-8102d8573519] Received merkle tree for test_users from /192.168.16.5
    INFO [AntiEntropyStage:1] 2016-07-25 07:32:47,091 RepairSession.java (line 221) [repair #70c35af0-526e-11e6-8646-8102d8573519] test_users is fully synced
    INFO [AntiEntropySessions:4] 2016-07-25 07:32:47,091 RepairSession.java (line 282) [repair #70c35af0-526e-11e6-8646-8102d8573519] session completed successfully

Active repair streams can also be monitored with this (Bash) command:

    $ while true; do date; diff <(nodetool -h 192.168.0.1 netstats) <(sleep 5 && nodetool -h 192.168.0.1 netstats); done

ref: http://stackoverflow.com/questions/25064717/how-do-i-know-if-nodetool-repair-is-finished/25081283#25081283

**How to check for stuck or orphaned repair streams?**

On each node, you can monitor this with `nodetool tpstats`, and check for anything "blocked" on the "AntiEntropy" lines.

    $ nodetool tpstats
    Pool Name                    Active   Pending      Completed   Blocked  All time blocked
    ...
    AntiEntropyStage                  0         0         854866         0                 0
    ...
    AntiEntropySessions               0         0           2576         0                 0
    ...

## Examples for running Nodetool Repair
Usage:

    $ nodetool repair [-h | -p | -pw | -u] <flags> [ -- keyspace_name [table_name]]

**Default Repair Option**

    $ nodetool repair

This command will repair the current node's primary token range (i.e. range which it owns) along with the replicas of other token ranges it has in all tables and all keyspaces on the current node:

For e.g. If you have replication factor of 3 then total of 5 nodes will be involved in repair:
2 nodes will be fixing 1 partition range
2 nodes will be fixing 2 partition ranges
1 node will be fixing 3 partition ranges. (Command was run on this node)

**Repair in Parallel** 
    
    $ nodetool repair -par

This command will run do perform the same task as default repair but by running the repair in parallel on the nodes containing replicas.

**Repair Primary Token Range**

This command repairs only the primary token range of the node in all tables and all keyspaces on the current node:

    $ nodetool repair -pr

Repair only the local Data Center on which the node resides:

    $ nodetool repair -pr -local

Repair only the primary range for all replicas in all tables and all keyspaces on the current node, only by streaming from the listed nodes:

    $ nodetool repair -pr -hosts 192.168.0.2, 192.168.0.3, 192.168.0.4

Repair only the primary range for all replicas in the stackoverflow keyspace on the current node:

    $ nodetool repair -pr -- stackoverflow

Repair only the primary range for all replicas in the test_users table of the stackoverflow keyspace on the current node:

    $ nodetool repair -pr -- stackoverflow test_users

