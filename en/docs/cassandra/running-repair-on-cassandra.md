---
title: "Running Repair on Cassandra"
slug: "running-repair-on-cassandra"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Syntax
 - **Synopsis**
 - `nodetool [node-options] repair [other-options]`

 - **Node options**
 - `[(-h <host> | --host <host>)]` 
 - `[(-p <port> | --port <port>)]` 
 - `[(-pw <password> | --password <password>)]` 
 - `[(-pwf <passwordFilePath> | --password-file <passwordFilePath>)]` 
 - `[(-u <username> | --username <username>)]`

 - **Other options**
 - `[(-dc <specific_dc> | --in-dc <specific_dc>)...]`
 - `[(-dcpar | --dc-parallel)]` 
 - `[(-et <end_token> | --end-token <end_token>)]`
 - `[(-full | --full)]`
 - `[(-hosts <specific_host> | --in-hosts <specific_host>)...]`
 - `[(-j <job_threads> | --job-threads <job_threads>)]`
 - `[(-local | --in-local-dc)]` 
 - `[(-pl | --pull)]`
 - `[(-pr | --partitioner-range)]` 
 - `[(-seq | --sequential)]`
 - `[(-st <start_token> | --start-token <start_token>)]` 
 - `[(-tr | --trace)]`
 - `[--]` 
 - `[<keyspace> <tables>...]`



## Parameters
| Parameter                                                   | Details                                                                                                                                                                                        |
| ----------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `-dc <specific_dc>`, `--in-dc <specific_dc>`                    | Use `-dc` to repair specific datacenters                                                                                                                                                         |
| `-dcpar`, `--dc-parallel`                                       | Use `-dcpar` to repair data centers in parallel.                                                                                                                                                 |
| `-et <end_token>`, `--end-token <end_token>`                    | Use `-et` to specify a token at which repair range ends                                                                                                                                          |
| `-full`, `--full`                                               | Use `-full` to issue a full repair.                                                                                                                                                              |
| `-h <host>`, `--host <host>`                                    | Node hostname or ip address                                                                                                                                                                    |
| `-hosts <specific_host>`, `--in-hosts <specific_host>`          | Use `-hosts` to repair specific hosts                                                                                                                                                            |
| `-j <job_threads>`, `--job-threads <job_threads>`               | Number of threads to run repair jobs. Usually this means number of CFs to repair concurrently. WARNING: increasing this puts more load on repairing nodes, so be careful. (default: 1, max: 4) |
| `-local`, `--in-local-dc`                                       | Use `-local` to only repair against nodes in the same datacenter                                                                                                                                 |
| `-p <port>`, `--port <port>`                                    | Remote jmx agent port number                                                                                                                                                                   |
| `-pl`, `--pull`                                                 | Use `--pull` to perform a one way repair where data is only streamed from a remote node to this node.                                                                                            |
| `-pr`, `--partitioner-range`                                    | Use `-pr` to repair only the first range returned by the partitioner                                                                                                                             |
| `-pw <password>`, `--password <password>`                       | Remote jmx agent password                                                                                                                                                                      |
| `-pwf <passwordFilePath>`, `--password-file <passwordFilePath>` | Path to the JMX password file                                                                                                                                                                  |
| `-seq`, `--sequential`                                          | Use `-seq` to carry out a sequential repair                                                                                                                                                      |
| `-st <start_token>`, `--start-token <start_token>`              | Use `-st` to specify a token at which the repair range starts                                                                                                                                    |
| `-tr`, `--trace`                                                | Use `-tr` to trace the repair. Traces are logged to `system_traces.events`.                                                                                                                        |
| `-u <username>`, `--username <username>`                        | Remote jmx agent username                                                                                                                                                                      |
| `--`                                                          | This option can be used to separate command-line options from the list of argument, (useful when arguments might be mistaken for command-line options                                          |
| `[<keyspace> <tables>...]`                                    | The keyspace followed by one or many tables                                                                                                                                                    |


## Running repair on Cassandra

Run repair on a particular partition range.
----------

    nodetool repair -pr

Run repair on the whole cluster. 
----------

    nodetool repair

Run repair in parallel mode.
----------

    nodetool repair -par




