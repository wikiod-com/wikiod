---
title: "Elasticsearch Configuration "
slug: "elasticsearch-configuration-"
draft: false
images: []
weight: 9880
type: docs
toc: true
---

Elasticsearch comes with a set of defaults that provide a good out of the box experience for development. The implicit statement there is that it is not necessarily great for production, which must be tailored for your own needs and therefore cannot be predicted.

The default settings make it easy to download and run multiple nodes _on the same machine_ without any configuration changes.

## Where are the settings?

Inside each installation of Elasticsearch is a `config/elasticsearch.yml`. That is where the following [settings](https://www.elastic.co/guide/en/elasticsearch/reference/current/setup-configuration.html) live:

- `cluster.name`
    - The name of the cluster that the node is joining. All nodes in the same cluster **must** share the same name.
    - Currently defaults to `elasticsearch`.
- `node.*`
    - `node.name`
        - If not supplied, a random name will be generated _each time the node starts_. This can be fun, but it is not good for production environments.
        - Names do _not_ have to be unique, but they **should** be unique.
    - `node.master`
        - A boolean setting. When `true`, it means that the node is an eligible master node and it can be _the_ elected master node.
        - Defaults to `true`, meaning every node is an eligible master node.
    - `node.data`
        - A boolean setting. When `true`, it means that the node stores data and handles search activity.
        - Defaults to `true`.
- `path.*`
    - `path.data`
        - The location that files are written for the node. _All nodes use this directory_ to store metadata, but data nodes will also use it to store/index documents.
        - Defaults to `./data`.
             - This means that `data` will be created for you as a peer directory to `config` _inside_ of the Elasticsearch directory.
    - `path.logs`
        - The location that log files are written.
        - Defaults to `./logs`.
- `network.*`
    - `network.host`
        - Defaults to `_local_`, which is effectively `localhost`.
            - This means that, by default, nodes cannot be communicated with from outside of the current machine!

    - `network.bind_host`
        - Potentially an array, this tells Elasticsearch what addresses of the current machine to bind sockets too.
            - It is this list that enables machines from outside of the machine (e.g., other nodes in the cluster) to talk to this node.
        - Defaults to `network.host`.
    - `network.publish_host`
        - A singular host that is used to advertise to other nodes how to best communicate with this node.
            - When supplying an array to `network.bind_host`, this should be the _one_ host that is intended to be used for inter-node communication.
        - Defaults to network.host`.
- `discovery.zen.*`
    - `discovery.zen.minimum_master_nodes`
        - Defines quorum for master election. This **must** be set using this equation: `(M / 2) + 1` where `M` is the number of _eligible_ master nodes (nodes using `node.master: true` implicitly or explicitly).
        - Defaults to `1`, which only is valid for a single node cluster!
    - `discovery.zen.ping.unicast.hosts`
        - The mechanism for joining this node to the rest of a cluster.
        - This _should_ list eligible master nodes so that a node can find the rest of the cluster.
        - The value that should be used here is the `network.publish_host` of those other nodes.
        - Defaults to `localhost`, which means it only looks on the local machine for a cluster to join.

## What type of settings exist?

Elasticsearch provides three different types of settings:

- Cluster-wide settings
    - These are settings that apply to everything in the cluster, such as all nodes or all indices.
- Node settings
    - These are settings that apply to just the current node.
- Index settings
    - These are settings that apply to just the index.

Depending on the setting, it can be:

- Changed dynamically at runtime
- Changed following a restart (close / open) of the index
    - Some index-level settings do not require the index to be closed and reopened, but might require the index to be forceably re-merged for the setting to apply.
        - The compression level of an index is an example of this type of setting. It can be changed dynamically, but only new _segments_ take advantage of the change. So if an index will not change, then it never takes advantage of the change unless you force the index to recreate its segments.
- Changed following a restart of the node
- Changed following a restart of the cluster
- Never changed

Always check the documentation for your version of Elasticsearch for what you can or cannot do with a setting.

## How can I apply settings?

You can set settings a few ways, some of which are not suggested:

- Command Line Arguments

In Elasticsearch 1.x and 2.x, you can submit most settings as Java System Properties prefixed with `es.`:

    $ bin/elasticsearch -Des.cluster.name=my_cluster -Des.node.name=`hostname`

In Elasticsearch 5.x, this changes to avoid using Java System Properties, instead using a custom argument type with `-E` taking the place of `-Des.`:

    $ bin/elasticsearch -Ecluster.name=my_cluster -Enode.name=`hostname`

This approach to applying settings works great when using tools like Puppet, Chef, or Ansible to start and stop the cluster. However it works very poorly when doing it manually.

- YAML settings
    - Shown in examples
- Dynamic settings
    - Shown in examples

The order that settings are applied are in the order of most dynamic:

1. Transient settings
2. Persistent settings
3. Command line settings
4. YAML (static) settings

If the setting is set twice, once at any of those levels, then the highest level takes effect.

## Dynamic Index Settings for Multiple Indices at the same time
You can apply the same change shown in the `Index Settings` example to _all_ existing indices with one request, or even a subset of them:

    PUT /*/_settings
    {
      "index": {
        "indexing.slowlog.threshold.index.warn": "1s",
        "search.slowlog.threshold": {
          "fetch.warn": "500ms",
          "query.warn": "2s"
        }
      }
    }

or

    PUT /_all/_settings
    {
      "index": {
        "indexing.slowlog.threshold.index.warn": "1s",
        "search.slowlog.threshold": {
          "fetch.warn": "500ms",
          "query.warn": "2s"
        }
      }
    }

or

    PUT /_settings
    {
      "index": {
        "indexing.slowlog.threshold.index.warn": "1s",
        "search.slowlog.threshold": {
          "fetch.warn": "500ms",
          "query.warn": "2s"
        }
      }
    }

If you prefer to more selectively do it as well, then you can select multiple without supply all:

    PUT /logstash-*,my_other_index,some-other-*/_settings
    {
      "index": {
        "indexing.slowlog.threshold.index.warn": "1s",
        "search.slowlog.threshold": {
          "fetch.warn": "500ms",
          "query.warn": "2s"
        }
      }
    }

## Static Elasticsearch Settings
Elasticsearch uses a YAML (Yet Another Markup Language) configuration file that can be found inside the default Elasticsearch directory ([RPM and DEB installs change this location amongst other things](https://www.elastic.co/guide/en/elasticsearch/reference/current/setup-dir-layout.html)).

You can set basic settings in `config/elasticsearch.yml`:

    # Change the cluster name. All nodes in the same cluster must use the same name!
    cluster.name: my_cluster_name
    
    # Set the node's name using the hostname, which is an environment variable!
    # This is a convenient way to uniquely set it per machine without having to make
    #  a unique configuration file per node.
    node.name: ${HOSTNAME}
    
    # ALL nodes should set this setting, regardless of node type
    path.data: /path/to/store/data
    
    # This is a both a master and data node (defaults)
    node.master: true
    node.data: true
    
    # This tells Elasticsearch to bind all sockets to only be available
    #  at localhost (default)
    network.host: _local_

## Persistent Dynamic Cluster Settings
If you need to apply a setting dynamically after the cluster has already started, and it can actually be set dynamically, then you can set it using `_cluster/settings` API.

Persistent settings are one of the two type of cluster-wide settings that can be applied. A persistent setting **will** survive a full cluster restart.

Note: Not all settings can be applied dynamically. For example, the cluster's name cannot be renamed dynamically. Most node-level settings cannot be set dynamically either (because they cannot be targeted individually).

This is **not** the API to use to set index-level settings. You can tell that setting is an index level setting because it should start with `index.`. Settings whose name are in the form of `indices.` _are_ cluster-wide settings because they apply to all indices.

    POST /_cluster/settings
    {
      "persistent": {
        "cluster.routing.allocation.enable": "none"
      }
    }

**Warning**: In Elasticsearch 1.x and 2.x, you cannot _unset_ a persistent setting.

Fortunately, this has been improved in Elasticsearch 5.x and you can now remove a setting by setting it to `null`:

    POST /_cluster/settings
    {
      "persistent": {
        "cluster.routing.allocation.enable": null
      }
    }

An unset setting will return to its default, or any value defined at a lower priority level (e.g., command line settings).

## Transient Dynamic Cluster Settings
If you need to apply a setting dynamically after the cluster has already started, and it can actually be set dynamically, then you can set it using `_cluster/settings` API.

Transient settings are one of the two type of cluster-wide settings that can be applied. A transient setting will **not** survive a full cluster restart.

Note: Not all settings can be applied dynamically. For example, the cluster's name cannot be renamed dynamically. Most node-level settings cannot be set dynamically either (because they cannot be targeted individually).

This is **not** the API to use to set index-level settings. You can tell that setting is an index level setting because it should start with `index.`. Settings whose name are in the form of `indices.` _are_ cluster-wide settings because they apply to all indices.

    POST /_cluster/settings
    {
      "transient": {
        "cluster.routing.allocation.enable": "none"
      }
    }

**Warning**: In Elasticsearch 1.x and 2.x, you cannot unset a transient settings without a full cluster restart.

Fortunately, this has been improved in Elasticsearch 5.x and you can now remove a setting by setting it to null:

    POST /_cluster/settings
    {
      "transient": {
        "cluster.routing.allocation.enable": null
      }
    }

An unset setting will return to its default, or any value defined at a lower priority level (e.g., `persistent` settings).

## Index Settings
Index settings are those settings that apply to a single index. Such settings will start with `index.`. The exception to that rule is `number_of_shards` and `number_of_replicas`, which also exist in the form of `index.number_of_shards` and `index.number_of_replicas`.

As the name suggests, index-level settings apply to a single index. Some settings must be applied at creation time because they cannot be changed dynamically, such as the `index.number_of_shards` setting, which controls the number of primary shards for the index.

    PUT /my_index
    {
      "settings": {
        "index.number_of_shards": 1,
        "index.number_of_replicas": 1
      }
    }

or, in a more concise format, you can combine key prefixes at each `.`:

    PUT /my_index
    {
      "settings": {
        "index": {
          "number_of_shards": 1,
          "number_of_replicas": 1
        }
      }
    }

The above examples will create an index with the supplied settings. You can dynamically change settings per-index by using the index `_settings` endpoint. For example, here we dynamically change the [slowlog settings](https://www.elastic.co/guide/en/elasticsearch/reference/current/index-modules-slowlog.html) for _only_ the warn level:

    PUT /my_index/_settings
    {
      "index": {
        "indexing.slowlog.threshold.index.warn": "1s",
        "search.slowlog.threshold": {
          "fetch.warn": "500ms",
          "query.warn": "2s"
        }
      }
    }

**Warning**: Elasticsearch 1.x and 2.x did not very strictly validate index-level setting names. If you had a typo, or simply made up a setting, then it would blindly accept it, but otherwise ignore it. Elasticsearch 5.x strictly validates setting names and it will reject any attempt to apply index settings with an unknown setting(s) (due to typo or missing plugin). Both statements apply to dynamically changing index settings and at creation time.

