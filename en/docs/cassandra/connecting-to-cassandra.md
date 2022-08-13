---
title: "Connecting to Cassandra"
slug: "connecting-to-cassandra"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

The Cassandra Driver from Datastax very much mirrors the Java JDBC MySQL driver.

`Session`, `Statement`, `PreparedStatement` are present in both drivers.

The Singleton Connection is from this question and answer: http://stackoverflow.com/a/24691456/671896

Feature wise, Cassandra 2 and 3 are identical.  Cassandra 3 introduced a complete rewrite of the data storage system.

## Java: Include the Cassandra DSE Driver
In your Maven project, add the following to your `pom.xml` file. The following versions are for Cassandra 3.x.

        <dependency>
            <groupId>com.datastax.cassandra</groupId>
            <artifactId>cassandra-driver-core</artifactId>
            <version>3.1.0</version>
        </dependency>
        <dependency>
            <groupId>com.datastax.cassandra</groupId>
            <artifactId>cassandra-driver-mapping</artifactId>
            <version>3.1.0</version>
        </dependency>
        <dependency>
            <groupId>com.datastax.cassandra</groupId>
            <artifactId>cassandra-driver-extras</artifactId>
            <version>3.1.0</version>
        </dependency>
        <dependency>
            <groupId>com.datastax.cassandra</groupId>
            <artifactId>dse-driver</artifactId>
            <version>1.1.0</version>
        </dependency>

## Java: Connect to a Local Cassandra Instance
Connecting to Cassandra is very similar to connecting to other datasources. With Cassandra, credentials are not required.  

    String cassandraIPAddress = "127.0.0.1";
    String cassandraKeyspace = "myKeyspace";
    String username = "foo";
    String password = "bar";
    
    com.datastax.driver.core.Cluster cluster = Cluster.builder()
        .addContactPoint(cassandraIPAddress)
        .withCredentials(username, password) // If you have setup a username and password for your node.
        .build();
    
    com.datastax.driver.core.Session session = cluster.connect(cassandraKeyspace);

    com.datastax.driver.core.Metadata metadata = cluster.getMetadata();

    // Output Cassandra connection status
    System.out.println("Connected to Cassandra cluster: " + metadata.getClusterName() + " with Partitioner: " + metadata.getPartitioner());

    // Loop through your entire Cluster.
    for (Host host : metadata.getAllHosts()) {
        System.out.println("Cassandra Host Address: " + host.getAddress() + " | Is Up = " + host.isUp());
    }

## Java: Connect Using a Singleton
    public enum Cassandra {

        DB;

        private Session session;
        private Cluster cluster;
        private static final Logger LOGGER = LoggerFactory.getLogger(Cassandra.class);

        /**
         * Connect to the cassandra database based on the connection configuration provided.
         * Multiple call to this method will have no effects if a connection is already established
         * @param conf the configuration for the connection
         */
        public void connect(ConnectionCfg conf) {
            if (cluster == null && session == null) {
                cluster = Cluster.builder().withPort(conf.getPort()).withCredentials(conf.getUsername(), conf.getPassword()).addContactPoints(conf.getSeeds()).build();
                session = cluster.connect(conf.getKeyspace());
            }
            Metadata metadata = cluster.getMetadata();
            LOGGER.info("Connected to cluster: " + metadata.getClusterName() + " with partitioner: " + metadata.getPartitioner());
            metadata.getAllHosts().stream().forEach((host) -> {
                LOGGER.info("Cassandra datacenter: " + host.getDatacenter() + " | address: " + host.getAddress() + " | rack: " + host.getRack());
            });
        }

        /**
         * Invalidate and close the session and connection to the cassandra database
         */
        public void shutdown() {
            LOGGER.info("Shutting down the whole cassandra cluster");
            if (null != session) {
                session.close();
            }
            if (null != cluster) {
                cluster.close();
            }
        }

        public Session getSession() {
            if (session == null) {
                throw new IllegalStateException("No connection initialized");
            }
            return session;
        }
    }

Using the Singleton connection

    public void cassandra() throws Exception {
        Cassandra.DB.connect();
        Cassandra.DB.getSession().execute(/* CQL | Statement | PreparedStatement */)
        Cassandra.DB.close();
    }

