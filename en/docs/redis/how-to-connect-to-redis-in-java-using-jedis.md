---
title: "How to Connect to Redis in Java using Jedis"
slug: "how-to-connect-to-redis-in-java-using-jedis"
draft: false
images: []
weight: 9790
type: docs
toc: true
---

There are more than ten different client libraries to use with Redis in Java.  One of the most popular clients is [Jedis](https://github.com/xetorthio/jedis).

Further information:

- [Java Redis Clients](https://redis.io/clients#java)
- [Jedis Github Repository](https://github.com/xetorthio/jedis)
- [Jedis Documentation/Wiki](https://github.com/xetorthio/jedis/wiki)


## Connecting to Redis
**Using a Pool**

Most code will want to connect to Redis using a pool of shared connection objects. Connecting to Redis using a pool involves two different code block.  At initialization time, your application needs to create the connection pool:

        JedisPoolConfig poolCfg = new JedisPoolConfig();
        poolCfg.setMaxTotal(3);

        pool = new JedisPool(poolCfg, hostname, port, 500, password, false);

The `JedisPoolConfig` provides options for tuning the pool.  

As your application processes it's workload, you will need to get a connection from the shared pool using the following code:

        try (Jedis jedis = pool.getResource()) {

            ...
        }

Best practice is to get the `Jedis` connection object from the pool within a try-with-resources block.

**Without Pools**

In some cases, such as a simple application or an integration test, you may not want to deal with shared pools and instead create the `Jedis` connection object directly.  That can be accomplished with the following code:

    try (Jedis jedis = new Jedis(hostname, port)) {
        jedis.connect();
        jedis.auth(password);
        jedis.select(db);

        . . .
    }

Again, best practice is to create the Jedis client object within a try-with-resources block.
 


## Getting Jedis
The Jedis library is generally added to Java project using a dependency management system built into the build environment of the project.  Two popular Java build systems are Maven and Gradle.  

**Using Gradle**

To add the Jedis library to a Gradle project, you will need configure a repository and add a dependency.  The following snippet shows how to add version 2.9.0 of the Jedis library to a Gradle project.

    repositories {
        mavenCentral()
    }

    dependencies {
        compile 'redis.clients:jedis:2.9.0'
    }


**Using Maven**

To add Jedis to a Maven project, you need to add a dependency to your dependency list and provide the coordinates of the library.  The following snippet would be added to your pom.xml file:

    <dependencies>
        <dependency>
            <groupId>redis.clients</groupId>
            <artifactId>jedis</artifactId>
            <version>2.9.0</version>
        </dependency>
    </dependencies>


## Executing Basic Get/Set Commands
Once you have established a connection to Redis you can get and set values using the `Jedis` connection object:

**Get**

    String value = jedis.get(myKey);

**Set**

    jedis.put(myKey, "some value");



## Executing Commands
To execute a Redis command using Jedis, you make method calls against the `Jedis` object you created from the pool.  Jedis exposes Redis commands as method calls, some example are:

    - String get(String key) 
    - Long geoadd(String key, double longitude, double latitude, String member)
    - List<String> hmget(String key, String... fields)
    - Long hsetnx(String key, String field, String value)

If you wanted to set the value of a String key in Redis you would use a code block similar to:

    try (Jedis jedis = pool.getResource()) {

       String myKey = "users:20";
       String myValue = "active";

       jedis.set(myKey, myValue);
    }


