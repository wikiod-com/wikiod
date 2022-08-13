---
title: "logging"
slug: "logging"
draft: false
images: []
weight: 9916
type: docs
toc: true
---

This topic shows how to use and configure logging (log4j) in Flink applications.

## Logging configuration
## Local mode

In local mode, for example when running your application from an IDE, you can configure `log4j` as usual, i.e. by making a `log4j.properties` available in the classpath. An easy way in maven is to create `log4j.properties` in the `src/main/resources` folder. Here is an example:

<!-- language: lang-properties -->

    log4j.rootLogger=INFO, console
    
    # patterns:
    #  d = date
    #  c = class
    #  F = file
    #  p = priority (INFO, WARN, etc)
    #  x = NDC (nested diagnostic context) associated with the thread that generated the logging event
    #  m = message
    
    
    # Log all infos in the console
    log4j.appender.console=org.apache.log4j.ConsoleAppender
    log4j.appender.console.layout=org.apache.log4j.PatternLayout
    log4j.appender.console.layout.ConversionPattern=%d{dd/MM/yyyy HH:mm:ss.SSS} %5p [%-10c] %m%n
    
    # Log all infos in flink-app.log
    log4j.appender.file=org.apache.log4j.FileAppender
    log4j.appender.file.file=flink-app.log
    log4j.appender.file.append=false
    log4j.appender.file.layout=org.apache.log4j.PatternLayout
    log4j.appender.file.layout.ConversionPattern=%d{dd/MM/yyyy HH:mm:ss.SSS} %5p [%-10c] %m%n
    
    # suppress info messages from flink
    log4j.logger.org.apache.flink=WARN


## Standalone mode

> In standalone mode, the actual configuration used is not the one in your `jar` file. This is because Flink has it own configuration files, which take precedence over your own. 

__Default files__:
Flink ships with the following default properties files:

 * `log4j-cli.properties`: Used by the Flink command line client (e.g. `flink run`) (not code executed on the cluster)
 * `log4j-yarn-session.properties`: Used by the Flink command line client when starting a YARN session (`yarn-session.sh`)
 * `log4j.properties`: JobManager/Taskmanager logs (both standalone and YARN)

Note that `${log.file}` default to `flink/log`. It can be overridden in `flink-conf.yaml`, by setting `env.log.dir`, 

> `env.log.dir` defines the directory where the Flink logs are saved. It has to be an absolute path.

__Log location__: the logs are _local_, i.e. they are produced in the machine(s) running the JobManager(s) / Taskmanager(s). 

__Yarn__: when running Flink on Yarn, you have to rely on the logging capabilities of Hadoop YARN. The most useful feature for that is the [YARN log aggregation](http://hortonworks.com/blog/simplifying-user-logs-management-and-access-in-yarn/). To enable it, set the `yarn.log-aggregation-enable` property to `true` in the `yarn-site.xml file`. Once that is enabled, you can retrieve all log files of a (failed) YARN session using:

    yarn logs -applicationId <application ID>

Unfortunately, logs are available _only after a session stopped running_, for example after a failure.






## Using a logger in your code
Add the `slf4j` dependency to your `pom.xml`:

    <properties>
        <slf4j.version>1.7.21</slf4j.version>
    </properties>

    <!-- ... --> 

    <dependency>
        <groupId>org.slf4j</groupId>
        <artifactId>slf4j-api</artifactId>
        <version>${slf4j.version}</version>
    </dependency>
    <dependency>
        <groupId>org.slf4j</groupId>
        <artifactId>slf4j-log4j12</artifactId>
        <version>${slf4j.version}</version>
    </dependency>


Create a logger object for use in your class:

    private Logger LOGGER = LoggerFactory.getLogger(FlinkApp.class);

In classes that need to be serialized, such as subclasses of `RichMapFunction`, don't forget to declare `LOGGER` as `transient`:

    private transient Logger LOG = LoggerFactory.getLogger(MyRichMapper.class);

In your code, use `LOGGER` as usual. Use placeholders (`{}`) to format objects and such:

    LOGGER.info("my app is starting");
    LOGGER.warn("an exception occurred processing {}", record, exception);

## Using different configuration(s) for each application
In case you need different settings for your various applications, there is (as of Flink 1.2) no easy way to do that.

If you use the _one-yarn-cluster-per-job_ mode of flink (i.e. you launch your scripts with: `flink run -m yarn-cluster` ...), here is a workaround :

1. create a `conf` directory somewhere near your project
2. create symlinks for all files in `flink/conf`:
      <!-- language: lang-shell --> 
      
        mkdir conf
        cd conf
        ln -s flink/conf/* .

3. replace the symlink `log4j.properties` (or any other file you want to change) by your own configuration

4. before launching your job, run 

    <!-- language: lang-shell --> 
        export FLINK_CONF_DIR=/path/to/my/conf

Depending on your version of flink, you might need to edit the file `flink/bin/config.sh`. If your run accross this line:

<!-- language: lang-shell --> 
    FLINK_CONF_DIR=$FLINK_ROOT_DIR_MANGLED/conf

change it with:

<!-- language: lang-shell --> 
    if [ -z "$FLINK_CONF_DIR" ]; then 
        FLINK_CONF_DIR=$FLINK_ROOT_DIR_MANGLED/conf; 
    fi



## Flink-on-Yarn workaround: get logs in real-time with rsyslog
Yarn does not by default aggregate logs before an application finishes, which can be problematic with streaming jobs that don't even terminate.

A workaround is to use [`rsyslog`](http://www.rsyslog.com/), which is available on most linux machines.

First, allow incoming udp requests by uncommenting the following lines in `/etc/rsyslog.conf`:

<!-- language: lang-properties --> 
    $ModLoad imudp
    $UDPServerRun 514

Edit your `log4j.properties` (see the other examples on this page) to use `SyslogAppender`:

<!-- language: lang-properties --> 
    log4j.rootLogger=INFO, file
    
    # TODO: change package logtest to your package
    log4j.logger.logtest=INFO, SYSLOG
    
    # Log all infos in the given file
    log4j.appender.file=org.apache.log4j.FileAppender
    log4j.appender.file.file=${log.file}
    log4j.appender.file.append=false
    log4j.appender.file.layout=org.apache.log4j.PatternLayout
    log4j.appender.file.layout.ConversionPattern=bbdata: %d{yyyy-MM-dd HH:mm:ss,SSS} %-5p %-60c %x - %m%n
    
    # suppress the irrelevant (wrong) warnings from the netty channel handler
    log4j.logger.org.jboss.netty.channel.DefaultChannelPipeline=ERROR, file
    
    # rsyslog
    # configure Syslog facility SYSLOG appender
    # TODO: replace host and myTag by your own
    log4j.appender.SYSLOG=org.apache.log4j.net.SyslogAppender
    log4j.appender.SYSLOG.syslogHost=10.10.10.102
    log4j.appender.SYSLOG.port=514
    #log4j.appender.SYSLOG.appName=bbdata
    log4j.appender.SYSLOG.layout=org.apache.log4j.EnhancedPatternLayout
    log4j.appender.SYSLOG.layout.conversionPattern=myTag: [%p] %c:%L - %m %throwable %n

The layout is important, because rsyslog treats a newline as a new log entry. Above, newlines (in stacktraces for example) will be skipped. If you really want multiline/tabbed logs to work "normally", edit `rsyslog.conf` and add:

<!-- language: lang-properties -->  
    $EscapeControlCharactersOnReceive off

The use of `myTag:` at the beginning of the `conversionPattern` is useful if you want to redirect all your logs into a specific file. To do that, edit `rsyslog.conf` and add the following rule:

<!-- language: lang-properties --> 
    if $programname == 'myTag' then /var/log/my-app.log
    & stop

