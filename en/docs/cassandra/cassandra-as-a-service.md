---
title: "Cassandra as a Service"
slug: "cassandra-as-a-service"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

This topic describes how to start Apache Cassandra as a service in windows and linux platforms. Remember you also start Cassandra from bin directory by running the batch or shell script.

## Linux

 1. Create the /etc/init.d/cassandra startup script.
 2. Edit the contents of the file:

        #!/bin/sh
        #
        # chkconfig: - 80 45
        # description: Starts and stops Cassandra
        # update daemon path to point to the cassandra executable
        DAEMON=<Cassandra installed directory>/bin/cassandra
        start() {
                echo -n "Starting Cassandra... "
                $DAEMON -p /var/run/cassandra.pid
                echo "OK"
                return 0
        }
        stop() {
                echo -n "Stopping Cassandra... "
                kill $(cat /var/run/cassandra.pid)
                echo "OK"
                return 0
        }
        case "$1" in
          start)
                start
                ;;
          stop)
                stop
                ;;
          restart)
                stop
                start
                ;;
          *)
                echo $"Usage: $0 {start|stop|restart}"
                exit 1
        esac
        exit $?

 3. Make the file executable:

    sudo chmod +x /etc/init.d/cassandra

 4. Add the new service to the list:

    sudo chkconfig --add cassandra

 5. Now you can manage the service from the command line:

        sudo /etc/init.d/cassandra start
        sudo /etc/init.d/cassandra stop
        sudo /etc/init.d/cassandra restart


## Windows

 1. Download the latest apache commons daemon from [Apache Commons Project Distributions][1].
 2. Extract the commons daemon in **&lt;Cassandra installed directory&gt;\bin**.
 3. Rename the extracted folder as daemon.
 4. Add **&lt;Cassandra installed directory&gt;** as **CASSANDRA_HOME** in windows environment variable.
 5. Edit the **cassandra.yaml** file in **&lt;Cassandra installed directory&gt;\conf** and uncomment the data_file_directories, commitlog_directory, saved_cache_directory and set the absolute paths.
 6. Edit **cassandra.bat** in **&lt;Cassandra installed directory&gt;\bin** and replace the value for the PATH_PRUNSRV as follows:

        for 32 bit windows, set PATH_PRUNSRV=%CASSANDRA_HOME%\bin\daemon\
        for 64 bit windows, set PATH_PRUNSRV=%CASSANDRA_HOME%\bin\daemon\amd64\

 7. Edit **cassandra.bat** and configure SERVICE_JVM for required service name.
    
        SERVICE_JVM="cassandra"

 8. With administrator privileges, run cassandra.bat install from command prompt.


  [1]: http://archive.apache.org/dist/commons/daemon/binaries/windows/

