---
title: "Setting up AEM as a Service"
slug: "setting-up-aem-as-a-service"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

Step-by-step guide on how-to setup AEM as a Service on a Linux server.

## Setting up AEM 6.x on CentOS 7
## Pre-requisites
1. AEM Installed on your server. Copy the path of the install (e.g: /mnt/crx)
2. Start AEM (e.g `java -jar cq-quickstart-author-p4502.jar`) once. This will generate all the necessary folders, especially **/mnt/crx/crx-quickstart/bin** that is required by the scripts.
2. Create a user who will have access to the service. (e.g: aem)

## Step-by-step guide
1. You will need root access 
2. Create these 2 files
   * aem

 
            #!/bin/bash
            #
            # /etc/rc.d/init.d/aem6
            #
            #
            # # of the file to the end of the tags section must begin with a #
            # character. After the tags section, there should be a blank line.
            # This keeps normal comments in the rest of the file from being
            # mistaken for tags, should they happen to fit the pattern.>
            #
            # chkconfig: 35 85 15
            # description: This service manages the Adobe Experience Manager java process.
            # processname: aem6
            # pidfile: /crx-quickstart/conf/cq.pid
             
            # Source function library.
            . /etc/rc.d/init.d/functions
             
            SCRIPT_NAME=`basename $0`
            AEM_ROOT=/opt/aem6
            AEM_USER=aem
             
            ########
            BIN=${AEM_ROOT}/crx-quickstart/bin
            START=${BIN}/start
            STOP=${BIN}/stop
            STATUS="${BIN}/status"
             
            case "$1" in
            start)
            echo -n "Starting AEM services: "
            su - ${AEM_USER} ${START}
            touch /var/lock/subsys/$SCRIPT_NAME
            ;;
            stop)
            echo -n "Shutting down AEM services: "
            su - ${AEM_USER} ${STOP}
            rm -f /var/lock/subsys/$SCRIPT_NAME
            ;;
            status)
            su - ${AEM_USER} ${STATUS}
            ;;
            restart)
            su - ${AEM_USER} ${STOP}
            su - ${AEM_USER} ${START}
            ;;
            reload)
            ;;
            *)
            echo "Usage: $SCRIPT_NAME {start|stop|status|reload}"
            exit 1
            ;;
            esac
        

   * aem.service

    

        [Unit]
        Description=Adobe Experience Manager
        
        [Service]
        Type=simple
        ExecStart=/usr/bin/aem start
        ExecStop=/usr/bin/aem stop
        ExecReload=/usr/bin/aem restart
        RemainAfterExit=yes
        
        [Install]
        WantedBy=multi-user.target

3. Open `aem` script file and update the below
   * AEM_ROOT (e.g: `/mnt/crx` is the root, where `/mnt/crx/crx-quickstart` is the full path)
   * AEM_USER (e.g: `aem`) 
4. SCP these files to the server
   * Copy `aem` to `/usr/bin/aem`
     * Example: From terminal on your desktop `$ scp <filename> user@1.1.1.1:/usr/bin/aem`
   * Copy `aem.service` to `/etc/system.d/system/aem.system`
     * Example: From terminal on your desktop `$ scp <filename> user@1.1.1.1:/etc/system.d/system/aem.system`
5. SSH to your server
   * `ssh user@1.1.1.1`
6. Give permissions to the files
   * `sudo chmod u+rwx /usr/bin/aem`
   * `sudo chmod u+rwx /etc/system.d/system/aem.system`
7. Update 
   * `cd /etc/system.d/system`
   * `systemctl enable aem.system`
8. You can restart the server or run the below commands to start AEM. Make sure you run **Pre-requisite Step 2** before running this command.

## Commands to START, RESTART and STOP AEM
1. Start AEM - `sudo service aem start`
2. Restart AEM - `sudo service aem restart`
3. Stop AEM - `sudo service aem stop`

## Notes
1. The example above was tested on CentOS 7
2. AEM 6.3 version was used. Although the above process should work for AEM 6.x

