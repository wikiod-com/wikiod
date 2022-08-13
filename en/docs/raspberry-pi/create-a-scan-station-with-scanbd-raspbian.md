---
title: "Create a scan station with scanbd (Raspbian)"
slug: "create-a-scan-station-with-scanbd-raspbian"
draft: false
images: []
weight: 9813
type: docs
toc: true
---

## Introduction and installation
The purpose of this documentation is to use a scanner without any user interface. A common use is to upload a PDF scanned file directly to Google Drive or Dropbox just by pressing scanner-buttons.

scanbd permits to trigger actions from the scanner buttons, it uses sane.

What make things a little bit trickier is that by polling the scanner, scanbd locks the device. As we also need to scan from local tools (like scanimage), we need to set a proxy that's going to interrupt scandb from polling when needed. This is the job of scanbm, which monitors the network and give priority over scanbd to any local access to the scanner.

Thus, we need to :

 - Set up the local sane configuration to only scan from the network (through scanbm)
 - Configure scanbd to have access and poll the local scanner

As *root*:

    apt-get update
    apt-get install libconfuse-dev libsane-dev libudev-dev libusb-dev xinetd


> The latest version of scanbd can be find here : https://sourceforge.net/projects/scanbd/files/releases/

    # Download the latest version of **scanbd**.
    wget https://sourceforge.net/projects/scanbd/files/releases/scanbd-1.4.4.tgz/download

    # Uncompress
    tar xvzf scanbd-1.4.4.tgz && cd 1.4.4

    # Configure with the installation path in /etc/
    ./configure --disable-Werror

    # install it
    make
    make install

    # Add a dbus policy to authorize "saned" user to manage scanbd :
    cp integration/scanbd_dbus.conf /etc/dbus-1/system.d/

    # Add the init script to manage the daemon :
    cp integration/scanbd.debian /etc/init.d/scanbd

**scanbd** is now installed but not yet configured.




## Configure sane to scan from the network
The local configuration of sane is inside `/etc/saned.d`


----------

**/etc/sane.d/dll.conf**

    # /etc/sane.d/dll.conf - Configuration file for the SANE dynamic backend loader
    #
    # Backends can also be enabled by configuration snippets under
    # /etc/sane.d/dll.d directory -- packages providing backends should drop
    # a config file similar to dll.conf in this directory, named after the package.
    #
    
    # The next line enables the network backend; comment it out if you don't need
    # to use a remote SANE scanner over the network - see sane-net(5) and saned(8)

    net

`dll.conf` contains only the `net` backend.


----------


**/etc/sane.d/net.conf :**

    # This is the net backend config file.
    
    ## net backend options
    # Timeout for the initial connection to saned. This will prevent the backend
    # from blocking for several minutes trying to connect to an unresponsive
    # saned host (network outage, host down, ...). Value in seconds.
    connect_timeout = 3
    
    ## saned hosts
    # Each line names a host to attach to.
    # If you list "localhost" then your backends can be accessed either
    # directly or through the net backend.  Going through the net backend
    # may be necessary to access devices that need special privileges.
    
    localhost

At this point, **scanbm** is not yet configured, no scanner can be reached from the network. We need to set up scanbm along with scanbd to test the set up.

## Configure scanbd to poll the local scanner
**Identify the local scanner**
------------------------------

By using lsusb, identify the **productId** (1909 here) :

    pi:# lsusb
    pi:# Bus 001 Device 005: ID 04a9:1909 Canon, Inc. CanoScan LiDE 110

With that **productId**, grep the correct configuration file (it depends of your scanner model, for me it is **genesys.conf**) : 

    pi:# grep 1909 /etc/sane.d/*conf
    pi:# /etc/sane.d/genesys.conf:usb 0x04a9 0x1909

Copy the file inside your scanbd configuration folder:

    cp /etc/sane.d/genesys.conf /usr/local/etc/scanbd/
    cp /etc/sane.d/dll.conf /usr/local/etc/scanbd/

Edit **/usr/local/etc/scanbd/dll.conf**  and replace `net` with the correct scanner backend:

    # /etc/sane.d/dll.conf - Configuration file for the SANE dynamic backend loader
    #
    # Backends can also be enabled by configuration snippets under
    # /etc/sane.d/dll.d directory -- packages providing backends should drop
    # a config file similar to dll.conf in this directory, named after the package.
    #
    
    # The next line enables the network backend; comment it out if you don't need
    # to use a remote SANE scanner over the network - see sane-net(5) and saned(8)

    genesys


----------


**Confirm that the local scanner is found by scanbd**
-------------------------------------------------


Now we are ready to test :

    scanbd -d7 -f

> Avoid as much as you can to manipulate the env var SANE_CONFIG_DIR. It can break things, the default value is already configured in /usr/local/etc/scanbd/scanbd.conf and in the init script.

Output should be similar to : 

    scanbd: foreground
    scanbd: reading config file /usr/local/etc/scanbd/scanbd.conf
    scanbd: debug on: level: 7
    scanbd: dropping privs to uid saned
    scanbd: dropping privs to gid scanner
    scanbd: group scanner has member: 
    scanbd: saned
    scanbd: pi
    scanbd: drop privileges to gid: 110
    scanbd: Running as effective gid 110
    scanbd: drop privileges to uid: 110
    scanbd: Running as effective uid 110
    scanbd: dbus_init
    scanbd: dbus match type='signal',interface='org.freedesktop.Hal.Manager'
    scanbd: SANE_CONFIG_DIR=/usr/local/etc/scanbd/sane.d
    scanbd: sane version 1.0
    scanbd: Scanning for local-only devices
    scanbd: found device: genesys:libusb:001:005 Canon LiDE 110 flatbed scanner


The most important here is`found device`. 

> What if "found device" doesn't appear ?
> -------------------------------------
> 
> You should check again your configuration,
> especially the identification part of this documentation and clear any eventual  SANE_CONFIG_DIR env var wrongly set by typing :
> `unset SANE_CONFIG_DIR`


----------

**Configure xinetd to make scanbm listening on the network**
------------------------------------------------------------

Create `/etc/xinetd.d/sane-port` which contains :

    service sane-port
    {
            port        = 6566
            socket_type = stream
            wait        = no
            user        = saned
            group       = scanner
            server      = /usr/local/sbin/scanbm
            server_args = scanbm -c /usr/local/etc/scanbd/scanbd.conf
            disable     = no
    }

> **/!\\**  Double-check the path of **scanbm** with `which scanbm`

Then, restart **xinetd** and start **scanbd** : 

    service xinetd restart
    /etc/init.d/scanbd start
`scanimage -L` should now display the scanner.

> Most issues at this stage concern the permissions. Ensure that
> `scanimage -L` works when you are logged as saned by doing `sudo -u saned
> -s`.
>
> If not, check USB permissions :  `ls -al /dev/bus/usb/***`



## Trigger actions from the scanner buttons

Actions are located in `/usr/local/etc/scanbd/scanbd.conf`.
I have 4 buttons that are **scan**, **copy**, **email** and **file**. 

> The default config file doesn't include all actions per default, you
> will probably have to add the block manually. You can have less or more buttons
> depending of your scanner model.

For each action, we will set a custom path for the **script** option.

    action scan {
        filter = "^scan.*"
        numerical-trigger {
                from-value = 1
                to-value   = 0
        }
        desc   = "Scan to file"
        # script must be an relative path starting from scriptdir (see above), 
        # or an absolute pathname. 
        # It must contain the path to the action script without arguments
        # Absolute path example: script = "/some/path/foo.script 
        script = "/home/pi/scan.sh"
    }
            
Don't forget to comment any other default action at the end of scanbd.conf :

    # devices 
    # each device can have actions and functions, you can disable not relevant devices
    #include(scanner.d/avision.conf)
    #include(scanner.d/fujitsu.conf)
    #include(scanner.d/hp.conf)
    #include(scanner.d/pixma.conf)
    #include(scanner.d/snapscan.conf)
    #include(scanner.d/canon.conf)


----------


You can now create your custom script to handle each action :

> Each line relative to `/sys/class/leds/led0/trigger` are for controlling the LED to monitor what's going on. You can do whatever you want, `cat /sys/class/leds/led0/trigger` gives you all different pattern of lights.

**/home/pi/scan.sh**

    #!/bin/bash
    
    # don't forget to create the folder
    scan_dir=/home/pi/scanned-files
    datetime=`date +%F_%H%M%S`
    
    echo none >/sys/class/leds/led0/trigger
    
    case $SCANBD_ACTION in
      scan)
        filename=file-$datetime
        logger -t "scanbd: $0" "$SCANBD_DEVICE $SCANBD_ACTION - scanning --resolution 150 --mode Color --depth 8 --format=tiff to $scan_dir/$filename.jpg"
        echo timer >/sys/class/leds/led0/trigger
        scanimage -d $SCANBD_DEVICE --resolution 150 --mode Color --depth 8 --format=tiff  --brightness 5 --contrast 20 | convert tiff:- -compress jpeg $scan_dir/$filename.pdf
        echo none >/sys/class/leds/led0/trigger
        logger -t "scanbd: $0" "Finished scanning"
        ;;
        
      email)
        logger -t "scanbd: $0" "Emailing $scan_dir/file-*pdf"
        echo heartbeat >/sys/class/leds/led0/trigger
        # here are the lines to send the file
        echo none >/sys/class/leds/led0/trigger

    esac

