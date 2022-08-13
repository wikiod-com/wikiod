---
title: "Packages and Initialization Scripts"
slug: "packages-and-initialization-scripts"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

There currently aren't any installation packages provided for Bosun or Scollector, only binaries on the [Bosun release][1] page. It is up to the end user to find the best way to deploy the files and run them as a service.


  [1]: https://github.com/bosun-monitor/bosun/releases

## Bosun systemd unit file
    #Create Bosun unit file at /etc/systemd/system/bosun.service
    [Unit]
    Description=Bosun Service
    After=network.target
    After=rsyslog.service
    
    [Service]
    Type=simple
    User=root
    ExecStart=/opt/bosun/bosun -c /opt/bosun/config/prod.conf
    Restart=on-abort
    
    [Install]
    WantedBy=multi-user.target
    
    #enable and start service
    #systemctl enable bosun
    #systemctl start bosun
    #If you edit this file, be sure to run `systemctl daemon reload` so Systemd recognizes the changes made

## TSDBRelay systemd unit file
[TSDBRelay][1] can be used to forward metrics to an OpenTSDB instance, send to Bosun for indexing, and relay to another opentsdb compatible instance for backup/DR/HA. It also has options to denormalize metrics with high tag cardinality or create redis/ledis backed external counters. 

    #Create tsdbrelay unit file at /etc/systemd/system/tsdbrelay.service
    [Unit]
    Description=tsdbrelay Service
    After=network.target
    
    [Service]
    Type=simple
    User=root
    ExecStart=/opt/tsdbrelay/tsdbrelay -b localhost:8070 -t localhost:4242 -l 0.0.0.0:5252  -r localhost:4243 #Local tsdb/bosun and influxdb opentsdb endpoint at 4243
    #For external counters add: -redis redishostname:6379 -db 0
    #For denormalized metrics: -denormalize=os.cpu__host,os.mem.used__host,os.net.bytes__host,os.net.bond.bytes__host,os.net.other.bytes__host,os.net.tunnel.bytes__host,os.net.virtual.bytes__host
    Restart=on-abort
    
    [Install]
    WantedBy=multi-user.target


  [1]: https://godoc.org/bosun.org/cmd/tsdbrelay

## Scollector init.d script
Example init script for scollector: 

```
#!/bin/bash
#
# scollector        Startup script for scollector.
#
# chkconfig: 2345 90 60
# description: scollector is a replacement for OpenTSDB's TCollector \
# and can be used to send metrics to a Bosun server

# Source function library.
. /etc/init.d/functions

RETVAL=0
PIDFILE=/var/run/scollector.pid

prog=scollector
exec=/opt/scollector/scollector-linux-amd64
scollector_conf=/opt/scollector/scollector.toml
scollector_logs=/var/log/scollector
scollector_opts="-conf $scollector_conf -log_dir=$scollector_logs"

lockfile=/var/lock/subsys/$prog

# Source config
if [ -f /etc/sysconfig/$prog ] ; then
    . /etc/sysconfig/$prog
fi

start() {
        [ -x $exec ] || exit 5
        umask 077
        echo -n $"Starting scollector: "
        daemon --check=$exec --pidfile="$PIDFILE" "{ $exec $scollector_opts & } ; echo \$! >| $PIDFILE"
        RETVAL=$?
        echo
        [ $RETVAL -eq 0 ] && touch $lockfile
        return $RETVAL
}
stop() {
        echo -n $"Shutting down scollector: "
        killproc -p "$PIDFILE" $exec
        RETVAL=$?
        echo
        [ $RETVAL -eq 0 ] && rm -f $lockfile
        return $RETVAL
}
rhstatus() {
        status -p "$PIDFILE" -l $prog $exec
}
restart() {
        stop
        start
}

case "$1" in
  start)
        start
        ;;
  stop)
        stop
        ;;
  restart)
        restart
        ;;
  status)
        rhstatus
        ;;
  *)
        echo $"Usage: $0 {start|stop|restart|status}"
        exit 3
esac

exit $?
```

## Bosun init.d script
Here is an init.d script for Bosun that includes setting Environmental Variables that can be used to hide secrets from the raw config. It uses http://software.clapper.org/daemonize/ to run the program as a daemon.

    #!/bin/sh
    #
    # /etc/rc.d/init.d/bosun
    # bosun
    #
    # chkconfig: - 98 02
    # description: bosun
    
    ### BEGIN INIT INFO
    # Provides:          bosun
    # Required-Start:    networking
    # Required-Stop:     networking
    # Default-Start:     2 3 4 5
    # Default-Stop:      0 1 6
    # Short-Description: Runs teh bosun
    # Description:       bosun
    
    ### END INIT INFO
    # Source function library.
    . /etc/rc.d/init.d/functions
    
    base_dir="/opt/bosun"
    exec="/opt/bosun/bosun"
    prog="bosun"
    config="${base_dir}/config/prod.conf"
    
    [ -e /etc/sysconfig/$prog ] && . /etc/sysconfig/$prog
    
    lockfile=/var/lock/subsys/$prog
    pidfile=/var/run/bosun.pid
    logfile=/var/log/$prog.log
    
    #These "secrets" can be used in the prod.conf using syntax like ${env.CHAT} or ${env.API_KEY}
    export CHAT=https://chat.company.com/rooms/123?key=123456789012345678901234567890
    export API_KEY=123456789012345678901234567890
    
    check() {
        $exec -t -c $config
        if [ $? -ne 0 ]; then
           echo "Errors found in configuration file, check it with '$exec -t'."
           exit 1
        fi
    }
    
    start() {
        [ -x $exec ] || exit 5
        [ -f $config ] || exit 6
        check
        echo -n $"Starting $prog: "
        # if not running, start it up here, usually something like "daemon $exec"
        ulimit -n 65536
        daemon daemonize -a -c $base_dir -e $logfile -o $logfile  -p $pidfile -l $lockfile $exec -c $config $OPTS
        retval=$?
        echo
        [ $retval -eq 0 ] && touch $lockfile
        return $retval
    }
    
    stop() {
        echo -n $"Stopping $prog: "
        # stop it here, often "killproc $prog"
        killproc -p $pidfile -d 5m
        retval=$?
        echo
        [ $retval -eq 0 ] && rm -f $lockfile
        return $retval
    }
    
    restart() {
        check
        stop
        start
    }
    
    reload() {
        restart
    }
    
    force_reload() {
        restart
    }
    
    rh_status() {
        # run checks to determine if the service is running or use generic status
        status $prog
    }
    
    rh_status_q() {
        rh_status >/dev/null 2>&1
    }
    
    
    case "$1" in
        start)
            rh_status_q && exit 0
            $1
            ;;
        stop)
            rh_status_q || exit 0
            $1
            ;;
        restart)
            $1
            ;;
        reload)
            rh_status_q || exit 7
            $1
            ;;
        force-reload)
            force_reload
            ;;
        status)
            rh_status
            ;;
        condrestart|try-restart)
            rh_status_q || exit 0
            restart
            ;;
        *)
            echo $"Usage: $0 {start|stop|status|restart|condrestart|try-restart|reload|force-reload}"
            exit 2
    esac

## Scollector systemd unit file
    #Create Scollector unit file at /etc/systemd/system/scollector.service
    [Unit]
    Description=Scollector Service
    After=network.target
    
    [Service]
    Type=simple
    User=root
    ExecStart=/opt/scollector/scollector -h mybosunserver.example.com
    Restart=on-abort
    
    [Install]
    WantedBy=multi-user.target

    #enable and start service
    #systemctl enable scollector
    #systemctl start scollector
    #If you edit this file, be sure to run `systemctl daemon reload` so Systemd recognizes the changes made

## Scollector and Bosun Packages for Chef/Puppet/Vagrant/Ansible
Chef Scollector Cookbook: https://github.com/alexmbird/chef-scollector

Chef Bosun Cookbook: https://github.com/ptqa/chef-bosun

Puppet scollector module: https://github.com/axibase/axibase-puppet-modules

Bosun Ansible/Vagrant example: https://github.com/gnosek/bosun-deploy

## Install scollector on CentOS 7
As a privileged user (root or sudo):

Create scollector directory:

`mkdir /opt/scollector`

In the /opt/scollector directory, download the latest binary build from the bosun/scollector site, [http://bosun.org/scollector/][1]

    wget https://github.com/bosun-monitor/bosun/releases/download/"version"/scollector-"OS"-"arch"

  ex:  
`wget https://github.com/bosun-monitor/bosun/releases/download/0.5.0/scollector-linux-amd64`

Create a symbolic link in /usr/local/bin:

    ln -s /opt/scollector/scollector-linux-amd64 /usr/local/bin/scollector

Create the configuration directory;

    mkdir /etc/scollector

Using this [guide][1] create your scollector configuration file, scollector.toml 

The path for the configuration file is then /etc/scollector/scollector.conf

ex:
   

     Host = "http://xxx.xxx.xxx.xxx:8070" #replace xxx with the IP of your Bosun server
        Hostname = "DevOps-Bosun-Prod"
        [[ICMP]]
          Host = "some.hostname.here"
        [[ICMP]]
          Host = "some.other.hostname.here"
        [tags]
          hostgroup = "system"
        #[[GoogleAnalytics]]
        #  ClientID = ""
        #  Secret = ""
        #  Token = ""

    
Create the Service file, /etc/systemd/system/scollector.service

ex:
    
    
    [Unit]
    Description=Scollector Service
    After=network.target
    
    [Service]
    Type=simple
    User=root
    ExecStart=/usr/local/bin/scollector -conf=/etc/scollector/scollector.toml
    Restart=on-abort
    
    
    [Install]
    WantedBy=multi-user.target

Tell Systemd that you have created a new service:

    systemctl enable scollector.service
Start scollector:

    systemctl start scollector
You can see if scollector has started by running:

    systemctl status scollector
Alternatively, you can view the system message log, you're looking for something like:

    Jul 29 23:19:27 bosun-prod systemd: Started Scollector Service.
    Jul 29 23:19:27 bosun-prod systemd: Starting Scollector Service...
    Jul 29 23:19:27 bosun-prod scollector[4363]: info: main.go:213: OpenTSDB host: http://127.0.0.1:8070


  [1]: https://www.wikiod.com/bosun/scollector-overview


