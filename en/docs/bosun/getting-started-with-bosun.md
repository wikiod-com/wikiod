---
title: "Getting started with Bosun"
slug: "getting-started-with-bosun"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Sample Configuration File
Here is an example of a Bosun config file used in a development environment:

<!-- language: lang-none -->
    tsdbHost = localhost:4242
    httpListen = :8070
    smtpHost = localhost:25
    emailFrom = bosun@example.org
    timeAndDate = 202,75,179,136
    ledisDir = ../ledis_data
    checkFrequency = 5m
    
    notification example.notification {
            email = alerts@example.org
            print = true
    }

In this case the config file indicates Bosun should connect to a local OpenTSDB instance on port 4242, listen for requests on port 8070 (on all IP addresses bound to the host), use the localhost SMTP system for email, display [additional time zones][1], use built in Ledis instead of external Redis for system state, and default alerts to a 5 minute interval.

The config also defines an example.notification that can be assigned to alerts, which would usually be included at the end of the config file (see sample alert example).


  [1]: http://stackoverflow.com/questions/34394387/how-to-specify-timezones-for-world-clock-in-bosun

## Docker Quick Start
The [quick start guide][1] includes information about using Docker to stand up a Bosun instance.

    $ docker run -d -p 4242:4242 -p 80:8070 stackexchange/bosun
    
This will create a new instance of Bosun which you can access by opening a browser to http://docker-server-ip. The docker image includes HBase/OpenTSDB for storing time series data, the Bosun server, and Scollector for gathering metrics from inside the bosun container. You can then point additional scollector instances at the Bosun server and use Grafana to create dashboards of OpenTSDB or Bosun metrics. 

The Stackexchange/Bosun image is designed only for testing. There are no alerts defined in the config file and the data will be deleted when the docker image is removed, but it is very helpful for getting a feel for how bosun works. For details on creating a production instance of Bosun see http://bosun.org/resources


  [1]: http://bosun.org/quickstart

## Sample Alert
Bosun alerts are defined in the config file using a [custom DSL][1]. They use functions to evaluate time series data and will generate alerts when the warn or crit expressions are non-zero. Alerts use templates to include additional information in the notifications, which are usually an email message and/or HTTP POST request.

<!-- language: lang-none -->
    template sample.alert {
        body = `<p>Alert: {{.Alert.Name}} triggered on {{.Group.host}}
        <hr>
        <p><strong>Computation</strong>
        <table>
            {{range .Computations}}
                <tr><td><a href="{{$.Expr .Text}}">{{.Text}}</a></td><td>{{.Value}}</td></tr>
            {{end}}
        </table>
        <hr>
        {{ .Graph .Alert.Vars.metric }}`

        subject = {{.Last.Status}}: {{.Alert.Name}} cpu idle at {{.Alert.Vars.q | .E}}% on {{.Group.host}}
    }
    
    notification sample.notification {
        email = alerts@example.com
    }
    
    alert sample.alert {
        template = sample.template
        $q = avg(q("sum:rate:linux.cpu{host=*,type=idle}", "1m"))
        crit = $q < 40
        notification = sample.notification
    }

The alert would send an email with the subject `Critical: sample.alert cpu idle at 25% on hostname` for any host who's Idle CPU usage has averaged less than 40% over the last 1 minute. This example is a "host scoped" alert, but Bosun also supports cluster, datacenter, or globally scoped alerts (see the [fundamentals video series](https://www.youtube.com/playlist?list=PLWetmRzVkFTdnjRmE-a-JRx2m8qgB6iu9) for more details).  

  [1]: http://bosun.org/configuration

