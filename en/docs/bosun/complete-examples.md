---
title: "Complete Examples"
slug: "complete-examples"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

## Linux Bonding Health
# Template Definition

    template linux.bonding {
        subject = {{.Last.Status}}: {{.Eval .Alert.Vars.by_host}} bad bond(s) on {{.Group.host}}
        body = `{{template "header" .}}
        <h2>Bond Status</h2>
        <table>
        <tr><th>Bond</th><th>Slave</th><th>Status</th></tr>
        {{range $r := .EvalAll .Alert.Vars.slave_status}}
            {{if eq $.Group.host .Group.host}}
                <tr>
                    <td>{{$r.Group.bond}}</td>
                    <td>{{$r.Group.slave}}</td>
                    <td {{if lt $r.Value 1.0}} style="color: red;" {{end}}>{{$r.Value}}</td>
                </tr>
            {{end}}
        {{end}}
        </table>
        `
    }

# Alert Definition

    alert linux.bonding {
        template = linux.bonding
        macro = host_based
        $notes = This alert triggers when a bond only has a single interface, or the status of a slave in the bond is not up
        $slave_status = max(q("sum:linux.net.bond.slave.is_up{bond=*,host=*,slave=*}", "5m", ""))
        $slave_status_by_bond = sum(t($slave_status, "host,bond"))
        $slave_count = max(q("sum:linux.net.bond.slave.count{bond=*,host=*}", "5m", ""))
        $no_good = $slave_status_by_bond < $slave_count || $slave_count < 2
        $by_host = max(t($no_good, "host"))
        warn = $by_host
    } 

  

# Notification Priview
[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/8kolx.jpg

## SSL Certs Expiring
This data is collected by the http_unit and scollector. It warns when an alert is going to expire within a certain amount of days, and then goes critical if the cert has passed the expiration date. This follows the recommended default of warn and crit usage in Bosun (warn: something is going to fail, crit: something has failed).

# Template Def

    template ssl.cert.expiring {
        subject = {{.Last.Status}}: SSL Cert Expiring in {{.Eval .Alert.Vars.daysLeft | printf "%.2f"}} Days for {{.Group.url_host}}
        body = `
        {{ template "header" . }}
        <table>
           <tr>
                <td>Url</td>
                <td>{{.Group.url_host}}</td>
           </tr>
           <tr>
                <td>IP Address Used for Test</td>
                <td>{{.Group.ip}}</td>
           </tr>
           <tr>
                <td>Days Remaining</td>
                <td>{{.Eval .Alert.Vars.daysLeft | printf "%.2f"}}</td>
           </tr>
           <tr>
                <td>Expiration Date</td>
                <td>{{.Last.Time.Add (parseDuration (.Eval .Alert.Vars.hoursLeft | printf "%vh")) }}</td>
           </tr>
        </table>
        `
    }

# Alert Definition

    alert ssl.cert.expiring {
        template = ssl.cert.expiring
        ignoreUnknown = true
        $notes = This alert exists to notify of us any SSL certs that will be expiring for hosts monitored by our http unit test cases defined in the scollector configuration file.
        $expireEpoch = last(q("min:hu.cert.expires{host=ny-bosun01,url_host=*,ip=*}", "1h", ""))
        $hoursLeft = ($expireEpoch - epoch()) / d("1h")
        $daysLeft = $hoursLeft / 24
        warn = $daysLeft <= 50
        crit = $daysLeft <= 0
        warnNotification = default
        critNotification = default
    }

## Alert Explanation
 

 - `q(..)` ([func doc][1]) querties OpenTSDB, one of Bosun's supported backends. In returns a type called a seriesSet (which is set of time series, each identified by tag).
 - `last()` ([func doc][2]) takes the last value of each series in the seriesSet and returns a numberSet.
 - The metric, `hu.cert.expires`. is returning the Unix time stamp of when the cert will expire
 - `epoch()` ([func doc][3]) returns the current unix timestamp. So subtracting current unix timestamp from the expiration epoch gives is the remaining time.
 - `d()` ([func doc][4]) returns the number of seconds represented by the duration string, the duration string uses the [same units][5] as OpenTSDB.

# Notification Preview

[![enter image description here][6]][6]

# Example Section of `scollector.toml` referencing the config for [httpunit][7] test cases:

    [[HTTPUnit]]
      TOML = "/opt/httpunit/data/httpunit.toml"


  [1]: http://bosun.org/expressions.html#qquery-string-startduration-string-endduration-string-seriesset
  [2]: http://bosun.org/expressions.html#lastseriesset-numberset
  [3]: http://bosun.org/expressions.html#epoch-scalar
  [4]: http://bosun.org/expressions.html#dstring-scalar
  [5]: http://opentsdb.net/docs/build/html/user_guide/query/dates.html
  [6]: http://i.stack.imgur.com/xivKg.jpg
  [7]: https://github.com/StackExchange/httpunit

## Header Template
In Bosun templates can reference other templates. For emails notifications, you might have a header template to show things you want in all alerts.

# Header Template

    template header {
        body = `
        <style>
        td, th {
            padding-right: 10px;
        }
        </style>
        <p style="font-weight: bold; text-decoration: underline;">
            <a style="padding-right: 10px;" href="{{.Ack}}">Acknowledge</a>
            <a style="padding-right: 10px;" href="{{.Rule}}">View Alert in Bosun's Rule Editor</a>
            {{if .Group.host}}
                <a style="padding-right: 10px;" href="https://status.stackexchange.com/dashboard/node?node={{.Group.host}}">View {{.Group.host}} in Opserver</a>
                <a href="http://kibana.ds.stackexchange.com/app/kibana?#/discover?_g=(refreshInterval:(display:Off,pause:!f,value:0),time:(from:now-15m,mode:quick,to:now))&_a=(columns:!(_source),index:%5Blogstash-%5DYYYY.MM.DD,interval:auto,query:(query_string:(analyze_wildcard:!t,query:'logsource:{{.Group.host}}')),sort:!('@timestamp',desc))">View {{.Group.host}} in Kibana</a>
            {{end}}
        </p>
        <table>
            <tr>
                <td><strong>Key: </strong></td>
                <td>{{printf "%s%s" .Alert.Name  .Group }}</td>
            </tr>
            <tr>
                <td><strong>Incident: </strong></td>
                <td><a href="{{.Incident}}">#{{.Last.IncidentId}}</a></td>
            </tr>
        </table>
        <br/>
        {{if .Alert.Vars.notes}}
            <p><strong>Notes:</strong> {{html .Alert.Vars.notes}}</p>
        {{end}}
        {{if .Alert.Vars.additionalNotes}}
            <p>
            {{if not .Alert.Vars.notes}}
                <strong>Notes:</strong>
            {{end}}
            {{ html .Alert.Vars.additionalNotes }}</p>
        {{end}}
        `
    }

Explanations:
 * `<style>...`: Although style blocks are not supported in email, bosun processes style blocks and then inlines them into the html. So this is shared css for any templates that include this template.
 * The `.Ack` link takes you to a Bosun view where you can acknowledge the alert. The `.Rule` link takes you to Bosun's rule editor setting the template, rule, and time of the alert so you can modify the alert, or run it at different times.
 * `{{if .Group.host}}...`: `.Group` is the tagset of the alert. So when the warn or crit expression has tags like host=*, we know the alert is in reference to a specific host in our environment. So we then show some links to host specific things.
 * The Alert name and key are included to ensure that at least the most basic information is in any alert
 * `.Alert.Vars.notes` this is included so if in any alert someone defines the `$notes` variables it will be show in the alert. The encourages people to write notes explaining the purpose of the alert and how to interpret it.
 * `.Alert.Vars.additionalNotes` is there in case we want to define a macro with notes, and then have instances of that macro with more notes added to the macro notes.

