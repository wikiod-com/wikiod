---
title: "Templates Overview"
slug: "templates-overview"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## Syntax
- #See https://golang.org/pkg/text/template/ for Go Template **Action** and **Function** syntax
- expression = alert status {{.Last.Status}} and a variable {{.Eval .Alert.Vars.q | printf "%.2f"}}
- expression = \`Use backticks to span
- multiple lines with line breaks
- in the Bosun config file\`
- template *name* {
  - subject = *expression*
  - body = *expression*
- }

Bosun templates are based on the [Go html/template][1] package and can be shared across multiple alerts, but a single template is used to render all [Bosun Notifications][2] for that alert. Alerts reference which template to use via the `template` directive and specify which notifications to use via the `warnNotification` and `critNotification` directives (can have multiple warn/crit notifications defined for each alert).

Templates are rendered when an alert instance is triggered and can:

 - Use variables defined in the alert to display text or graphs
 - Use [Go Template Actions and Functions][3] like if, range, and, not, index, and printf
 - Access Bosun metadata to display additional details about a system
 - Access other [Bosun Template Variables and Functions][4]
 - Pull information from other systems via [HTTPGet and HTTPGetJSON][5]
 - Use Images, HTML, and CSS styles for rich notifications (CSS can be inlined for better email support)

The template subject will be displayed as headers on the dashboard, as the subject line of email notifications, and as the default contents of HTTP POST notifications. The template body will be displayed when an alert instance is expanded and as the body of email notifications.

  [1]: https://golang.org/pkg/html/template/
  [2]: https://www.wikiod.com/bosun/notifications-overview
  [3]: https://golang.org/pkg/text/template/
  [4]: https://bosun.org/configuration#template
  [5]: https://www.wikiod.com/bosun/templates-httpget-and-httpgetjson

## Low Memory Alert and Template
Templates can be previewed and edited using the Rule Editor tab in Bosun. Use the *Jump to* links to select the alert you want to edit, then you can use the *template* button next to *macro* to switch between the alert an template sections of the configuration. If an alert has multiple instances you can use `host=xxx,name=xxx` in the *Template Group* section to specify for which tagset you want to see the template rendered.

<!-- language: lang-none -->
    template os.low.memory {
        subject = {{.Last.Status}}: Low Memory: {{.Eval .Alert.Vars.q | printf "%.0f"}}% Free Memory on {{.Group.host}} ({{.Eval .Alert.Vars.free | bytes }} Free of {{.Eval .Alert.Vars.total | bytes }} Total)

        body = `
        <p><a href="{{.Ack}}">Acknowledge</a> | <a href="{{.Rule}}">View Alert in Bosun's Rule Editor</a></p>
        <p><strong>Alert Key: </strong>{{printf "%s%s" .Alert.Name  .Group }}</p>
        <p><strong>Incident: </strong><a href="{{.Incident}}">#{{.Last.IncidentId}}</a></p>
        <p><strong>Notes: </strong>{{html .Alert.Vars.notes}}</p>
    
        <strong>Graph</strong>
        <div>{{.Graph .Alert.Vars.graph .Alert.Vars.graph_unit}}</div>
        `
    }
    
    notification sample.notification {
        email = alerts@example.com
    }
    
    alert os.low.memory {
        template = os.low.memory
        $notes = Alerts when less than 5% free, or less than 500MB (when total > 2GB). In Linux, Buffers and Cache are considered "Free Memory".
    
        $default_time = "2m"
        $host = wildcard(*)
        $graph = q("avg:300s-avg:os.mem.percent_free{host=$host}", "1d", "")
        $graph_unit = Percent Free Memory (Including Buffers and Cache)
        $q = avg(q("avg:os.mem.percent_free{host=$host}", $default_time, ""))
        $total = last(q("sum:os.mem.total{host=$host}", $default_time, ""))
        $free = last(q("sum:os.mem.free{host=$host}", $default_time, ""))

        #Warn when less than 5% free or total > 2GB and free < 500MB
        warn = $q < 5 || ($total > 2147483648 && $free < 524288000)
        #Crit when less than 0.5% free
        crit = $q <= .5
        critNotification = sample.notification
    }

After you test the alert on the Rule Editor page you can use the *Results* tab to see computations, *Template* to see the rendered alert notification, and *Timeline* to see all alert incidents (only when *From* and *To* dates are specified).

[![Bosun Rule Editor Template Preview][1]][1]


  [1]: http://i.stack.imgur.com/eVCJS.png

## Embedded Templates and CSS Styles
You can embed another template body into your template via `{{template "mysharedtemplate" .}}` to reuse shared components. Here is an example that creates a header template that can be reused at the top of all other template bodies. It also uses CSS to stylize the output so that it is easier to read. Note that any `<style>...</style>` blocks will be converted to inline CSS on each element so that email clients like Gmail will render the output correctly.

<!-- language: lang-none -->
    template header {
        body = `
        <style>
        td, th {
            padding-right: 10px;
        }
        a.rightpad {
            padding-right: 10px;
        }
        </style>
        <p style="font-weight: bold; text-decoration: underline;">
            <a class="rightpad" href="{{.Ack}}">Acknowledge</a>
            <a class="rightpad" href="{{.Rule}}">View Alert in Bosun's Rule Editor</a>
            {{if .Group.host}}
                <a class="rightpad" href="https://opserver/dashboard/node?node={{.Group.host}}">View {{.Group.host}} in Opserver</a>
                <a href="http://kibana/app/kibana?#/discover?_g=(refreshInterval:(display:Off,pause:!f,value:0),time:(from:now-15m,mode:quick,to:now))&_a=(columns:!(_source),index:%5Blogstash-%5DYYYY.MM.DD,interval:auto,query:(query_string:(analyze_wildcard:!t,query:'logsource:{{.Group.host}}')),sort:!('@timestamp',desc))">View {{.Group.host}} in Kibana</a>
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

        <p><strong>Tags</strong>
        <table>
            {{range $k, $v := .Group}}
                {{if eq $k "host"}}
                    <tr><td>{{$k}}</td><td><a href="{{$.HostView $v}}">{{$v}}</a></td></tr>
                {{else}}
                    <tr><td>{{$k}}</td><td>{{$v}}</td></tr>
                {{end}}
            {{end}}
        </table></p>
        `
    }

After which you can add start your templates with ``body = `{{template "header" .}}`` to get the following output at the top:

[![Bosun Header Template Preview][1]][1]


  [1]: http://i.stack.imgur.com/SbHXc.png

## Generic Template with optional Graphs
It often is faster to use a generic template when first creating a new alert and only specialize the template when you need to display more information. The following template will display a subject with a numerical value, custom formatting, and description string and then a body with up to two graphs. If no graph variables are specified it will instead list the computations used in the alert. The generic template also uses the name of the alert to generate the subject (replacing dots with spaces) and checks for variables to exist before using them to prevent errors.

<!-- language: lang-none -->
    #See Embedded Templates and CSS Styles example for header template
    template header { ... } 
    
    template computation {
        body = `
        <p><strong>Computation</strong>
        <table>
            {{range .Computations}}
                <tr><td><a href="{{$.Expr .Text}}">{{.Text}}</a></td><td>{{.Value}}</td></tr>
            {{end}}
        </table></p>`
    }
    
    template generic_template {
        subject = {{.Last.Status}}: {{replace .Alert.Name "." " " -1}}: {{if .Alert.Vars.value}}{{if .Alert.Vars.value_format}}{{.Eval .Alert.Vars.value | printf .Alert.Vars.value_format}}{{else}}{{.Eval .Alert.Vars.value | printf "%.1f"}}{{end}}{{end}}{{if .Alert.Vars.value_string}}{{.Alert.Vars.value_string}}{{end}}{{if .Group.host}} on {{.Group.host}}{{end}}
        
        body = `{{template "header" .}}
    
        {{if or .Alert.Vars.generic_graph .Alert.Vars.generic_graph_all}}
            <strong>Graph</strong>
            {{if and .Alert.Vars.graph_unit .Alert.Vars.generic_graph}}
                <div>{{.Graph .Alert.Vars.generic_graph .Alert.Vars.graph_unit}}</div>
            {{else if .Alert.Vars.generic_graph}}
                <div>{{.Graph .Alert.Vars.generic_graph}}</div>
            {{end}}
            {{if and .Alert.Vars.graph_unit2 .Alert.Vars.generic_graph2}}
                <div>{{.Graph .Alert.Vars.generic_graph2 .Alert.Vars.graph_unit2}}</div>
            {{else if .Alert.Vars.generic_graph2}}
                <div>{{.Graph .Alert.Vars.generic_graph2}}</div>
            {{end}}
            {{if and .Alert.Vars.generic_graph_all .Alert.Vars.graph_unit}}
                <div>{{.GraphAll .Alert.Vars.generic_graph_all .Alert.Vars.graph_unit}}</div>
            {{else if .Alert.Vars.generic_graph_all}}
                <div>{{.GraphAll .Alert.Vars.generic_graph_all}}</div>
            {{end}}
            {{if and .Alert.Vars.generic_graph_all2 .Alert.Vars.graph_unit2}}
                <div>{{.GraphAll .Alert.Vars.generic_graph_all2 .Alert.Vars.graph_unit2}}</div>
            {{else if .Alert.Vars.generic_graph_all2}}
                <div>{{.GraphAll .Alert.Vars.generic_graph_all2}}</div>
            {{end}}
        {{else}}
            {{template "computation" .}}
        {{end}}`
    }
    
    
    alert puppet.last.run {
        template = generic_template
        $timethreshold = 60
        $timegraph = 24h
        $notes = Checks if puppet has not run in at least ${timethreshold} minutes. Doesn't include hosts which have puppet disabled.
        
        $generic_graph = q("sum:300s-max:puppet.last_run{host=*}", "$timegraph", "") / 60
        $graph_unit = Minutes since Last Puppet Run
        $generic_graph2 = q("sum:300s-max:puppet.disabled{host=*}", "$timegraph", "")
        $graph_unit2 = Puppet Disabled=1 Enabled=0
        
        $value = last(q("sum:puppet.last_run{host=*}", "6h", "")) / 60
        $value_format = It has been %.0f
        $value_string = ` minutes since last run`
        $disabled = max(q("sum:puppet.disabled{host=*}", "60m", ""))
        warn = ($value > $timethreshold) && ! $disabled
        warnNotification = default
        runEvery = 15
    }

Which will produce a subject like "warning: puppet last run: It has been 62 minutes since last run on co-lb04" and include a graphs of last_run and disabled for that host. If you want to graph all results for a query instead of just the matching tagsets you can use **$generic_graph_all** and **$generic_graph_all2** as the variable names.

