---
title: "Templates Graph and GraphAll"
slug: "templates-graph-and-graphall"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

[Bosun Templates][1] can include graphs to provide more information when sending a notification. The graphs can use variables from the alert and filter base on the tagset for the alert instance or use the GraphAll function to graph all series. When viewed on the Dashboard or in an email you can click on the graph to load it in the Expression page. 

You can also create a [Generic Template with optional Graphs][2] that can be shared across multiple alerts.


  [1]: https://bosun.org/configuration#template
  [2]: https://www.wikiod.com/bosun/templates-overview#Generic Template with optional Graphs

## Graph using Alert Variable
Using .Graph will filter the results to only include those that match the tagset for the alert. For instance an alert for os.low.memory{host=ny-web01} would only include series with the host=ny-web01 tags. If multiple series match then only the first matching result will be used.

<!-- language: lang-none -->
    template graph.template {
        subject = ...

        body = `{{template "header" .}}
    
        <strong>Graph</strong>
        <div>{{.Graph .Alert.Vars.graph}}</div>

        <strong>Graph With Y Axis Label Literal</strong>
        <div>{{.Graph .Alert.Vars.graph "Free Memory in GB"}}</div>

        <strong>Graph With Y Axis Label From Variable</strong>
        <div>{{.Graph .Alert.Vars.graph .Alert.Vars.graph_unit}}</div>

        `
    }

    alert os.low.memory {
        template = graph.template
        ...
        $graph = q("avg:300s-avg:os.mem.percent_free{host=$host}", "1d", "")
        $graph_unit = Percent Free Memory (Including Buffers and Cache)
        ...
    }

## GraphAll using Alert Variable
Using .GraphAll will include all the results in the graph.

<!-- language: lang-none -->
    template graph.template {
        subject = ...

        body = `{{template "header" .}}
    
        <strong>GraphAll</strong>
        <div>{{.GraphAll .Alert.Vars.graph}}</div>

        <strong>GraphAll With Y Axis Label Literal</strong>
        <div>{{.GraphAll .Alert.Vars.graph "All Systems Free Memory in GB"}}</div>

        <strong>GraphAll With Y Axis Label From Variable</strong>
        <div>{{.GraphAll .Alert.Vars.graph .Alert.Vars.graph_unit}}</div>

        `
    }

    alert os.low.memory {
        template = graph.template
        ...
        $graph = q("avg:300s-avg:os.mem.percent_free{host=$host}", "1d", "")
        $graph_unit = All Systems Percent Free Memory (Including Buffers and Cache)
        ...
    }

## Graph or GraphAll using inline or dynamic query
Graph queries can be defined inline if you don't want to use an Alert variable.

<!-- language: lang-none -->
    template graph.template {
        subject = ...

        body = `{{template "header" .}}
    
        <strong>Graph With Inline Query</strong>
        <div>{{.Graph "q(\"avg:300s-avg:os.mem.percent_free{host=specifichost}\", \"1d\", \"\")" "Free Memory in GB"}}</div>

        <strong>GraphAll with Inline Query</strong>
        <div>{{.GraphAll "q(\"avg:300s-avg:os.mem.percent_free{host=host1|host2|host3}\", \"1d\", \"\")" "All Systems Free Memory in GB"}}</div>

        `
    }

Sometimes you may want to create the query for a graph dynamically in the template itself by combining one or more variables. For instance a host down alert might want to include the Bosun known hosts ping metric using the dst_host tag.

<!-- language: lang-none -->
    template host.down {
        subject = ...

        body = `{{template "header" .}}
    
        <strong>Graph from one variable</strong>
        <div>{{printf "q(\"sum:bosun.ping.timeout{dst_host=%s}\", \"8h\", \"\")" (.Group.host) | .Graph}}</div>

        <strong>Graph from multiple variables</strong>
        <div>{{printf "q(\"sum:%s{host=%s,anothertag=%s}\", \"8h\", \"\")" "some.metric.name" .Group.host "anothervalue" | .Graph}}</div>
        `
    }

The printf statement will generate `q("sum:bosun.ping.timeout{dst_host=alerthostname}", "8h", "")` when that host triggers an alert and then use that to create the graph in the notification.

## Filter, Sort, Limit and Graph
When using GraphAll you may still want to filter the results, in which case you can use an Alert variable with the [Filter][1], [Sort][2], and [Limit][3] functions.

<!-- language: lang-none -->
    template graph.template {
        subject = ...

        body = `{{template "header" .}}
    
        <strong>Graph Filtered Variable</strong>
        <div>{{.Graph .Alert.Vars.graph_below_5 .Alert.Vars.graph_unit}}</div>

        <strong>Graph Filter+Sort+Limit Variable (Maximum of 10 series)</strong>
        <div>{{.Graph .Alert.Vars.graph_lowest_10 .Alert.Vars.graph_unit2}}</div>

        `
    }

    alert os.low.memory {
        template = graph.template
        ...
        $graph_all = q("avg:300s-avg:os.mem.percent_free{host=ny-*}", "1d", "")
        $graph_unit = All Systems with Less than 5 Percent Free Memory
        $graph_below_5 = filter($graph_all, min($graph_all) < 5)

        $graph_unit2 = Ten Systems with lowest Percent Free Memory
        $graph_lowest_10 = filter($graph_all, limit(sort(min($graph_min_5),"asc"),10))        
        ...
    }


  [1]: https://bosun.org/expressions#filterseriesset-numberset-seriesset
  [2]: https://bosun.org/expressions#sortnumberset-ascdesc-string-numberset
  [3]: https://bosun.org/expressions#limitnumberset-count-scalar-numberset

## Using Merge to Combine Series
If you want to graph two series on one graph, you can use the [Merge][1] function. This can also be combined with the [Series][2] function to manipulate the Y axis (like forcing it to start at zero).

<!-- language: lang-none -->
    template graph.template {
        subject = ...

        body = `{{template "header" .}}
    
        <strong>Graph With Merge+Series so Y Axis Starts At Zero</strong>
        <div>{{.Graph .Alert.Vars.graph_merged .Alert.Vars.graph_unit}}</div>
        `
    }

    alert os.low.memory {
        template = graph.template
        ...
        $graph_time = "1d"
        $graph_host = q("avg:300s-avg:os.mem.percent_free{host=myhost}", $graph_time, "")
        $graph_unit = Notice the Y axis always starts at zero now
        $graph_series = series("value=zero", epoch()-d($graph_time), 0, epoch(),0)
        $graph_merged = merge($graph_host,$graph_series)    
        ...
    }


  [1]: https://bosun.org/expressions#mergeseriesset-seriesset
  [2]: https://bosun.org/expressions#seriestagset-string-epoch-value--seriesset

