---
title: "lscount"
slug: "lscount"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Parameters
| Parameter | Details |  
| --------- | ------- |
| indexRoot | The root name of the index to hit, the format is expected to be `fmt.Sprintf("%s-%s", index_root, d.Format("2006.01.02"))`
| keyString | Creates groups (like tagsets) and can also filter those groups. It is the format of `"field:regex,field:regex..."`. The `:regex` can be ommited.
| filterString | An Elastic regexp query that can be applied to any field. It is in the same format as the keystring argument.
| bucketDuration | The same format is an opentsdb duration, and is the size of buckets returned (i.e. counts for every 10 minutes)
| startDuration |set the time window from now - see the OpenTSDB `q()` function for more details.
| endDuration | set the time window from now - see the OpenTSDB `q()` function for more details.

# Deprecation

**The LogStash query functions are deprecated**, and only for use with v1.x of ElasticSearch. If you are running v2 or above of ElasticSearch, then you should refer to the Elastic Query functions.

# Caveats

- There is currently no escaping in the keystring, so if you regex needs to have a comma or double quote you are out of luck.
- The regexs in keystring are applied twice. First as a regexp filter to elastic, and then as a go regexp to the keys of the result. This is because the value could be an array and you will get groups that should be filtered. This means regex language is the intersection of the golang regex spec and the elastic regex spec.
Elastic uses lucene style regex. This means regexes are always anchored (see the documentation).
- If the type of the field value in Elastic (aka the mapping) is a number then the regexes won’t act as a regex. The only thing you can do is an exact match on the number, ie “eventlogid:1234”. It is recommended that anything that is a identifier should be stored as a string since they are not numbers even if they are made up entirely of numerals.
- Alerts using this information likely want to set ignoreUnknown, since only “groups” that appear in the time frame are in the results    

## Counting total number of documents in last 5 minutes
`lscount` returns a time bucketed count of matching documents in the LogStash index, according to the specified filter.

A trivial use of this would be to check how many documents in total have been received in the 5 minutes, and alert if it is below a certain threshold.

A Bosun alert for this might look like:

    alert logstash.docs {
        $notes = This alerts if there hasn't been any logstash documents in the past 5 minutes
        template = logstash.docs
        $count_by_minute = lscount("logstash", "", "", "5m", "5m", "")
        $count_graph = lscount("logstash", "", "", "1m", "60m", "")
        $q = avg($count_by_minute)
        crit = $q < 1
        critNotification = default
    }

    template logstash.docs {
        body = `{{template "header" .}}
        {{.Graph .Alert.Vars.count_graph }}
        {{template "def" .}}
        {{template "computation" .}}`
        subject = {{.Last.Status}}: Logstash docs per second: {{.Eval .Alert.Vars.q | printf "%.2f"}} in the past 5 minutes
    }

This has two instances of lscount:

- $count_by_minute = lscount("logstash", "", "", "5m", "5m", "")
     - This counts the number of documents from the last 5 minutes, in a single 5 minute bucket. You will get one data point in the returned seriesSet with the total number of documents from the last 5 minutes, in the latest `logstash` index
- $count_graph = lscount("logstash", "", "", "1m", "60m", "")
     - This counts the number of documents from the last hour, in 1 minute buckets. There will be a total of 60 data points in the seriesSet returned, which in this instance is used in a graph.

