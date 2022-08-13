---
title: "lsstat"
slug: "lsstat"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Parameters
 | Parameter | Details |
 | --------- | ------- |
 | indexRoot | The root name of the index to hit, the format is expected to be `fmt.Sprintf("%s-%s", index_root, d.Format("2006.01.02"))`
 | keyString | Creates groups (like tagsets) and can also filter those groups. It is the format of `"field:regex,field:regex..."`. The `:regex` can be ommited.
 | filterString | An Elastic regexp query that can be applied to any field. It is in the same format as the keystring argument.
 | field | The field in ElasticSearch to perform the operation on. Must be a numeric field.
 | rStat | Can be one of `avg`, `min`, `max`, `sum`, `sum_of_squares`, `variance`, `std_deviation`
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

## The average value of a field over time
`lsstat` returns various summary stats per bucket for the specified field. The field *must* be **numeric** in elastic.

`rStat` can be one of `avg`, `min`, `max`, `sum`, `sum_of_squares`, `variance`, `std_deviation`.

The rest of the fields behave the same as `lscount`, except that there is no division based on bucketDuration (since these are summary stats)

    $max_querytime_by_minute = lsstat("logstash", "", "env:prod", "querytime", "max", "1m", "1h", "")

The `lsstat` in this queries the `logstash` indexes, filters on a field `env` with the value `prod`, and gives the `max` value of `querytime` for the last hour, in one minute buckets.

