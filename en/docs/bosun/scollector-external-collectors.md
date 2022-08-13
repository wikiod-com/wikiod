---
title: "Scollector External Collectors"
slug: "scollector-external-collectors"
draft: false
images: []
weight: 9910
type: docs
toc: true
---

Scollector supports [tcollector](http://opentsdb.net/docs/build/html/user_guide/utilities/tcollector.html#collecting-lots-of-metrics-with-tcollector) style [external collectors](http://bosun.org/scollector/external-collectors) that can be used to send metrics to Bosun via custom scripts or executables. External collectors are a great way to get started collecting data, but when possible it is recommended for applications to send data directly to Bosun or to update scollector so that it natively supports additional systems.

The [ColDir configuration key][1] specifies the external collector directory, which is usually set to something like **/opt/scollector/collectors/** in Linux or **C:\\Program Files\\scollector\\collectors\\** in Windows. It should contain numbered directories just like the ones used in OpenTSDB tcollector. Each directory represents how often scollector will try to invoke the collectors in that folder (example: 60 = every 60 seconds). Use a directory named 0 for any executables or scripts that will run continuously and create output on their own schedule. Any non-numeric named directories will be ignored, and a lib and etc directory are often used for library and config data shared by all collectors.

External collectors can use either the [simple data output format](http://bosun.org/scollector/external-collectors#simple-data-output-format) from tcollector or they can send [JSON data](http://bosun.org/scollector/external-collectors#json-data-output-format) if they want to include metadata.


  [1]: https://www.wikiod.com/bosun/scollector-overview#Setup with sample scollector.toml file

## Powershell external collector script function
    <#
        .DESCRIPTION
            Writes the metric out in bosun external collector format which is compatible with scollector external scripts
        .PARAMETER metric
            Name of the metric (eg : my.metric)
        .PARAMETER type
            Type of metric (counter, gauge, etc)
        .PARAMETER unit
            Type of unit (connections, operations, etc)
        .PARAMETER desc
            Description of the metric
        .PARAMETER value
            The current value for the metric
    #>
    function Write-Metric
    {
    param(
        [string]$metric,
        [string]$type,
        [string]$unit,
        [string]$desc,
        $value
    )
    
    $epoch = New-Object DateTime (1970,1,1)
    
    
    $obj = @{
        metric = $metric
        name = "rate"
        value = $type
    }
    
    Write-Host (ConvertTo-Json $obj -Compress)
    
    $obj.name="unit"
    $obj.value=$unit
    
    Write-Host (ConvertTo-Json $obj -Compress)
    
    $obj.name="desc"
    $obj.value=$desc
    
    Write-Host (ConvertTo-Json $obj -Compress)
    
    $output = @{
        metric = $metric
        timestamp= [int]([datetime]::UtcNow.Subtract($epoch).TotalSeconds)
        value=$value
        tags= @{
            host=$env:computername.ToLower()
        }
    }
    
    Write-Host (ConvertTo-Json $output -Compress)
    
    }

## Sample collector written in PowerShell
<!-- language: lang-psh -->

    #Example of a PowerShell external collector. See http://bosun.org/scollector/external-collectors for details
    #This file should be saved in C:\Program Files\scollector\collectors\0\mymetrics.ps1 since it is a continuous output script
    #scollector.toml should have ColDir = 'C:\Program Files\scollector\collectors'
    
    #Setup format strings and other variables
    $epoch = New-Object DateTime (1970,1,1)
    $MetricMetadata='{{"metric":"{0}","name":"{1}","value":"{2}"}}'
    $MetricData='{{"metric":"{0}","timestamp":{1:F0},"value":{2:G}{3}}}'
    $MetricTags=',"tags":{{{0}}}'
    $Base="mymetric"
    
    #Send metadata for each metric once on startup (Scollector will resend to Bosun periodically)
    Write-Output ($MetricMetadata -f "$Base.test","rate","gauge") #See https://godoc.org/bosun.org/metadata#RateType
    Write-Output ($MetricMetadata -f "$Base.test","unit","item")  #See https://godoc.org/bosun.org/metadata#Unit
    Write-Output ($MetricMetadata -f "$Base.test","desc","A test metric")
    
    #Create tags and send metrics
    $tags=$MetricTags -f '"mykey":"myvalue"' #generate static tags here. Can append additional tags in the loop if needed. 
    #Use $tags="" to exclude all tags but those added by Scollector.
    Write-Output ($MetricData -f "$Base.test",[datetime]::UtcNow.Subtract($epoch).TotalSeconds,42.123,$tags)
    do {
        $delay = Get-Random -Minimum 5 -Maximum 25
        sleep -Seconds $delay
        Write-Output ($MetricData -f "$Base.test",[datetime]::UtcNow.Subtract($epoch).TotalSeconds,$delay,$tags)
    } while ($true)
    
    #If a continuous output script ever exits scollector will restart it. If you just want periodic data every 60 seconds you 
    #can use a /60/ folder instead of /0/ and allow the script to exit when finished sending a batch of metrics.

## Twitter Collector written in Go
The following can be saved as main.go. After you update the EDITME settings and build the executable it can be used as a continuous external collector. 

<!-- language: lang-golang -->

    package main
    
    import (
        "fmt"
        "log"
        "net/url"
        "strconv"
        "time"
    
        "github.com/ChimeraCoder/anaconda"
    )
    
    func main() {
        anaconda.SetConsumerKey("EDITME")
        anaconda.SetConsumerSecret("EDITME")
        api := anaconda.NewTwitterApi("EDITME", "EDITME")
        v := url.Values{}
        sr, err := api.GetSearch("stackoverflow", nil)
        if err != nil {
            log.Println(err)
        }
        var since_id int64 = 0
        for _, tweet := range sr {
            if tweet.Id > since_id {
                since_id = tweet.Id
            }
        }
        count := 0
        for {
            now := time.Now().Unix()
            v.Set("result_type", "recent")
            v.Set("since_id", strconv.FormatInt(since_id, 10))
            sr, err := api.GetSearch("stackoverflow", nil)
            if err != nil {
                log.Println(err)
            }
            for _, tweet := range sr {
                if tweet.Id > since_id {
                    count += 1
                    since_id = tweet.Id
                }
            }
            fmt.Println("twitter.tweet_count", now, count, "query=stackoverflow")
            time.Sleep(time.Second * 30)
        }
    }

## Hadoop HDFS disk usage written in Bash
This is a continuous collector that uses the `hadoop fs -du -s  /hbase/*` command to get details about the HDFS disk usage. This metric is very useful for tracking space in an OpenTSDB system.

<!-- language: lang-js -->

    #!/bin/bash
    while true; do
        while read -r bytes raw_bytes path; do
            echo "hdfs.du $(date +"%s") $bytes path=$path"
            #https://community.cloudera.com/t5/Storage-Random-Access-HDFS/hdfs-du-format-change/td-p/27192 KMB 2015-08-24T12:01:20Z
            echo "hdfs.du.raw $(date +"%s") $raw_bytes path=$path"
        done < <(hadoop fs -du -s  /hbase/*)
        sleep 30
    done

## StackExchange.Exceptional collector written in Go with Metadata
The following Go file can be compiled into a continuous external collector that will query a MSSQL server database that uses the [StackExchange.Exceptional][1] schema. It will query multiple servers/databases for all exceptions since UTC 00:00 to convert the raw entries into a counter. It also uses the bosun.org/metadata package to include metadata for the **exceptional.exceptions.count** metric.

<!-- language: lang-golang -->

    /*
    Exceptional is an scollector external collector for StackExchange.Exceptional.
    */
    package main
    
    import (
        "database/sql"
        "encoding/json"
        "fmt"
        "log"
        "strings"
        "time"
    
        "bosun.org/metadata"
        "bosun.org/opentsdb"
    
        _ "github.com/denisenkom/go-mssqldb"
    )
    
    func mssqlConnect(server, database, user, pass, port string) (*sql.DB, error) {
        dsn := fmt.Sprintf("server=%s;port=%s;database=%s;user id=%s;password=%s", server, port, database, user, pass)
        return sql.Open("mssql", dsn)
    }
    
    type Exceptions struct {
        GUID            string
        ApplicationName string
        MachineName     string
        CreationDate    time.Time
        Type            string
        IsProtected     int
        Host            string
        Url             string
        HTTPMethod      string
        IPAddress       string
        Source          string
        Message         string
        Detail          string
        StatusCode      int
        SQL             string
        DeletionDate    time.Time
        FullJson        string
        ErrorHash       int
        DuplicateCount  int
    }
    
    type ExceptionsCount struct {
        ApplicationName string
        MachineName     string
        Count           int64
        Source          string
    }
    
    type ExceptionsDB struct {
        Server     string
        DBName     string
        DBPassword string
        DBPort     string
        Source     string
    }
    
    const (
        defaultPassword = "EnterPasswordHere"
        defaultPort     = "1433"
    
        metric     = "exceptional.exceptions.count"
        descMetric = "The number of exceptions thrown per second by applications and machines. Data is queried from multiple sources. See status instances for details on exceptions."
    )
    
    func main() {
        mds := []metadata.Metasend{
            {
                Metric: metric,
                Name:   "rate",
                Value:  "counter",
            },
            {
                Metric: metric,
                Name:   "unit",
                Value:  metadata.Error,
            },
            {
                Metric: metric,
                Name:   "desc",
                Value:  descMetric,
            },
        }
        for _, m := range mds {
            b, err := json.Marshal(m)
            if err != nil {
                log.Fatal(err)
            }
            fmt.Println(string(b))
        }
        instances := [...]ExceptionsDB{
            {"NY_AG", "NY.Exceptions", defaultPassword, defaultPort, "NY_Status"},
            {"CO-SQL", "CO.Exceptions", defaultPassword, defaultPort, "CO_Status"},
            {"NY-INTSQL", "Int.Exceptions", defaultPassword, defaultPort, "INT_Status"},
        }
        for _, exdb := range instances {
            go run(exdb)
        }
        select {}
    }
    
    func run(exdb ExceptionsDB) {
        const interval = time.Second * 30
    
        query := func() {
            // Database name is the same as the username
            db, err := mssqlConnect(exdb.Server, exdb.DBName, exdb.DBName, exdb.DBPassword, exdb.DBPort)
            if err != nil {
                log.Println(err)
            }
            defer db.Close()
            var results []ExceptionsCount
            sqlQuery := `
            SELECT ApplicationName, MachineName, MAX(Count) as Count FROM
            (
                --New since UTC rollover
                SELECT ApplicationName, MachineName, Sum(DuplicateCount) as Count from Exceptions
                WHERE CreationDate > CONVERT (date, GETUTCDATE())
                GROUP BY MachineName, ApplicationName
                UNION --Zero out any app/machine combos that had exceptions in last 24 hours
                SELECT DISTINCT ex.ApplicationName, ex.MachineName, 0 as Count from Exceptions ex WHERE ex.CreationDate Between Convert(Date, GETUTCDATE()-1) And Convert(Date, GETUTCDATE())
            ) as T
            GROUP By T.MachineName, T.ApplicationName`
            rows, err := db.Query(sqlQuery)
            if err != nil {
                log.Println(err)
                return
            }
            defer rows.Close()
            for rows.Next() {
                var r ExceptionsCount
                if err := rows.Scan(&r.ApplicationName, &r.MachineName, &r.Count); err != nil {
                    log.Println(err)
                    continue
                }
                r.Source = exdb.Source
                results = append(results, r)
            }
            if err := rows.Err(); err != nil {
                log.Println(err)
            }
            if len(results) > 0 {
                now := time.Now().Unix()
                for _, r := range results {
                    application, err := opentsdb.Clean(r.ApplicationName)
                    if err != nil {
                        log.Println(err)
                        continue
                    }
                    db := opentsdb.DataPoint{
                        Metric:    metric,
                        Timestamp: now,
                        Value:     r.Count,
                        Tags: opentsdb.TagSet{
                            "application": application,
                            "machine":     strings.ToLower(r.MachineName),
                            "source":      r.Source,
                        },
                    }
                    b, err := db.MarshalJSON()
                    if err != nil {
                        log.Println(err)
                        continue
                    }
                    fmt.Println(string(b))
                }
            }
        }
    
        for {
            wait := time.After(interval)
            query()
            <-wait
        }
    }


  [1]: https://github.com/NickCraver/StackExchange.Exceptional

