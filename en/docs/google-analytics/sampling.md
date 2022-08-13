---
title: "Sampling"
slug: "sampling"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Parameters
|Parameter|Details|
|---------|-------|
|sampleRate|Float number describing the amount of users in percent to be tracked. Default = 100.


> Sampling in Analytics is the practice of selecting a subset of data
> from your traffic and reporting on the trends available in that sample
> set. Sampling is widely used in statistical analysis because analyzing
> a subset of data gives similar results to analyzing all of the data.
> In addition, sampling speeds up processing for reports when the volume
> of data is so large as to slow down report queries.

In layman's terms this means that when there is a large amount of data we can take a chunk of that data and analyze based upon that sample.   When looking at large data sets it can often be faster to analyse upon a sample rather then the full data set.  However one must always take into account that the results will not be 100% the same as if you had done the analysis upon the full data set.

Google Analytics handle's sampling is as follows:

> Analytics inspects the number of sessions for the specified date range
> at the property level. If the number of sessions in the property over
> the given date range exceeds 500k sessions (100M for Analytics 360)1,
> Analytics will employ a sampling algorithm which uses a sample set
> proportional to the distribution of sessions by day for the selected
> date range. Thus, the session sampling rate varies for every query
> depending on the number of sessions included in the selected date
> range for the given property.

Additional information can be found here:  [How sampling works][1]


  [1]: https://support.google.com/analytics/answer/2637192?hl=en

## Sample Rate
    ga('create', 'UA-XXXX-Y', {'sampleRate': 5});

Optional. This may only be set in the create method.

Specifies what percentage of users should be tracked. This defaults to 100 (no users are sampled out) but large sites may need to use a lower sample rate to stay within Google Analytics processing limits.

