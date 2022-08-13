---
title: "Basic Samtools"
slug: "basic-samtools"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Count number of records per reference in bamfile
    samtools idxstats thing.bam

## Convert sam into bam (and back again)
Samtools can be used to convert between sam and bam:

* `-b` indicates that the input file will be in BAM format
* `-S` indicates that the stdout should be in SAM format
```
samtools view -sB thing.bam > thing.sam
```
And to convert between sam and bam:

```
samtools view thing.sam > thing.bam
samtools sort thing.bam thing
samtools index thing.bam
```
This will produce a sorted, indexed bam.  This will create the files `thing.bam` and `thing.bam.bai`.  To use a bam you must have an index file.


