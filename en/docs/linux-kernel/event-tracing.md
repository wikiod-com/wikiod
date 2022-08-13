---
title: "Event Tracing"
slug: "event-tracing"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

## Tracing I2C Events
Note: I am assuming that `debugfs` is mounted under `/sys/kernel/debug`

If not, try:

    mount -t debugfs none /sys/kernel/debug

Change into the tracing directory:

    cd /sys/kernel/debug/tracing/

Make sure the function tracer is disabled:

    echo nop > current_tracer

Enable all I2C events:

    echo 1 > events/i2c/enable

Make sure tracing is enabled:

    echo 1 > tracing_on

The trace messages can be viewed in `/sys/kernel/debug/tracing/trace`, example:

    ... i2c_write: i2c-5 #0 a=044 f=0000 l=2 [02-14]
    ... i2c_read: i2c-5 #1 a=044 f=0001 l=4
    ... i2c_reply: i2c-5 #1 a=044 f=0001 l=4 [33-00-00-00]
    ... i2c_result: i2c-5 n=2 ret=2

The trace events user-space API documentation can be found in the file `Documentation/trace/events.txt` of the kernel source.

