---
title: "Dates and Time"
slug: "dates-and-time"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Date formatting
Time::Piece is available in perl 5 after version 10

    use Time::Piece;
    
    my $date = localtime->strftime('%m/%d/%Y');
    print $date;

>     Output
>     07/26/2016

