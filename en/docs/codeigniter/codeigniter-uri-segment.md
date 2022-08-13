---
title: "CodeIgniter URI Segment"
slug: "codeigniter-uri-segment"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## URI Segments: 
For example, please consider the following URI:

    http://stackoverflow.com/questions/some-number/how-can-i-do-this/others

Segment allows to retrieve a specific segment form URI string where n is a segment number. Segments are numbered from left to right. For example, the following code:

    $this->uri->segment(n)

Is used to retrieve a specific segment from the URI where n is the segment number.

    echo $this->uri->segment(0);//it will print stackoverflow.com
    echo $this->uri->segment(1);//it will print questions
    echo $this->uri->segment(2);//it will print some-number
    echo $this->uri->segment(3);//it will print how-can-i-do-this
    echo $this->uri->segment(4);//it will print others



## Get last and before last URI segment
*Get last segment*

    echo end($this->uri->segment_array()); //it will print others

*Get before last segment*

    echo $this->uri->segment(count($this->uri->segment_array())-1); //it will print how-can-i-do-this


More info: [http://stackoverflow.com/questions/9221164/code-igniter-get-before-last-uri-segment][1]


  [1]: http://stackoverflow.com/questions/9221164/code-igniter-get-before-last-uri-segment

