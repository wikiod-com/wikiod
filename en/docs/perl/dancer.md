---
title: "Dancer"
slug: "dancer"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

About:

Dancer2 (the successor of Dancer) is a simple but powerful web application framework for Perl.

It is inspired by Sinatra and written by Alexis Sukrieh.

Key features: ••• Dead Simple - Intuitive, minimalist and very expressive syntax. ••• Flexible - PSGI support, plugins and modular design allow for strong scalability. ••• Few dependencies - Dancer depends on as few CPAN modules as possible making it easy to install.


## Easiest example
    #!/usr/bin/env perl
    use Dancer2;

    get '/' => sub {
        "Hello World!"
    };

    dance;
    

