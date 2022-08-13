---
title : dns Tutorial
slug : dns-tutorial
weight : 9986
draft : false
images : []
type : docs
---

The Domain Name System (DNS) is a _hierarchical_, _distributed_ global database, most commonly used for mapping hostnames to their respective IP addresses.  It is defined in [RFC 1034][1] and [RFC 1035][2], and numerous updates thereof.

A domain name is a sequence of _labels_ separated by the period character (`.`).  Each label can have a maximum of 63 characters, and a domain name can have a maximum of 255 characters.

The DNS is often described as a tree structure, with the "root zone" at the top, the Top Level Domains (TLDs) e.g. `com`, `uk`, etc below that, etc.  Within a domain name the labels are shown in a "little endian" order with the leaf node label appearing left-most, and the TLD appearing right-most.  If a trailing period appears after the TLD then the name is said to be a _Fully Qualified Domain Name_, where the trailing period represents the root zone itself.  

  [1]: https://tools.ietf.org/html/rfc1034
  [2]: https://tools.ietf.org/html/rfc1035

