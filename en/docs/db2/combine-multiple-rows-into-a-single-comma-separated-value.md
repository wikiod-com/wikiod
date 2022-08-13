---
title: "Combine multiple rows into a single comma separated value"
slug: "combine-multiple-rows-into-a-single-comma-separated-value"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Using the listagg() function
Let's say you've got a table of loans, and another related table of parcels, where each loan can have one or more parcels associated with it.  If you want a query to show each loan and a list of all its associated parcels, but you only want each loan to show up once, then you could use something like this:

    select 
      loan.loannumber, 
      parcel_agg.p_list as parcel_list 
    from 
      schema.loan loan 
      left join 
      ( select loannumber, listagg(parcelnum, ', ') from schema.parcel parcel group by loannumber ) parcel_agg on parcel_agg.loannumber = loan.loannumber


