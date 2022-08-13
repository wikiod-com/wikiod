---
title: "Understanding Transaction Searches"
slug: "understanding-transaction-searches"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

A deep understanding of how Transaction searches function is crucial knowledge for every NetSuite developer, but the default behaviour of these searches, and controlling that behaviour, can be quite confusing initially.

References:

* NetSuite Help page: "Using Main Line in Transaction Search Criteria"

## Filtering with Main Line
When we only want one result per transaction, that means we only want the Body, or Main Line, of each transaction. To accomplish this, there is a filter named "Main Line".

By setting the *Main Line* filter to *Yes* in our search criteria, we are essentially saying "Only show me body-level data for the transactions in my results":

[![enter image description here][1]][1]

Modifying our previous search criteria this way now gives us the single result we expected originally:

[![enter image description here][2]][2]

If we reverse our *Main Line* filter to *No*, we are saying "Show me only the data from sublists in my results":

[![enter image description here][3]][3]

To recap *Main Line*'s behaviour:

* With *Main Line* set to *Yes*, we received *one* result for *only the body* of the transaction.
* With *Main Line* set to *No*, we received *three* results for *only the sublist data* of the transaction.
* With no *Main Line* filter at all, we received *four* results, essentially the combination of all the body and sublist data for the transaction.

*Note that the Main Line filter is not supported for Journal Entry searches.*

  [1]: https://i.stack.imgur.com/4lW4y.png
  [2]: https://i.stack.imgur.com/hLr63.png
  [3]: https://i.stack.imgur.com/SZpWa.png

## Filtering only on Internal ID
Let's explore an example Transaction search where we define a filter for a single transaction's internal ID:

[![Filter by Internal ID][1]][1]

We've specified a filter to only show us results for the Transaction with the internal ID of 875; here is that Transaction:

[![Example Sales Order][2]][2]

We can see it is a Sales Order with a single line item.

Because internal IDs are unique across all transactions, we can expect only one search result for this search. Here is the search result:

[![Unexpected Results][3]][3]

Instead of the single result we expect, we get *four* results. What's more, every result has exactly the same internal ID. How is that possible?

To understand what is happening here, we need to recall that data stored in NetSuite records is divided into two categories:

1. Body Data: Data stored in standalone fields of the record (e.g. Date, Sales Rep, Document Number, Coupon Code)
1. Sublist Data: Data stored in lists within each record, usually displayed on subtabs in the UI (e.g. Items on a Sales Order)

Transactions contain multiple sublists of data, including its:

* line items
* shipping information
* tax information
* COGS (Cost of Goods Sold) details

In these search results, NetSuite is actually showing us one result for the transaction body, then other results for data on the various sublists within that same transaction.

Notice the column in our search results simply named with an asterisk (*). Notice also that one of the results has an asterisk populated in this column while the rest are empty. This column indicates which search result represents the body of the transaction, which is also called the transaction's Main Line.

There are times when you will want transaction searches to only show the Main Line data, and times where you will only want the line-level detail. The remaining examples show how to control what shows up in our results.

  [1]: https://i.stack.imgur.com/7P0vY.png
  [2]: https://i.stack.imgur.com/KfGVs.png
  [3]: https://i.stack.imgur.com/WJbQT.png

## Filtering Specific Sublists
Recall that every transaction contains multiple sublists of data. Now that we can show only sublist data using *Main Line*, we can further refine our search results to specific sublist data.

Most of the sublists included in Transaction results have a corresponding search filter to toggle whether they are included in your results:

* Use the *Shipping Line* filter to control data from the Shipping sublist
* Use the *Tax Line* filter to control data from the Tax sublist
* Use the *COGS Line* filter to control data from the COGS sublist

Each of these filters behaves like *Main Line* or any other checkbox filter: *Yes* to include this data, *No* to exclude it from your results.

Notice that there is no filter for *Item Line* to control the data from the Item sublist. Essentially, in order to say "Only show me the data from the Items sublist", we need to specify all of these aforementioned filters as *No* in our criteria:

[![enter image description here][1]][1]

With this criteria, your search will return one result per item line on each matching transaction.

In my opinion, this missing filter is a major gap in the search functionality that should be fixed; it would be much easier and more consistent to simply have an *Item Line is Yes* filter. Until then, this is how you must specify that you only want Item data in your transaction results.


  [1]: https://i.stack.imgur.com/XOMbM.png

