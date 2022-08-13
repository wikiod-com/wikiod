---
title: "Executing a Search"
slug: "executing-a-search"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## SS 2.0 From Saved Search
    require(['N/search'], function(SEARCHMODULE){
        var savedSearchId = 'customsearch_mySavedSearch';
        var mySearch = SEARCHMODULE.load(savedSearchId);
        var resultset = mySearch.run();
        var results = resultset.getRange(0, 1000);
        for(var i in results){
            var result = results[i];
            for(var k in result.columns){
                log.debug('Result is ' + result.getValue(result.columns[k])); //Access result from here
            }
        }
    });

## SS 2.0 Ad Hoc Search
    require(['N/search'], function(SEARCHMODULE){
        
        var type = 'transaction';
        var columns = [];
        columns.push(SEARCHMODULE.createColumn({
            name: 'internalid'
        }));
        columns.push(SEARCHMODULE.createColumn({
            name: 'formulanumeric',
            formula: '{quantity}-{quantityshiprecv}'
        }));
        
        var salesOrdersArray = [123,456,789];
        var filters = [];
        filters.push(['type', 'anyof', 'SalesOrd']);
        filters.push('and');
        filters.push(['mainline', 'is', 'F']);
        filters.push('and');
        filters.push(['internalid', 'anyof', salesOrdersArray]);
            
        var mySearchObj = {};
        mySearchObj.type = type;
        mySearchObj.columns = columns;
        mySearchObj.filters = filters;
        
        var mySearch = SEARCHMODULE.create(mySearchObj);
        var resultset = mySearch.run();
        var results = resultset.getRange(0, 1000);
        for(var i in results){
            var result = results[i];
            var row = {};
            for(var k in result.columns){
                log.debug('Result is ' + result.getValue(result.columns[k])); //Access result from here
            }
        }
    });

## Performing a summarized search
<!-- language: lang-js -->

    // Assuming N/search is imported as `s`
    var mySalesOrderSearch = s.create({
        type: 'salesorder'
        // Use the summary property of a Column to perform grouping/summarizing
        columns: [{
            name: 'salesrep',
            summary: s.Summary.GROUP
        },{
            name: 'internalid',
            summary: s.Summary.COUNT
        }],
        filters: [{
            name: 'mainline',
            operator: 'is',
            values: ['T']
        }]
    });

    mySalesOrderSearch.run().each(function (result) {
        var repId = result.getValue({
            "name": "salesrep",
            "summary": s.Summary.GROUP
        });
        var repName = result.getText({
            "name": "salesrep",
            "summary": s.Summary.GROUP
        });
        var orderCount = parseInt(result.getValue({
                "name": "internalid",
                "summary": s.Summary.COUNT
        }), 10);

        log.debug({
            "title": "Order Count by Sales Rep",
            "details": repName + " has sold " + orderCount + " orders."
        });
    });

