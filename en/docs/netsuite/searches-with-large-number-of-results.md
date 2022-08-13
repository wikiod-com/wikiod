---
title: "Searches with large number of results"
slug: "searches-with-large-number-of-results"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Suitescript 2.0 provides 4 methods to handle the search results.

They have different syntax, limitations and governance, and are appropriate for different situations. We will focus here on how to access **ALL** search results, using each of these methods.




## Using Search.ResultSet.each method
This is shortest, easiest and most commonly used method. Unfortunately, it has one major limitation - cannot be used on searches with more than 4000 results (rows).

<!-- language: lang-js -->

        // Assume that 'N/search' module is included as 'search'
        
        var s = search.create({
            type : search.Type.TRANSACTION,
            columns : ['entity','amount'],
            filters : [  ['mainline', 'is', 'T'],
                  'and', ['type', 'is', 'CustInvc'],
                  'and', ['status', 'is', 'open']
                ]
        });

        var resultSet = s.run();
        
        // you can use "each" method on searches with up to 4000 results    
        resultSet.each( function(result) {
            
            // you have the result row. use it like this....
            var transId = result.id;
            var entityId = result.getValue('entity'); 
            var entityName = result.getText('entity');
            var amount = result.getValue('amount');
            
            // don't forget to return true, in order to continue the loop
            return true;  

        });

## Using ResultSet.getRange method
In order to use getRange for handling the large number of results, we will have to consider the following: <br/>
1. getRange has 2 parameters: **start** and **end**. Always positive, always (start < end)
2. **start** is the inclusive index of the first result to return
3. **end** is the exclusive index of the last result to return
4. If there are fewer results available than requested, then the array will contain fewer than end - start entries. For example, if there are only 25 search results, then getRange(20, 30) will return an array of 5 search.Result objects.
5. Although the above help sentence doesn't say it directly, both **start** and **end** could be outside the range of available results. In the same example -  if there are only 25 search results, getRange(100, 200) will return an empty array [ ]
6. Maximum 1000 rows at a time. (end - start) <= 1000

<!-- language: lang-js -->

        // Assume that 'N/search' module is included as 'search'
        
        // this search will return a lot of results (not having any filters) 
        var s = search.create({
            type: search.Type.TRANSACTION,
            columns : ['entity','amount'],
            filters: [] 
        });
        
        var resultSet = s.run();

        // now take the first portion of data.
        var currentRange = resultSet.getRange({
                start : 0,
                end : 1000
        });
        
        var i = 0;  // iterator for all search results
        var j = 0;  // iterator for current result range 0..999

        while ( j < currentRange.length ) {
            
            // take the result row
            var result = currentRange[j];
            // and use it like this....
            var transId = result.id;
            var entityId = result.getValue('entity'); 
            var entityName = result.getText('entity');
            var amount = result.getValue('amount');
            
            // finally:
            i++; j++;
            if( j==1000 ) {   // check if it reaches 1000
                j=0;          // reset j an reload the next portion
                currentRange = resultSet.getRange({
                    start : i,
                    end : i+1000
                });
            }
        }
Lets calculate the Governance. We have 1 + count/1000 getRange calls taking 10 units each, so: <p>
G = (1 + count/1000 ) * 10<br/><br/>
Example: 9500 rows will take 100 units
        


## Using Search.PagedData.fetch method
PagedData is an object, returned by the Search.runPaged(options) method. It works exactly as the UI searches do. PagedData object contains 2 important properties, that you can see on the right side of results header in search results page in Netsuite UI:
* **count** (the total number of the results)
* **pageRanges** (list of pages, available in UI as combo-box selector)<br/>

options.pageSize parameter is limited again to 1000 result rows.<br/> **PagedData.fetch** method is used to fetch the result portion you want (indexed by pageIndex parameter). With a little bit more code, you receive the same convenient callback function as Search.ResultSet.each, without having the 4000 rows limitation. 
        
<!-- language: lang-js -->
        // Assume that 'N/search' module is included as 'search'

        // this search will return a lot of results (not having any filters)  
        var s = search.create({
            type: search.Type.TRANSACTION,
            columns : ['entity','amount'],
            filters : [] 
        });
        
        var pagedData = s.runPaged({pa​g​e​S​i​z​e : 1000});
        
        // iterate the pages
        for( var i=0; i < pagedData.pageRanges.length; i++ ) {

            // fetch the current page data
            var currentPage = pagedData.fetch(i);

            // and forEach() thru all results
            currentPage.data.forEach( function(result) {

                // you have the result row. use it like this....
                var transId = result.id;
                var entityId = result.getValue('entity'); 
                var entityName = result.getText('entity');
                var amount = result.getValue('amount');

            });

        }

Lets calculate the Governance. We have 5 units for runPaged(), and 1 + count/1000 pagedData.fetch calls taking 5 units each, so:<p>
G = 5 + ceil( count/1000 ) * 5<br/><br/>
Example: 9500 rows will take 55 units. Approximately half of the getRange governance units.

## Using dedicated Map/Reduce script
For really huge search results, you can use dedicated Map/Reduce script. It is much more inconvenient, but sometimes unavoidable. And sometimes could be very handy.<br/>
The trick here is, that in Get Input Data stage, you can provide to the NS engine not the actual data (i.e. script result), but just the definition of the search. NS will execute the search for you without counting the governance units. Then each single result row will be passed to the Map stage.<br/>
Of course, there is a limitation: The total persisted size of data for a map/reduce script is not allowed to exceed 50MB. In a search result, each key and the serialized size of each value is counted towards the total size. "Serialized" means, that the search result row is converted to string with JSON.stringify. Thus, the value size is proportional to the number of search result columns in a result set. If you get to trouble with STORAGE_SIZE_EXCEEDED error, consider reducing the columns, combining to formulas, grouping the result or even splitting the search to multiple sub-searches, which could be executed in Map or Reduce stages.<br/>

<!-- language: lang-js -->
        /**
         * @NApiVersion 2.0
         * @NScriptType MapReduceScript
         */
        define(['N/search'], function(search) {

        function getInputData()
        {
            return search.create({
                type: search.Type.TRANSACTION,
                columns : ['entity','amount'],
                filters : [] 
            });
        }

        function map(context)
        {
            var searchResult = JSON.parse(context.value);
            // you have the result row. use it like this....
            var transId = searchResult.id;
            var entityId = searchResult.values.entity.value;
            var entityName = searchResult.values.entity.text;
            var amount = searchResult.values.amount.value;
            
            // if you want to pass some part of the search result to the next stage
            // write it to context:
            context.write(entityId, transId);
        }

        function reduce(context)
        {
           // your code here ...
        }

        function summarize(summary)
        {
            // your code here ...
        }

        return {
            getInputData: getInputData,
            map: map,
            reduce: reduce,
            summarize: summarize
        };
    });


Of course the example here is simplified, without error handling and is given just to be compared with others. More examples are available at [Map/Reduce Script Type examples in NS Help Center][1]
 


  [1]: https://system.eu2.netsuite.com/app/help/helpcenter.nl?fid=section_4387799161.html

