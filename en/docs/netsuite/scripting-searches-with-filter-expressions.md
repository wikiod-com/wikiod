---
title: "Scripting searches with Filter Expressions"
slug: "scripting-searches-with-filter-expressions"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

When you create searches with Suitescript, you could provide as "filters" either array of Filter objects, or filter expression. The second option is more readable and gives you very flexible option to provide nested expressions (up to 3 levels) using not only the default "AND", but also, "OR" and "NOT" operators.

## Filter term
To understand the filter expressions, we should start with Filter Term. This is a simple **array of strings**, containing at least 3 elements:
1. **Filter** (Field/Join field/Formula/Summary)
2. **Operator** (search.Operator)
3. **Values** (string value(or array of string values), to be used as filter parameter)


<!-- language: lang-js -->
     // Simple example:
     ['amount', 'equalto', '0.00']
     
     // When the field is checkbox, use 'T' or 'F'
     ['mainline', 'is', 'T']

     // You can use join fields
     ['customer.companyname', 'contains', 'ltd']

     // summary filter term
     ['sum(amount)', 'notlessthan', '170.50']

     // summary of joined fields
     ['sum(transaction.amount)', 'greatherthan', '1000.00']

     // formula:
     ["formulatext: NVL({fullname},'John')", "contains", "ohn"]

     // and even summary formula refering joined fields:
     ['sum(formulanumeric: {transaction.netamount} + {transaction.taxtotal})', 'greaterthanorequalto','100.00']

     // for selection fields, you may use 'anyof'
     // and put values in array
     ['type','anyof',['CustInvc','VendBill','VendCred']]

     // when using unary operator, like isempty/isnotempty
     // don't forget that the filter term array contains at least 3 elements
     // and put an empty string as third:
     ['email', 'isnotempty', '']

     // you may have more than 3 elements in Filter Term array,
     // when the operator requires more than one value:
     ['grossamount','between','100.00','200.00']

In some selector fields, you can use special values.

<!-- language: lang-js -->
     // When filtering the user related fields, you can use:
     // Me (Current user): @CURRENT@
     // My Team (somebody from the team I am leading): @HIERARCHY@
     ['nextapprover','is','@CURRENT@']

     // The same is valid for Subsidiary, Department, Location, etc.
     // @CURRENT@ means MINE (Subsidiary, Department...)
     // @HIERARCHY@ means MINE or DESCENDANTS
     ["subsidiary","is","@HIERARCHY@"]

     // on selection fields you may use @ANY@ and @NONE@
     ['nextapprover','is','@NONE@']





## Filter expression
Simple filter expression is also an **array**. It contains one or more filter terms, combined with operators - 'AND', 'OR', 'NOT'. (Operators are case insensitive):

<!-- language: lang-js -->

    [ 
      ['mainline', 'is', 'T'],
      'and', ['type','anyof',['CustInvc','CustCred']],
      'and', 'not', ['amount', 'equalto', '0.00'],
      'or', ['customer.companyname', 'contains', 'ltd']
    ]
More complex filter expressions, could contain filter terms **AND** nested filter expressions, combined with operators. No more than 3 levels of nested expressions are allowed:

<!-- language: lang-js -->

    [ 
      ['mainline', 'is', 'T'],
      'and', ['type','anyof',['CustInvc','CustCred']],
      'and', [ ['customer.companyname', 'contains', 'ltd'],
               'or', ['customer.companyname', 'contains', 'inc']
             ],
      'and', [ ['subsidiary', 'is', 'HQ'],
               'or', ['subsidiary', 'anyof', '@HIERARCHY@']
             ],
      'and', ['trandate', 'notbefore', 'yesterday']
     ]

And finally, let's put all this altogether in a SS2.0 sample:

<!-- language: lang-js -->

    var s = search.create({
        type    : 'transaction',        
        columns : [
                   'trandate', 
                   'tranid',
                   'currency',
                   'customer.companyname',
                   'customer.country', 
                   'amount' 
                  ],
        filters : [ 
                    ['mainline', 'is', 'T'],
                    'and', ['type','anyof',['VendBill','VendCred']],
                    'and', [ ['customer.companyname', 'contains', 'ltd'],
                             'or', ['customer.companyname', 'contains', 'inc']
                           ],
                    'and', [ ['subsidiary', 'is', 'HQ'],
                             'or', ['subsidiary', 'anyof', '@HIERARCHY@']
                           ],
                    'and', ['trandate', 'notbefore', 'yesterday']
                  ]
    });



## Filter expressions vs Filter Objects
Filter expressions **cannot include** Filter Objects. This is very important. If you decide to form your filters with Filter Expression, you use array of string arrays. The following syntax is **wrong**:

<!-- language: lang-js -->

    // WRONG!!!
    var f1 = search.createFilter({
                name: 'mainline',
                operator: search.Operator.IS,
                values: 'T'
    });

    var f2 = search.createFilter({
                name: 'type',
                operator: search.Operator.ANYOF,
                values: ['VendBill','VendCred']
    });

    // here you will receive an error message
    var s = search.create({
        type    : 'transaction',        
        filters : [ f1, 'and', f2 ] // f1,f2 are Filter Objects, instead of string arrays
    });

Instead, use the **correct**:
<!-- language: lang-js -->

    // CORRECT!!!
    var f1 = ['mainline', search.Operator.IS, 'T'];

    var f2 = ['type', search.Operator.ANYOF, ['VendBill','VendCred'] ];

    var s = search.create({
        type    : 'transaction',        
        filters : [ f1, 'and', f2 ]
    });

or if you want to keep with Filter Objects approach, pass an array of filter objects, and forget about operators 'AND', 'OR', 'NOT'. It will be always **AND**


<!-- language: lang-js -->

    // correct, but not useful
    var f1 = search.createFilter({
                name: 'mainline',
                operator: search.Operator.IS,
                values: 'T'
    });

    var f2 = search.createFilter({
                name: 'type',
                operator: search.Operator.ANYOF,
                values: ['VendBill','VendCred']
    });


    var s = search.create({
        type    : 'transaction',        
        filters : [ f1, f2 ] // here you have array of Filter Objects,
                             // filtering only when all of them are TRUE
    });


## Useful hints
1. Here you can find the list of available search filter values for date fileds:<br/>
https://system.netsuite.com/app/help/helpcenter.nl?fid=section_N3010842.html<br/>
These you can use in expressions like:


<!-- language: lang-js -->

    ['trandate', 'notbefore', 'daysAgo17']

2. Here are the search operators:<br/>
https://system.netsuite.com/app/help/helpcenter.nl?fid=section_N3005172.html<br/>
Of course you can use **serach.Operator** enum:<br/>
https://system.netsuite.com/app/help/helpcenter.nl?fid=section_4345782273.html<br/>

3. Here are the search summary types:<br/>
https://system.netsuite.com/app/help/helpcenter.nl?fid=section_N3010474.html<br/>

4. You can use ANYOF operator only on select type fields (List/Record). If you want to use it against free-text fields (like names, emails etc.), the only way is to create a nested Filter Expression with 'OR' operators:


<!-- language: lang-js -->

    [ ['email', 'startswith', 'user1@abcd.com'],
      'or', ['email', 'startswith', 'user2@abcd.com'], 
      'or', ['email', 'startswith', 'user3@abcd.com'], 
      'or', ['email', 'startswith', 'user4@abcd.com'] 
    ]

or you can write small script, doing this instead of you:

<!-- language: lang-js -->

    function stringFieldAnyOf(fieldId, listOfValues) {    
        var result = [];
        if (listOfValues.length > 0) {
            for (var i = 0; i < listOfValues.length; i++) {
                result.push([fieldId, 'startswith', listOfValues[i]]);
                result.push('or');
            }
            result.pop(); // remove the last 'or'
        }
        return result;
    }

    // usage: (two more filters added just to illustrate how to combine with other filters)
    var custSearch = search.create({
      type: record.Type.CUSTOMER,
      columns: searchColumn,
      filters: [
                ['companyname', 'startswith', 'A'], 
                'and', stringFieldAnyOf('email', ['user1@abcd.com', 'user2@abcd.com']),
                'and', ['companyname', 'contains', 'b']
               ]

    });

5. Still not confident? Looking for a cheat? :) <br/>
Create a saved search in the Netsuite UI, take the search ID (lets say: customsearch1234) and log.debug the filter expression:


<!-- language: lang-js -->

        var s = search.load('customsearch1234');
        
        log.debug('filterExpression', JSON.stringify(s.filterExpression));




