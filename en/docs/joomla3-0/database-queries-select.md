---
title: "Database queries (select)"
slug: "database-queries-select"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

For more information on database queries, take a look at the [Selecting data using JDatabase][1]


  [1]: https://docs.joomla.org/Selecting_data_using_JDatabase

## Ordering and setting a limit
Ordering the results and setting a limit can easily be achieved with 2 additional lines added to the chain, like so:

    $db = JFactory::getDbo();
    
    $query = $db->getQuery(true);
    $query->select('*')
          ->from('#__users')
          ->where('username = '. $db->q('John'))
          ->order('id DESC')
          ->setLimit(15, 0);
     
    $db->setQuery($query);
    
    $results = $db->loadObjectList();

This orders the results by `id` in descending order showing only the first 15 records.

The `setLimit()` function takes 2 parameters. `limit` and `offset`. In the example above, we're only taking 15 records starting from the first row.

## Simple select query
A simple query that selects all users from the `#__users` table with a `username` that matches `John`

    $db = JFactory::getDbo();
    
    $query = $db->getQuery(true);
    $query->select('*');
    $query->from('#__users');
    $query->where('username = '. $db->q('John'));
     
    $db->setQuery($query);
    
    $results = $db->loadObjectList();

You can also chain the query to improve readability and reduce the SQL code like so:

    $db = JFactory::getDbo();
    
    $query = $db->getQuery(true);
    $query->select('*')
          ->from('#__users')
          ->where('username = '. $db->q('John'));
     
    $db->setQuery($query);
    
    $results = $db->loadObjectList();

Note that in this example, we have used `$db->q()` which is the shorthand methods for `$db->quote()`

