---
title: "Getting started with pagination"
slug: "getting-started-with-pagination"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting pagination set up or installed.

## Pagination solution
The following documentation describes both MySQLi and PDO supported pagination solution.

Go to https://github.com/rajdeeppaul/Pagination and download `pagination.php` file into your project directory. Let's say your directory structure looks like this:

    project directory
           |
           |--pagination.php (pagination script)
           |--index.php (file where you want to use this pagination script)

And suppose you want the URL to look like this:

    http://example.com/index.php          // user is on page 1
    http://example.com/index.php?page=1   // user is on page 1
    http://example.com/index.php?page=5   // user is on page 5
    // etc ...

<h1>index.php</h1>

 1. Include `pagination.php` file in `index.php` page, like this:

        require_once('pagination.php');

 2. Create an instance of `Pagination` class, like this:

        $pg = new Pagination($databaseDriver, $hostname, $username, $password, $databaseName);

    The constructor method takes the following parameters,

    (1)`$databaseDriver`: Database driver, currently supported drivers are MySQLi and PDO <br />
    (2)`$hostname`: Hostname <br />
    (3)`$username`: Username <br />
    (4)`$password`: Password <br />
    (4)`$databaseName`: Database name <br />

    Example(s):

        $pg = new Pagination('mysqli', 'localhost', 'root', 'pass', 'pagination_db');
        $pg = new Pagination('pdo', 'localhost', 'root', 'pass', 'pagination_db');

 3. Set pagination parameters using `setPaginationParameters()` method, like this:

        $pg->setPaginationParameters($rowsPerPage, $numOfPaginationLinks);

    The `setPaginationParameters()` method takes the following parameters,

    (1)`$rowsPerPage`: Number of table rows to display per page <br />
    (2)`$numOfPaginationLinks`: Number of pagination links to display per page <br />

    Example(s):

        $pg->setPaginationParameters(10, 5);

 4. Call `getResult()` method to display table rows based on the URL query `?page=X`, like this:

        $resultSet = $pg->getResult($queryString, $bindParameterArray, $globalGetArray, $keyFromURLQuery);

    The `getResult()` method takes the following parameters,

    (1)`$queryString`: `SELECT` query string <br />
    (2)`$bindParameterArray`: Array containing bind variables or values <br />
    (3)`$globalGetArray`: Superglobal array `$_GET` <br />
    (4)`$keyFromURLQuery`: Key from the URL query i.e. `page` <br />

    Example(s):

        $resultSet = $pg->getResult('SELECT * FROM pagination', NULL, $_GET, 'page');
        $resultSet = $pg->getResult('SELECT * FROM pagination WHERE column1 = ? AND column2 = ?', array($value1, $value2), $_GET, 'page');

    **Note:** Don't specify any `LIMIT` or `OFFSET` clause in the query, the script will take care of these.

    Now loop through the result set i.e. `$resultSet` array to access/display row details, like this:

        foreach($resultSet as $row){
            /* access/display row details */
            /* $row['column1'], $row['column2'] etc. */
        }

    **Note:** If you want to see the complete array structure, do `var_dump($resultSet);`.

 5. Display pagination links using `getPaginationLinks()` method, like this:

        $pgLinks = $pg->getPaginationLinks();

    The `getPaginationLinks()` method doesn't take any parameter and returns an array of pagination links in the following format,

        array (size=3)
          'prev' => @boolean
          'links' => @array
          'next' => @boolean

    Now loop through the `$pgLinks` array to display paginagtion links, like this:

        if(is_array($pgLinks) && count($pgLinks) && $pgLinks['prev']){
            /* previous pages are available */
            echo '&laquo; ';
        }
        if(is_array($pgLinks) && count($pgLinks) && count($pgLinks['links'])){
            /* show pagination links */
            foreach($pgLinks['links'] as $link){
                echo '<a href="example.php?page='.$link.'">'.$link.'</a> ';
            }
        }
        if(is_array($pgLinks) && count($pgLinks) && $pgLinks['next']){
            /* next pages are available */
            echo '&raquo;';
        }

    **Note:** If you want to see the complete array structure, do `var_dump($pgLinks);`.

<hr />

**Footnote(s):** You can style the result rows and pagination links as per your choice.


