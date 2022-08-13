---
title: "Pagination with PHP and MySql"
slug: "pagination-with-php-and-mysql"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| ------ | ------ |
| $params = []   | optional parameters for `['table' => 'tableName', 'sort' => 'ASC', 'columns' => 'colId, name, etc']`  |
| $attributes = [] | optional list of attributes `['ul-class': => 'space separated list of classes', 'ul-attr': 'id="someId" data-pre="pre"', 'li-class': 'space separated list of classes', 'li-attr': 'id="someid"']` |

## Creating a Simple Pagination
You need to include the `Pagination.php` in your page/s.

    require_once 'Paginator.php';

    $Paginator = new Paginator('mysql:host=localhost;dbname=ng_app', 'root', '000000');
    $Paginator->setItemLimitPerPage(4);
    $Paginator->setTable('comments');
    $Paginator->createPages(); // this will create pages using PHP copy()
    $Paginator->setCurrentPageClass('active'); // set the current page class
    $Paginator->setUrlPattern('/php_paginator/');
    $numPrevPage = 4; // number of pages to appear before the current page
    $numNextPage = 4; // number of pages to appear after the current page
    $paginationCssClass = 'pagination';

Creating Pagination
-------------------


----------

    <div class="text-center">
        <!-- our pagination  using Bootstrap-->
        <hr>
        <?php
          $Paginator->pagination($Paginator->getPageNumber(), $numPrevPage, $numNextPage, $paginationCssClass);
        ?>
    </div

[![enter image description here][1]][1]

Creating Paginator file
---------------------------


----------

    <?php
    
    class Paginator
    {
        private $_db;
        private $_table = null;
        private $_currentPageClass = '';
        private $_itemLimitPerPage;
        private $_rowOffset = 0;
        private $_urlPattern = '/';
    
        /**
         * @return string url pattern
         */
        public function getUrlPattern()
        {
            return $this->_urlPattern;
        }
    
        /**
         * @param string $urlPattern
         */
        public function setUrlPattern($urlPattern)
        {
            $this->_urlPattern = $urlPattern;
        }
    
        /**
         * @return int value of itemLimitPerPage
         */
        public function getItemLimitPerPage()
        {
            return $this->_itemLimitPerPage;
        }
    
        /**
         * @param int $limitItems number of items per page
         */
        public function setItemLimitPerPage($limitItems)
        {
            $this->_itemLimitPerPage = $limitItems;
        }
    
        /**
         * @return int value of rowOffset
         */
        public function getRowOffset()
        {
            return $this->_rowOffset;
        }
    
        /**
         * @param int $rowOffset number of row offset
         */
        public function setRowOffset($rowOffset)
        {
            $this->_rowOffset = $rowOffset;
        }
    
    
        /**
         * Paginator constructor.
         * @param string $dsn database host and database name
         * @param string $username database username
         * @param string $password user password
         */
        public function __construct($dsn, $username, $password)
        {
            try {
                $this->_db = new PDO($dsn, $username, $password);
            } catch (PDOException $e) {
                echo $e->getMessage();
            }
        }
    
        /**
         * Get the name of the table
         * @return string the name of the table
         */
        public function getTable()
        {
            return $this->_table;
        }
    
        /**
         * Set the name of the table
         * @param string $table the name of the table to be used
         */
        public function setTable($table)
        {
            $this->_table = $table;
        }
    
        /**
         * Get the class to be used on the current item/page
         * @return string the current page class
         */
        public function getCurrentPageClass()
        {
            return $this->_currentPageClass;
        }
    
        /**
         * Set the class to be used on the current item/page
         * @param string $currentPageClass set the class to be used for the current page
         */
        public function setCurrentPageClass($currentPageClass)
        {
            $this->_currentPageClass = $currentPageClass;
        }
    
    
        /**
         * Get the number of rows available
         * @param null $table optional table name
         * @return int the number of row count
         * @throws Exception when table is not set or provided
         */
        public function getRowCount($table = null)
        {
            if ($this->_table === null && $table === null) {
                throw new Exception("Table was not set");
            } else {
                if ($table !== null) {
                    $stmt = $this->_db->prepare("SELECT * FROM $table");
                    $stmt->execute();
                    return $stmt->rowCount();
                } elseif ($this->_table !== null) {
                    $stmt = $this->_db->prepare("SELECT * FROM $this->_table");
                    $stmt->execute();
                    return $stmt->rowCount();
                }
            }
        }
    
        /**
         * Get the number of rows left from the database
         * @param null $table optional table name
         * @return int number of rows left
         * @throws Exception when table is not set or provided
         */
        public function getRowsLeft($table = null)
        {
            if ($this->getCurrentPage() !== 'index.php') {
                $this->_rowOffset = ($this->_itemLimitPerPage * $this->getPageNumber());
            }
            if ($this->_table === null && $table === null) {
                throw new Exception("Table was not set");
            } else {
                if ($table !== null) {
                    $stmt = $this->_db->prepare("SELECT * FROM $table LIMIT " . $this->getRowOffset() . "," . $this->getItemLimitPerPage());
                    $stmt->execute();
                    return $stmt->rowCount();
                } elseif ($this->_table !== null) {
                    $stmt = $this->_db->prepare("SELECT * FROM $this->_table LIMIT " . $this->getRowOffset() . "," . $this->getItemLimitPerPage());
                    $stmt->execute();
                    return $stmt->rowCount();
                }
            }
        }
    
        /**
         * Get data to be used on the current page
         * @param int $colId column id
         * @param array $params optional parameters for ['table' => 'tableName', 'sort' => 'ASC', 'columns' => 'colId, name, etc']
         * @return array columns from database
         * @throws Exception when table is not set or provided
         */
        public function getPageData($colId, $params = [])
        {
            if ($this->_table === null && !isset($params['table'])) {
                throw new Exception("Table was not set");
            }
            $columns = isset($params['columns']) ? $params['columns'] : '*';
            $sort = isset($params['sort']) ? $params['sort'] : 'DESC';
            if (isset($params['table'])) {
                $table = $params['table'];
                $rowsLeft = $this->getRowsLeft($table);
                if ($rowsLeft < $this->_itemLimitPerPage) {
                    $this->_itemLimitPerPage = $rowsLeft;
                }
                $select = "SELECT $columns FROM " . $table . " ORDER BY $colId $sort LIMIT ?,?";
                $prepare = $this->_db->prepare($select);
                $prepare->bindParam(1, $this->_rowOffset, PDO::PARAM_INT);
                $prepare->bindParam(2, $this->_itemLimitPerPage, PDO::PARAM_INT);
                $prepare->execute();
                $results = $prepare->fetchAll();
                return $results;
            } elseif ($this->_table !== null) {
                $rowsLeft = $this->getRowsLeft($this->_table);
                if ($rowsLeft < $this->_itemLimitPerPage) {
                    $this->_itemLimitPerPage = $rowsLeft;
                }
                $prepare = $this->_db->prepare("SELECT * FROM $this->_table ORDER BY $colId $sort LIMIT " . $this->getRowOffset() . "," . $this->getItemLimitPerPage());
                $prepare->execute();
                $results = $prepare->fetchAll();
                return $results;
            }
        }
    
        /**
         * Create pages that will appear before the current page
         * @param int $pageNumber the current page number
         * @param int $numPrevPages the number of pages to appear before the current page
         * @param $cssClass class set to the li list
         * @param $attr attribtes for li list
         * @return string list of pagination links
         */
        function prevPages($pageNumber, $numPrevPages, $cssClass, $attr)
        {
            $listItems = ''; // to save all list items.
            while ($numPrevPages >= 1) {
                $pageNumber -= 1;
                if ($pageNumber >= 1) {
                    $page = $pageNumber . '.php';
                    if (file_exists("$page")) {
                        $listItems = '<li class="' . $cssClass . '" ' . $attr . '><a href="' . $this->getUrlPattern() . $pageNumber . '.php">' . $pageNumber . '</a></li>' . $listItems;
                    }
                }
                $numPrevPages -= 1;
            }
            return $listItems;
        }
    
        /**
         * Create pages that will appear after the current page
         * @param $pageNumber the current page number
         * @param $numNextPages the number of pages to appear after the current page
         * @param $cssClass class set to the li list
         * @param $attr attribtes for li list
         * @return string list of pagination links
         */
        function nextPages($pageNumber, $numNextPages, $cssClass, $attr)
        {
            $listItems = ''; // to save list items.
            $count = 1;
            while ($count <= $numNextPages) {
                $pageNumber += 1;
                $page = $pageNumber . '.php';
                if (file_exists("$page")) {
                    $listItems .= '<li class="' . $cssClass . '" ' . $attr . '><a href="' . $this->getUrlPattern() . $pageNumber . '.php">' . $pageNumber . '</a></li>';
                }
                $count += 1;
            }
            return $listItems;
        }
    
        /**
         * Create the pagination links
         * @param $pageNumber the current page number
         * @param $numPrevPages the number of pages to appear before the current page
         * @param $numNextPages  the number of pages to appear after the current page
         * @param array $attributes optional list of list attributes.
         * ['ul-class': => 'space separated list of classes', 'ul-attr': 'id="someId" data-pre="pre"', 'li-class': 'space separated list of classes', 'li-attr': 'id="someid"']
         */
        function pagination($pageNumber, $numPrevPages, $numNextPages, $attributes = [])
        {
            $ulCssClass = isset($attributes['ul-class']) ? $attributes['ul-class'] : '';
            $ulAttr = isset($attributes['ul-attr']) ? $attributes['ul-attr'] : '';
            $liCssClass = isset($attributes['li-class']) ? $attributes['li-class'] : '';
            $liAttr = isset($attributes['li-attr']) ? $attributes['li-attr'] : '';
            $prevPagesList = '<ul class="' . $ulCssClass . '" ' . $ulAttr . '>' . $this->prevButton($pageNumber) . $this->prevPages($pageNumber, $numPrevPages, $liCssClass, $liAttr);
            $nextPageList = $this->nextPages($pageNumber, $numNextPages, $liCssClass, $liAttr) . $this->nextButton($pageNumber) . '</ul>';
            if ($pageNumber == 'index') {
                $listItems = $prevPagesList . $nextPageList;
            } else {
                $listItems = $prevPagesList . '<li class="' . $this->getCurrentPageClass() . '"><a href="">' . $pageNumber . '</a> </li>' . $nextPageList;
            }
            echo $listItems;
        }
    
        /**
         * Create a link for previous button
         * @param $pageNumber the current page number
         * @return string the previous link item list
         */
        function prevButton($pageNumber)
        {
            $prev = '';
            if ($pageNumber == 1) {
                $prev = '<li><a href="index.php">&laquo; Previous</a></li>';
            } elseif ($pageNumber > 1) {
                $prev = '<li><a href="' . $this->getUrlPattern() . ($pageNumber - 1) . '.php' . '">&laquo; Previous</a></li>';
            }
            return $prev;
        }
    
        /**
         * Create a link for next button
         * @param $pageNumber the current page number
         * @return string the next link item list
         */
        function nextButton($pageNumber)
        {
            if ($pageNumber == 'index') {
                $page = '1.php';
            } else {
                $page = ($pageNumber + 1) . '.php';
            }
            if (file_exists($page)) {
                return '<li><a href="' . $this->getUrlPattern() . ($pageNumber + 1) . '.php">Next &raquo; </a></li>';
            }
            return '';
        }
    
        /**
         * Get the current page number
         * @return int the current page number
         */
        function getPageNumber()
        {
            $currentPage = basename($_SERVER['SCRIPT_FILENAME']);
            $pageNumber = rtrim($currentPage, '.php');
            return $pageNumber;
        }
    
        /**
         * Get the current page
         * @return string return the current page
         */
        function getCurrentPage()
        {
            $currentPage = basename($_SERVER['SCRIPT_FILENAME']);
            return $currentPage;
        }
    
        /**
         * create the required pages
         */
        function createPages()
        {
            $last_page = ($this->getRowCount() / $this->getItemLimitPerPage()) - 1;
            if (!is_int($last_page)) {
                $last_page = (int)$last_page + 1;
            }
            for ($counter = 1; $counter <= $last_page; $counter++) {
                $page = $counter . '.php';
                if (!file_exists($page)) {
                    copy('index.php', $page);
                }
            }
        }
    }

[see this repo for more info][2]


  [1]: https://i.stack.imgur.com/JW3bx.png
  [2]: https://github.com/julekgwa/php_paginator

