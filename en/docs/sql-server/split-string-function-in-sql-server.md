---
title: "Split String function in Sql Server"
slug: "split-string-function-in-sql-server"
draft: false
images: []
weight: 9923
type: docs
toc: true
---

## Split string in Sql Server 2008/2012/2014 using XML
Since there is no `STRING_SPLIT` function we need to use XML hack to split the string into rows:

**Example:**

    SELECT split.a.value('.', 'VARCHAR(100)') AS Value 
    FROM   (SELECT Cast ('<M>' + Replace('A|B|C', '|', '</M><M>')+ '</M>' AS XML) AS Data) AS A 
           CROSS apply data.nodes ('/M') AS Split(a); 

**Result:**

    +-----+
    |Value|
    +-----+
    |A    |
    +-----+
    |B    |
    +-----+
    |C    |
    +-----+

## Split a String in Sql Server 2016
In **SQL Server 2016** finally they have introduced Split string function : [**`STRING_SPLIT`**][1] 

**Parameters:** 
It accepts two parameters 

***String***:

> Is an expression of any character type (i.e. nvarchar, varchar,
> nchar or char).

***separator*** :

> Is a single character expression of any character type (e.g.
> nvarchar(1), varchar(1), nchar(1) or char(1)) that is used as
> separator for concatenated strings.

**Note:** You should always check if the expression is a non-empty string.

**Example:**

    Select Value
    From STRING_SPLIT('a|b|c','|')

In above example 

    String    : 'a|b|c'
    separator : '|'

**Result :**

    +-----+
    |Value|
    +-----+
    |a    |
    +-----+
    |b    |
    +-----+
    |c    |
    +-----+

**If it's an empty string:**

    SELECT value
    FROM STRING_SPLIT('',',')


**Result :**

      +-----+
      |Value|
      +-----+
    1 |     |
      +-----+

You can avoid the above situation by adding a `WHERE` clause

    SELECT value
    FROM STRING_SPLIT('',',')
    WHERE LTRIM(RTRIM(value))<>''

  [1]: https://msdn.microsoft.com/en-gb/library/mt684588.aspx


## T-SQL Table variable and XML
    Declare @userList Table(UserKey VARCHAR(60))
    Insert into @userList values ('bill'),('jcom'),('others')
    --Declared a table variable and insert 3 records

    Declare @text XML
    Select  @text = (
            select UserKey  from @userList for XML Path('user'), root('group')
    ) 
    --Set the XML value from Table 

    Select @text
    
    --View the variable value
    XML: \<group>\<user>\<UserKey>bill\</UserKey>\</user>\<user>\<UserKey>jcom\</UserKey>\</user>\<user>\<UserKey>others\</UserKey>\</user>\</group>



