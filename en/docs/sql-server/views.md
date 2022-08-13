---
title: "Views"
slug: "views"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Views are stored queries that can be queried like regular tables. Views are not part of the physical model of the database. Any changes that are applied to the data source of a view, such as a table, will be reflected in the view as well.

## Create a view with schema binding
If a view is created WITH SCHEMABINDING, the underlying table(s) can't be dropped or modified in such a way that they would break the view. For example, a table column referenced in a view can't be removed.


    CREATE VIEW dbo.PersonsView
    WITH SCHEMABINDING
    AS
    SELECT 
        name,
        address
    FROM dbo.PERSONS  -- database schema must be specified when WITH SCHEMABINDING is present

Views *without* schema binding can break if their underlying table(s) change or get dropped. Querying a broken view results in an error message. sp_refreshview can be used to ensure existing views without schema binding aren't broken.

## Create a view
    CREATE VIEW dbo.PersonsView
    AS
    SELECT
        name, 
        address 
    FROM persons;
    

## Create or replace view
This query will drop the view - if it already exists - and create a new one.

    IF OBJECT_ID('dbo.PersonsView', 'V') IS NOT NULL
        DROP VIEW dbo.PersonsView
    GO
    
    CREATE VIEW dbo.PersonsView
    AS
    SELECT
        name, 
        address 
    FROM persons;


