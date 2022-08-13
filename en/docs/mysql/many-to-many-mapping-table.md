---
title: "Many-to-many Mapping table"
slug: "many-to-many-mapping-table"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

 - Lack of an `AUTO_INCREMENT` id for this table -- The PK given is the
   'natural' PK; there is no good reason for a surrogate.
 - `MEDIUMINT` --
   This is a reminder that all `INTs` should be made as small as is safe
   (smaller ⇒ faster). Of course the declaration here must match the
   definition in the table being linked to.
 - `UNSIGNED` -- Nearly all
   INTs may as well be declared non-negative
 - `NOT NULL` -- Well, that's
   true, isn't it?
 - `InnoDB` -- More effecient than MyISAM because of the
   way the `PRIMARY KEY` is clustered with the data in InnoDB.
 - `INDEX(y_id, x_id)` -- The `PRIMARY KEY` makes it efficient to go one
   direction; the makes the other direction efficient. No need to say
   `UNIQUE`; that would be extra effort on `INSERTs`.
 - In the secondary
   index, saying just `INDEX(y_id)` would work because it would implicit
   include `x_id`. But I would rather make it more obvious that I am
   hoping for a 'covering' index.

You _may_ want to add more columns to the table; this is rare.  The extra columns could provide information about the _relationship_ that the table represents.

You _may_ want to add `FOREIGN KEY` constraints.

## Typical schema
    CREATE TABLE XtoY (
        # No surrogate id for this table
        x_id MEDIUMINT UNSIGNED NOT NULL,   -- For JOINing to one table
        y_id MEDIUMINT UNSIGNED NOT NULL,   -- For JOINing to the other table
        # Include other fields specific to the 'relation'
        PRIMARY KEY(x_id, y_id),            -- When starting with X
        INDEX      (y_id, x_id)             -- When starting with Y
    ) ENGINE=InnoDB;

(See Remarks, below, for rationale.)

