---
title: "What are the ORM Methods and details?"
slug: "what-are-the-orm-methods-and-details"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

**Create method:** Create new record with specified value. Takes a number of field values, and returns a recordset containing the record created

    def create(self,vals):
        return super(class_name, self).create(vals)

**Write Method:** Update records with given ids with the given field values.Takes a number of field values, writes them to all the records in its recordset. Does not return anything

    def write(self,vals):
        return super(class_name, self).write(vals)

**Search method:** Search for records based on a search domain.Takes a search domain, returns a recordset of matching records. Can return a subset of matching records (offset and limit parameters) and be ordered (order parameter)

    self.search([('customer','=',True)])
    self.env['res.partner'].search(['partner','=',True])

**Browse method:** Fetch records as objects allowing to use dot notation to browse fields and relations.Takes a database id or a list of ids and returns a recordset, useful when record ids are obtained from outside Odoo (e.g. round-trip through external system) or when calling methods in the old API.

    self.browse([7,8,9])
    self.env['res.partner'].browse([7,8,9])

**Exists methods:** Returns a new recordset containing only the records which exist in the database. Can be used to check whether a record (e.g. obtained externally) still exists.

    records = records.exists()

**ref method:** Environment method returning the record matching a provided external id

    self.env.ref('base.group_public')

**ensure_one method:** checks that the recordset is a singleton (only contains a single record), raises an error otherwise

    records.ensure_one()



## Different types of ORM Methods
 1. create()
 2. write()
 3. search()
 4. browse()
 5. exists()
 6. ref()
 7. ensure_one()

