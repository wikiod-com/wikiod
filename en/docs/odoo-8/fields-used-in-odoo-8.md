---
title: "Fields used in Odoo 8"
slug: "fields-used-in-odoo-8"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

This is section where you can find the details about the fields that is being used in Odoo 8

## Parameters
| Parameters| Description|
| ------ | ------ |
| string="Name"| Optional label of the field|
| compute="_compute_name_custom"| Transform the fields into computed fields
| store=True| If computed it will store the result
| select=True| Force index on field
| readonly=True| Field will be readonly in views
| inverse="_write_name"| On update trigger
| required=True| Mandatory field
| translate=True| Translation enable
| help='blabla'| Help tooltip text
| comodel_name="model.name"| Name of the related model
| inverse_name="field_name"| relational column of the opposite model
| relation='many2many_table_name'| relational table name for many2many
| columns1='left_column_name'| relational table left column name
| column2='right_column_name'|relational table right column name  |

**Odoo and ORM:**
Odoo uses ORM(Object Relational Mapping) technique to interact with database. ORM will help to create a virtual object database that can be used within from the Python. In ORM technique each model is represented by a class that subclasses Models.model.

Models.model is the main super class for regular database persisted Odoo models. Odoo models are created by inheriting from this class.

Example:

    class Employee(Models.model):
        _name = 'module.employee'
    
        #Rest of the code goes here

Here _name is a structural attribute, which tells the system about the name of the database table to be created.

Each model has a number of class variables, each of which represents a database field in the model. Each field is represented by an instance of a openerp.fields.Field class. Fields in Odoo are listed below..

1 Boolean Field

    ex: flag = fields.Boolean()

2 Char Field

    ex: flag = fields.Char()
3 Text


    ex: flag = fields.Text()
4 Html


    ex: flag = fields.Html()
5 Integer


    ex: flag = fields.Integer()
6 Float


    ex: flag = fields.Float()
7 Date


    ex: flag = fields.Date()
8 Datetime


    ex: flag = fields.Datetime()
9 Selection


    ex: flag = fields.Selection()
10 Many2one


    ex: flag = fields.Many2one()
11 One2many


    ex: flag = fields.One2many()
12 Many2many


    ex: flag = fields.Many2many()

## Examples fields of Odoo 8
Odoo uses ORM(Object Relational Mapping) technique to interact with database. ORM will help to create a virtual object database that can be used within from the Python. In ORM technique each model is represented by a class that sub-classes Models.model.
Models.model is the main super class for regular database persisted Odoo models. Odoo models are created by inheriting from this class


    name = fields.Char(string='New Value')
    
    flag = fields.Boolean(string='Flag',default=False)
    
    amount = fields.Float(string='Amount',digits=(32, 32))
    
    code = fields.Selection(string='Code',selection=[('a', 'A'),('b','B')])
    
    customer = fields.Many2one(comodel_name='res.users')
    
    sale_order_line = fields.One2many(comodel_name='res.users', inverse_name='rel_id')
    
    tags = fields.Many2many(comodel_name='res.users',
                            relation='table_name',
                            column1='col_name',
                            column2='other_col_name')

