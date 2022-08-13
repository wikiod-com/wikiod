---
title: "Database first model generation"
slug: "database-first-model-generation"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Generating model from database
In `Visual Studio` go to your `Solution Explorer` then click on `Project` you will be adding model <kbd>Right mouse</kbd>. Choose `ADO.NET Entity Data Model`

[![enter image description here][1]][1]

Then choose `Generate from database` and click `Next` in next window click `New Connection...` and point to the database you want to generate model from (Could be `MSSQL`, `MySQL` or `Oracle`)

[![enter image description here][2]][2]

After you done this click `Test Connection` to see if you have configured connection properly (do not go any further if it fails here). 

Click `Next` then choose options that you want (like style for generating entity names or to add foreign keys). 

Click `Next` again, at this point you should have model generated from database.


  [1]: http://i.stack.imgur.com/mCO7W.png
  [2]: http://i.stack.imgur.com/JvFsr.png

## Adding data annotations to the generated model
In T4 code-generation strategy used by Entity Framework 5 and higher, data annotation attributes are not included by default. To include data annotations on top of certain property every model regeneration, open template file included with EDMX (with `.tt` extension) then add a `using` statement under `UsingDirectives` method like below:

    foreach (var entity in typeMapper.GetItemsToGenerate<EntityType>
    (itemCollection))
    {
        fileManager.StartNewFile(entity.Name + ".cs");
        BeginNamespace(code);
    #>
    <#=codeStringGenerator.UsingDirectives(inHeader: false)#>
    using System.ComponentModel.DataAnnotations;  // --> add this line

As an example, suppose the template should include `KeyAttribute` which indicates a primary key property. To insert `KeyAttribute` automatically while regenerating model, find part of code containing `codeStringGenerator.Property` as below:

    var simpleProperties = typeMapper.GetSimpleProperties(entity);
        if (simpleProperties.Any())
        {
            foreach (var edmProperty in simpleProperties)
            {
    #>
        <#=codeStringGenerator.Property(edmProperty)#>
    <#
            }
        }

Then, insert an if-condition to check key property as this:

    var simpleProperties = typeMapper.GetSimpleProperties(entity);
        if (simpleProperties.Any())
        {
            foreach (var edmProperty in simpleProperties)
            {
                 if (ef.IsKey(edmProperty)) { 
    #>    [Key]
    <#      } 
    #>
        <#=codeStringGenerator.Property(edmProperty)#>
    <#
            }
        }

By applying changes above, all generated model classes will have `KeyAttribute` on their primary key property after updating model from database.

**Before**

    using System;

    public class Example
    {
        public int Id { get; set; }
        public string Name { get; set; }
    }

**After**

    using System;
    using System.ComponentModel.DataAnnotations;
    
    public class Example
    {
        [Key]
        public int Id { get; set; }
        public string Name { get; set; }
    }

