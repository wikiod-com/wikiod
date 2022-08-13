---
title: "Check if Table Exists in Current DB"
slug: "check-if-table-exists-in-current-db"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Simply place this code as a Public Module that can be called by any other procedure.  When calling the code, add in the name of the table you want to look up within the parenthesis.  The Code returns a True/False value on whether or not a table already exists.  This is helpful when needing to determine if a table needs to be deleted/purged or if a table already exists before performing additional code.

## Example
    Public Function TblExists(sTable As String) As Boolean
        
        On Error Resume Next
        Dim tdf As TableDef
      
        Set tdf = CurrentDb.TableDefs(sTable)
    
        If Err.Number = 0 Then
            TblExists = True
        Else
            TblExists = False
        End If
    End Function



## A somewhat simpler function that can be used in one line
    Public Function TableExists(value as String) as Boolean
    
        On Error Resume Next
        TableExists = Len(CurrentDb.Tabledefs(value).Name & "") > 0
    
    End Function

