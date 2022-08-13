---
title: "SQL in Excel VBA - Best Practices"
slug: "sql-in-excel-vba---best-practices"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## How to use ADODB.Connection in VBA?
Requirements:
=============

Add following references to the project:

•    Microsoft ActiveX Data Objects 2.8 Library

•    Microsoft ActiveX Data Objects Recordset 2.8 Library

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/3BDBt.png


Declare variables
=================

    Private mDataBase As New ADODB.Connection
    Private mRS As New ADODB.Recordset
    Private mCmd As New ADODB.Command

Create connection
=================

a. with Windows Authentication
------------------------------

    Private Sub OpenConnection(pServer As String, pCatalog As String)
        Call mDataBase.Open("Provider=SQLOLEDB;Initial Catalog=" & pCatalog & ";Data Source=" & pServer & ";Integrated Security=SSPI")
        mCmd.ActiveConnection = mDataBase
    End Sub

b. with SQL Server Authentication
---------------------------------

    Private Sub OpenConnection2(pServer As String, pCatalog As String, pUser As String, pPsw As String)
        Call mDataBase.Open("Provider=SQLOLEDB;Initial Catalog=" & pCatalog & ";Data Source=" & pServer & ";Integrated Security=SSPI;User ID=" & pUser & ";Password=" & pPsw)
        mCmd.ActiveConnection = mDataBase
    End Sub

Execute sql command
===================

    Private Sub ExecuteCmd(sql As String)
        mCmd.CommandText = sql
        Set mRS = mCmd.Execute
    End Sub

Read data from record set
=========================

    Private Sub ReadRS()
        Do While Not (mRS.EOF)
            Debug.Print "ShipperID: " & mRS.Fields("ShipperID").Value & " CompanyName: " & mRS.Fields("CompanyName").Value & " Phone: " & mRS.Fields("Phone").Value
            Call mRS.MoveNext
        Loop
    End Sub

Close connection
================

    Private Sub CloseConnection()
        Call mDataBase.Close
        Set mRS = Nothing
        Set mCmd = Nothing
        Set mDataBase = Nothing
    End Sub

How to use it?
==============

    Public Sub Program()
        Call OpenConnection("ServerName", "NORTHWND")
        Call ExecuteCmd("INSERT INTO [NORTHWND].[dbo].[Shippers]([CompanyName],[Phone]) Values ('speedy shipping','(503) 555-1234')")
        Call ExecuteCmd("SELECT * FROM [NORTHWND].[dbo].[Shippers]")
        Call ReadRS
        Call CloseConnection
    End Sub

Result
======

ShipperID: 1 CompanyName: Speedy Express Phone: (503) 555-9831

ShipperID: 2 CompanyName: United Package Phone: (503) 555-3199

ShipperID: 3 CompanyName: Federal Shipping Phone: (503) 555-9931

ShipperID: 4 CompanyName: speedy shipping Phone: (503) 555-1234


