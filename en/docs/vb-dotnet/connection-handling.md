---
title: "Connection Handling"
slug: "connection-handling"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Public connection property

        Imports System.Data.OleDb

        Private WithEvents _connection As OleDbConnection
        Private _connectionString As String = "myConnectionString"

        Public ReadOnly Property Connection As OleDbConnection
            Get
                If _connection Is Nothing Then
                    _connection = New OleDbConnection(_connectionString)
                    _connection.Open()
                Else
                    If _connection.State <> ConnectionState.Open Then
                        _connection.Open()
                    End If
                End If
                Return _connection
            End Get
        End Property

