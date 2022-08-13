---
title: "TRYCATCH"
slug: "trycatch"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

TRY/CATCH is a language construct specific to MS SQL Server's T-SQL.

It allows error handling within T-SQL, similar to that seen in .NET code.

## Transaction In a TRY/CATCH

This will rollback both inserts due to an invalid datetime:

    BEGIN TRANSACTION
    BEGIN TRY
        INSERT INTO dbo.Sale(Price, SaleDate, Quantity)
        VALUES (5.2, GETDATE(), 1)
        INSERT INTO dbo.Sale(Price, SaleDate, Quantity)
        VALUES (5.2, 'not a date', 1)
        COMMIT TRANSACTION
    END TRY
    BEGIN CATCH
        THROW
        ROLLBACK TRANSACTION
    END CATCH

This will commit both inserts:

    BEGIN TRANSACTION
    BEGIN TRY
        INSERT INTO dbo.Sale(Price, SaleDate, Quantity)
        VALUES (5.2, GETDATE(), 1)
        INSERT INTO dbo.Sale(Price, SaleDate, Quantity)
        VALUES (5.2, GETDATE(), 1)
        COMMIT TRANSACTION
    END TRY
    BEGIN CATCH
        THROW
        ROLLBACK TRANSACTION
    END CATCH

