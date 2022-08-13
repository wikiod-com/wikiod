---
title: "Create Text Delimited File in Access Vba"
slug: "create-text-delimited-file-in-access-vba"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Some times you may need to create a delimited text file for various uses. The following example is one of many ways to help you do that. I have used a pipe ("|") for my delimiter, to change that just change the assignment of the Sep variable. In my example I dump the recordset to an array, this is by no means the defacto approach, its just my go to. This can easily be done using the recordset itself as well. 



This is worth pointing out:

     Wholeline = Wholeline & aRR(i, j) & Sep

Because recordsets dump to arrays transposed, you will have to read it to the text file backwards. This is actually kind of handy if youre working with dynamic arrays, as it is already transposed for you, so redim'ing the "row count" can be done before any heavy lifting. 

Also worth nothing:

You can easily transpose youre array by dumping it into a new one line by line using this syntax:

        Dim xaRR() As String
        ReDim xaRR(q, z)
        xaRR(j, i) = aRR(i, j)

this isnt too relevant to my post, but its worth pointing out. 

## Example:
    Private Sub this()
        Dim rs As DAO.Recordset
        Dim q%: Dim z%
        Set rs = CurrentDb.OpenRecordset("SELECT * FROM Invoice;")
    
        With rs
            rs.MoveLast
            q = rs.RecordCount
            rs.MoveFirst
            z = rs.Fields.Count
        End With
        
        Dim aRR As Variant
        
        aRR = rs.GetRows(q)
        
        Dim i%: Dim j%: Dim counter#: Dim Sep$: Dim Wholeline$: Dim NewTextFile$: Dim path$: Dim fileNameV$
        
        Sep = "|"
        path = Environ("USERPROFILE") & "\Desktop" & "\"
        fileNameV = "Text007.txt"
        NewTextFile = path & fileNameV
        Open NewTextFile For Output As #2
        For j = LBound(aRR, 2) To UBound(aRR, 2)
            For i = LBound(aRR, 1) To UBound(aRR, 1)
                Wholeline = Wholeline & aRR(i, j) & Sep
            Next i
            Print #2, Wholeline
            Wholeline = vbNullString
        Next j
        Close #2
    
        rs.Close
        Set rs = Nothing
        Erase aRR
        
    End Sub


