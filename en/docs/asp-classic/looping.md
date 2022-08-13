---
title: "Looping"
slug: "looping"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## For Loop
In classic ASP we can specify a for loop with the *for* keyword. With the for statement we need the *next* statement which will increment the counter.

<!-- language: vbs -->
    For i = 0 To 10
        Response.Write("Index: " & i)
    Next
The *step* keyword can be used to changed the how the *next* statement will modify the counter.

<!-- language: vbs -->
    For i = 10 To 1 Step -1
        'VBS Comment
    Next

To exit a for loop, use the *Exit For* statement

<!-- language: vbs -->
    For i = 0 To 10
        Response.Write("Index: " & i)
        If i=7 Then Exit For 'Exit loop after we write index 7
    Next

We can also use a `For...Each` loop to perform a loop through a series of defined elements in a collection. For instance:

    Dim farm, animal
    farm = New Array("Dog", "Cat", "Horse", "Cow")
    Response.Write("Old MacDonald had a Farm, ")
    For Each animal In farm
        Response.Write("and on that farm he had a " & animal & ".<br />")
    Next


## Do Loop
Do while is very similar to for loop however this generally is used if our loop repetitions is unknown.

Do While:
<!-- language: vbs -->
    'Continues until i is greater than 10
    Do While i <= 10
        i = i + 1
    Loop

    'Or we can write it so the first loop always executes unconditionally:
    'Ends after our first loop as we failed this condition on our previous loop
    Do
        i = i + 1
    Loop While i <= 10

Do Until:

<!-- language: vbs -->

    'Ends once i equates to 10
    Do Until i = 10
        i = i + 1
    Loop

    'Or we can write it so the first loop always executes unconditionally:
    'Ends after our first loop as we failed this condition on our previous loop
    Do
        i = i + 1
    Loop Until i=10

Exiting a Do loop is similar to a for loop but just using the *Exit Do* statement.

<!-- language: vbs -->

    'Exits after i equates to 10
    Do Until i = 10
        i = i + 1
        If i = 7 Then Exit Do
    Loop

