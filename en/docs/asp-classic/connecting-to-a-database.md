---
title: "Connecting to a database"
slug: "connecting-to-a-database"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Classic ASP utilises a technology called [ActiveX Data Objects](http://stackoverflow.com/tags/adodb/info) when requiring access to external data sources. The ADODB Library provides three main objects for this purpose, [`ADODB.Connection`](https://msdn.microsoft.com/en-us/library/ms681519(v=vs.85).aspx), [`ADODB.Command`](https://msdn.microsoft.com/en-us/library/ms677502(v=vs.85).aspx) and the [`ADODB.Recordset`](https://msdn.microsoft.com/en-us/library/ms681510(v=vs.85).aspx).



## Populating a dropdown from the database
(Caveat emptor: there are many, many programmers who go into absolute conniptions if they meet code that uses recordsets instead of commands and stored procedures.)

<!-- language: lang-vbs -->

    <%
    dim rs, sql
    dim SelectedUser
    SelectedUser = request.form("user")
    if IsNumeric(SelectedUser) then
        SelectedUser = CLng(SelectedUser)
    else
        SelectedUser = 0
    end if
    %>
    ...
    <p>Select a user: <select name="user" size="1">
    <%
    sql = "SELECT id, displayname FROM users WHERE active = 1 ORDER BY displayname"
    set rs = server.createobject("ADODB.Recordset")
    rs.open sql,"[connection string stuff goes here]",1,2
    do until rs.eof
        response.write "<option value='" & rs("id") & "'"
        if rs("id") = SelectedUser then response.write " selected"
        response.write ">" & rs("displayname") & "</option>" & vbCrLf
        rs.Movenext '<- VERY VERY IMPORTANT!
    loop
    rs.close
    set rs = nothing
    %>
    </select></p>
    ...


