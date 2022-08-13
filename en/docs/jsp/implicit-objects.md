---
title: "Implicit objects"
slug: "implicit-objects"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Java objects which are created by web container.There are 9 implicit objects


| object     | Type               |
| ---------- | ------------------ |
| out        | JspWriter          |
| request    | HttpServletRequest |
| response   | HttpServletResponse|
| config     | ServletConfig      |
| application| ServletContext     |
| session    | HttpSession        |
| pageContext| PageContext        |
| page       | Object             |
| exception  | Throwable          |


## JSP out implicit object - simply displaying date and time.
    <html>  
    <body>  
    <% out.print("Today is:"+java.util.Calendar.getInstance().getTime()); %>  
    </body>  
    </html>

  

## request object in JSP
used to get request information such as parameter, header information, remote address, server name, server port, content type, character encoding.

***Index.html***
  
      <form action="req.jsp">  
        <input type="text" name="username">  
        <input type="submit" value="go"><br/>  
        </form>

***req.jsp***

    <%   
    String name=request.getParameter("username");  
    out.print("welcome "+name);  
    %>

  
  

