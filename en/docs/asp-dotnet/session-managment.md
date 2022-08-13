---
title: "Session Managment"
slug: "session-managment"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Advantage and Disadvantage of Session State, types of session
     The advantages of using Session State are
    
        1)Better security
        2)Reduced bandwidth
    
    The disadvantages of using Session state are
    
        1)More resource consumption of server.
        2)Extra code/care if a Web farm is used(we will discuss this shortly)
       
 
       **Session State Modes**
    
       1) InProc mode, which stores session state in memory on the Web server. This is the default.
    
       2) StateServer mode, which stores session state in a separate process called the ASP.NET state service. This ensures that session state is preserved if the Web application is restarted and also makes session state available to multiple Web servers in a Web farm.
    
       3) SQLServer mode stores session state in a SQL Server database. This ensures that session state is preserved if the Web application is restarted and also makes session state available to multiple Web servers in a Web farm.
    
      4) Custom mode, which enables you to specify a custom storage provider.
    
        Off mode, which disables session state.



