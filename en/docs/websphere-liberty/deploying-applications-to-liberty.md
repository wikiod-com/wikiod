---
title: "Deploying applications to Liberty"
slug: "deploying-applications-to-liberty"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Deploying a simple application on the command line
 1. Create a simple servlet:


    package web.example;
    
    import java.io.IOException;
    import javax.servlet.ServletException;
    import javax.servlet.annotation.WebServlet;
    import javax.servlet.http.HttpServlet;
    import javax.servlet.http.HttpServletRequest;
    import javax.servlet.http.HttpServletResponse;
    
    @WebServlet("/*")
    public class HelloServlet extends HttpServlet 
    {           
        protected void doGet(HttpServletRequest request, HttpServletResponse response) 
                throws ServletException, IOException 
        {
            response.getWriter().println("Hello world!");
        }
    }
<br>


2. Package the application into a Web Archive (.war):


    helloapp.war
    +- META-INF\
    +- WEB-INF\
       +- web\example\HelloServlet.class

3. Add the application to your Liberty server:


    $>  mv helloapp.war $WLP_INSTALL_DIR/usr/servers/myServer/apps/


4. Configure your server.xml to know the application and enable the Servlet 3.1 technology:


    $>  cat $WLP_INSTALL_DIR/usr/servers/myServer/server.xml

    <server>
      <featureManager>
        <feature>servlet-3.1</feature>
      </featureManager>
      
      <application location="helloapp.war"/>    
    </server>
    
        
5. Start the server:

    
    $>  server start myServer
    Starting server myServer
    Server myServer started with process ID 1234.


6.  Check the console.log to verify that the application started, and what URL to find it at:


    $>  tail $WLP_INSTALL_DIR/usr/servers/myServer/logs/console.log
    ...
    [AUDIT   ] CWWKT0016I: Web application available (default_host): http://localhost:9080/helloapp/
    [AUDIT   ] CWWKZ0001I: Application helloapp started in 0.272 seconds.


7.  In a web browser, to go the URL `http://localhost:9080/helloapp/` as indicated in the console.log.  You should see the message from your servlet:

 
    Hello world!


    

