---
title: "Creating custom tag libraries using JSP 2.0(Without java classes)"
slug: "creating-custom-tag-libraries-using-jsp-20without-java-classes"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

In order to use custom tags in JSP,we used to have TLD files,along with a Java class to define the custom tags but post JSP 2.0 specs,the same can be achieved,without the need for any Source code files,making it easy to maintain and it can also be created by someone with basic knowledge of JSP tags without need for java code.

Usually for adding static content to a HTML page,we use either
>link rel="import" tag
(in HTML5) or jQuery .load("<Html file>")

to include any external content.In case of any dynamic content,using JSP,we use
> jsp:include

to include such content.But for cases,when we want to add custom content,which can interact with the existing content,we use custom JSP tags.They allow us to generate custom content,based on params from the caller and also pass back values processed in the custom tag back to the caller.

References:

 1. [Custom tags in JSP pages][1]
 2. [Difference between JSP tags and includes][2]
 3. [Tag variable scope][3]


  [1]: https://docs.oracle.com/cd/E19159-01/819-3669/bnalj/index.html
  [2]: http://stackoverflow.com/questions/14580120/whats-the-difference-between-including-files-with-jsp-include-directive-jsp-in
  [3]: https://tomcat.apache.org/tomcat-5.5-doc/jspapi/javax/servlet/jsp/tagext/VariableInfo.html

## A basic example of using a custom tag without any Java code
In order to create an use a custome tag,we need to follow couple of steps:

 1. Create a tag file,defining the attributes used by it and any variables which will be used by the tag
         
       a. The attributes need to have a name,their type and and required field with a boolean value

       b. The variables will be defined with a name along with a certain scope-

       NESTED(Available in tag body),

       AT_BEGIN(Within tag till end of scope) and 

       AT_END(From end of tag till end of scope)

            <%@ attribute name="name" required="true" type="java.lang.String" description="Name of User"  %>
            <%@ attribute name="role" required="true" type="java.lang.String" description="Role of User" %>
            <%@ variable name-given="passBack" scope="AT_BEGIN"%>
            <%@taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%>
        
            <h1>${name}</h1>
            <h2>${role}</h2>
            
            <%String backToCaller="Back to caller";%>
            <c:set var="passBack" value="Pass back successful"/>

 2. Define the entry to import tag,where tagdir will be directory containing tag file with .tag extension

        <%@ taglib prefix="tags" tagdir="/WEB-INF/tags" %>

 3. Finally use the tag,passing the required attributes,as per tag definition.
Here `customTag` is the name of the tag file,along with prefix as defined

        <tags:customTag name="Hello Tag!!" role="I am the boss here"/>

 4. The variable set from tag can be retrieved in the caller jsp using expression language

    Hello ${passBack}

         

 



