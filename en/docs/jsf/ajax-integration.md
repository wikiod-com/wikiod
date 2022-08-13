---
title: "Ajax Integration"
slug: "ajax-integration"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Partially update the view
Makes an ajax request and updates only part of the view.

**Bean.java**

    @ManagedBean
    @ViewScoped
    public class Bean {
    
        public Date getCurrentDate() {
            return new Date();
        }
    
    }

**sample.xhtml**

<!DOCTYPE html>

    <html xmlns="http://www.w3.org/1999/xhtml"
        xmlns:h="http://xmlns.jcp.org/jsf/html"
        xmlns:f="http://xmlns.jcp.org/jsf/core"
        xmlns:ui="http://xmlns.jcp.org/jsf/facelets">
    <h:head />
    <h:body>
        <h:form>
            <h:commandButton value="Execute ajax">
                <f:ajax render="output" />
            </h:commandButton>
            <p>
                <h:outputText id="output" value="Ajax date: #{bean.currentDate}" />
            </p>
            <p>
                <h:outputText id="output2" value="Non-Ajax date: #{bean.currentDate}" />
            </p>
        </h:form>
    </h:body>
    </html>

## Send form parts and listen to the request
Makes a request only sending part of the form. The `text1` value is set, but not `text2`, as the listener states.

**Bean.java**

    @ManagedBean
    @ViewScoped
    public class Bean {
    
        private String text1;
    
        private String text2;
    
        public String getText1() {
            return text1;
        }
    
        public void setText1(String text1) {
            this.text1 = text1;
        }
    
        public String getText2() {
            return text2;
        }
    
        public void setText2(String text2) {
            this.text2 = text2;
        }
    
        public void listener() {
            System.out.println("values: " + text1 + " " + text2);
        }
    
    }

**sample.xhtml**

    <!DOCTYPE html>
    <html xmlns="http://www.w3.org/1999/xhtml"
        xmlns:h="http://xmlns.jcp.org/jsf/html"
        xmlns:f="http://xmlns.jcp.org/jsf/core"
        xmlns:ui="http://xmlns.jcp.org/jsf/facelets">
    <h:head />
    <h:body>
        <h:form>
            <h:inputText id="my_input" value="#{bean.text1}" />
            <h:inputText value="#{bean.text2}" />
            <h:commandButton value="Execute ajax">
                <f:ajax execute="@this my_input" listener="#{bean.listener}" />
            </h:commandButton>
        </h:form>
    </h:body>
    </html>



## Ajax on javascript event
The date is updated whenever user types on the input field:

**Bean.java**

    @ManagedBean
    @ViewScoped
    public class Bean {
        
        public Date getCurrentDate(){
            return new Date();
        }
    
    }

**sample.xhtml**

    <html xmlns="http://www.w3.org/1999/xhtml"
        xmlns:h="http://xmlns.jcp.org/jsf/html"
        xmlns:f="http://xmlns.jcp.org/jsf/core"
        xmlns:ui="http://xmlns.jcp.org/jsf/facelets">
    <h:head />
    <h:body>
        <h:form>
            <h:inputText>
                <f:ajax event="keyup" render="output" />
            </h:inputText>
            <p>
                <h:outputText id="output" value="Ajax date: #{bean.currentDate}" />
            </p>
        </h:form>
    </h:body>
    </html>

## Delay
Executes the requests with the specified delay in milliseconds, meaning that if any subsequent request happens after the previous has been queued, the first one will be skiped. The feature is available starting from JSF 2.2:

**Bean.java**

    @ManagedBean
    @ViewScoped
    public class Bean {
        
        public Date getCurrentDate(){
            return new Date();
        }
    
    }

**sample.xhtml**

    <html xmlns="http://www.w3.org/1999/xhtml"
        xmlns:h="http://xmlns.jcp.org/jsf/html"
        xmlns:f="http://xmlns.jcp.org/jsf/core"
        xmlns:ui="http://xmlns.jcp.org/jsf/facelets">
    <h:head />
    <h:body>
        <h:form>
            <h:inputText>
                <f:ajax event="keyup" render="output" delay="2000" />
            </h:inputText>
            <p>
                <h:outputText id="output" value="Ajax date: #{bean.currentDate}" />
            </p>
        </h:form>
    </h:body>
    </html>

