---
title: "Hello Primefaces"
slug: "hello-primefaces"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Hello Primefaces
This is a simple application with primefaces, it is a login page:

1-Configuration of **web.xml**:

<!-- language: lang-xhtml -->

    <?xml version="1.0" encoding="UTF-8"?>
    <web-app version="3.1" xmlns="http://xmlns.jcp.org/xml/ns/javaee" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
             xsi:schemaLocation="http://xmlns.jcp.org/xml/ns/javaee http://xmlns.jcp.org/xml/ns/javaee/web-app_3_1.xsd">
        <context-param>
            <param-name>javax.faces.PROJECT_STAGE</param-name>
            <param-value>Development</param-value>
        </context-param>
        <servlet>
            <servlet-name>Faces Servlet</servlet-name>
            <servlet-class>javax.faces.webapp.FacesServlet</servlet-class>
            <load-on-startup>1</load-on-startup>
        </servlet>
        <servlet-mapping>
            <servlet-name>Faces Servlet</servlet-name>
            <url-pattern>*.xhtml</url-pattern>
        </servlet-mapping>
        <session-config>
            <session-timeout>
                30
            </session-timeout>
        </session-config>
        <welcome-file-list>
            <welcome-file>index.xhtml</welcome-file>
        </welcome-file-list>
    </web-app>

2-Create **ManagedBean**:

<!-- language: lang-java -->
    
    import javax.faces.application.FacesMessage;
    import javax.faces.bean.ManagedBean;
    import javax.faces.bean.RequestScoped;
    import javax.faces.context.FacesContext;
    
    @ManagedBean
    @RequestScoped
    public class LoginBean {
    
        private String username;
        private String password;
    
        public LoginBean() {
        }
    
        public void login() {
            //Simple login
            if (!this.username.equals("") && this.username.equals(password)) {
                FacesContext.getCurrentInstance().addMessage(null, new FacesMessage(
                        FacesMessage.SEVERITY_INFO, "Success", "Login with success"));
            } else {
                FacesContext.getCurrentInstance().addMessage(null, new FacesMessage(
                        FacesMessage.SEVERITY_ERROR, "Error", "Username or password not correct"));
            }
        }
    
        public String getUsername() {
            return username;
        }
    
        public void setUsername(String username) {
            this.username = username;
        }
    
        public String getPassword() {
            return password;
        }
    
        public void setPassword(String password) {
            this.password = password;
        }
    }

3-Create ***.xhtml page**:

<!-- language: lang-xhtml -->

    <?xml version='1.0' encoding='UTF-8' ?>
    <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
    <html xmlns="http://www.w3.org/1999/xhtml"
          xmlns:h="http://xmlns.jcp.org/jsf/html"
          xmlns:p="http://primefaces.org/ui">
        <h:head>
            <title>Hello Primefaces</title>
        </h:head>
        <h:body>
            <h:form>
                <p:messages id="my_message" showDetail="true" autoUpdate="true" closable="true" />
                <h:panelGrid columns="2">
                    <p:outputLabel value="UserName :" for="username"/>
                    <p:inputText id="username" value="#{loginBean.username}" required="true" 
                                 requiredMessage="Username is required"/>
                    <p:outputLabel value="Password :" for="password"/>
                    <p:password id="password" value="#{loginBean.password}" required="true" 
                                requiredMessage="Password is required"/>
                    <span/>
                    <p:commandButton value="Login" actionListener="#{loginBean.login}"/>
                </h:panelGrid>
            </h:form>
        </h:body>
    </html>

4-Deploy your application.

5-open your browser and type: `http://localhost:8080/HelloPrimefaces/`



