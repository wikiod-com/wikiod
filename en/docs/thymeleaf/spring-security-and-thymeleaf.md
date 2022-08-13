---
title: "Spring Security and Thymeleaf"
slug: "spring-security-and-thymeleaf"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Secure your WebApp with Login and Logout
This example is a very simple Spring Boot application.

# Maven Dependencies
At first add the following dependencies to your project. [Spring Initializr](http://start.spring.io/) is recommended when you create a new project.

    <parent>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-parent</artifactId>
        <version>1.5.1.RELEASE</version>
        <relativePath/>
    </parent>

    <dependencies>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-web</artifactId>
        </dependency>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-security</artifactId>
        </dependency>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-thymeleaf</artifactId>
        </dependency>
        <dependency>
            <groupId>org.thymeleaf.extras</groupId>
            <artifactId>thymeleaf-extras-springsecurity4</artifactId>
        </dependency>
    </dependencies>

# Create your WebApp
Create a web application with websites and controller. For example this very small webapp with just one page (index.html) and an entry for the login page.

    @Configuration
    public class MvcConfig extends WebMvcConfigurerAdapter{
        
        @Override
        public void addViewControllers(ViewControllerRegistry registry) {
            registry.addRedirectViewController("/", "index");
            registry.addViewController("/index").setViewName("index");
            registry.addViewController("/login").setViewName("login");
        }
    }

# Secure your WebApp
Configure Spring Security to secure your webapp. Eg. allow any request by authenticated users only. Allow static resources like js and css, otherwise they won't be loaded for non-authenticated users. Exclude the login & logout page from this rule and create a test user:  

    @Configuration
    @EnableWebSecurity
    public class WebSecurityConfig extends WebSecurityConfigurerAdapter {
    
        @Override
        protected void configure(HttpSecurity http) throws Exception {
            http.authorizeRequests()
                    .antMatchers("/css/*.css", "/js/*.js").permitAll()
                    .anyRequest().authenticated()
                    .and()
                    .formLogin()
                    .loginPage("/login")
                    .permitAll()
                    .and()
                    .logout()
                    .permitAll();
        }

        @Autowired 
        public void configureGlobal(AuthenticationManagerBuilder auth) throws Exception { 
            auth.inMemoryAuthentication() 
                    .withUser("user").password("password").roles("USER"); 
        } 
    }

# Create the login page
The login page needs to have a form that makes a post request to "/login":

    <!DOCTYPE html>
    <html xmlns="http://www.w3.org/1999/xhtml"
          xmlns:th="http://www.thymeleaf.org">
    
        <head>
            <title>Login</title>
            <link th:href="@{/css/stylesheet.css}" rel="stylesheet" type="text/css"/>
            <script type="text/javascript" th:src="@{/js/login.js}"></script>
        </head>
        <body>
            <!-- show notification on error -->
            <div th:if="${param.error}">
                Invalid username or password.
            </div>
    
            <!-- show notification of logout -->
            <div th:if="${param.logout}">
                You have been logged out.
            </div>
    
            <!-- login form -->
            <div>
                <form th:action="@{/login}" method="post">
                    <h2 >Please sign in</h2>
                    <label >User Name</label>
                    <input type="text" name="username" th:required="required" th:autofocus="autofocus"/>
                    <br/>
                    
                    <label>Password</label>
                    <input type="password" name="password" th:required="required" />
                    <br/>
                   
                    <input type="submit" value="Sign In"/>
                </form>
            </div>
        </body>
    </html>

When the user enters the wrong username/password the error parameter is set. When the user logs out the logout parameter is set. This is used to show the corresponding messages.

# Access user properties

After a successful login the user is directed to the **index.html**. The [Spring Security Dialect](https://github.com/thymeleaf/thymeleaf-extras-springsecurity) allows us to access the user properties like his username:  

    <!DOCTYPE html>
    <html xmlns="http://www.w3.org/1999/xhtml"
          xmlns:th="http://www.thymeleaf.org">
        <head>
            <title>Index</title>
        </head>
        <body>
            <div>
                <h3>Welcome <span th:text="${#authentication.name}"/></h3>
                <form th:action="@{/logout}" method="post"> 
                    <input type="submit" value="Logout"/> 
                </form>
            </div>
        </body>
    </html>

A logout is achieved via post request to "/logout"

## Displaying something for authenticated users only
    <div sec:authorize="isAuthenticated()">
        This text is displayed for authenticated users.    
    </div>

## Display username

You can show username for autenticated users
   

    <div sec:authorize="isAuthenticated()">
        Welcome, <span sec:authentication="name">Username</span>     
    </div>

   

 

## Display different content to different roles
The sec:authorize attribute renders its content when the attribute expression is evaluated to true

    <div sec:authorize="hasRole('ROLE_ADMIN')">
        Content for administrators
    </div>
    <div sec:authorize="hasRole('ROLE_USER')">
        Content for users
    </div>

The sec:authentication attribute is used to print logged user roles:

    Roles: <span sec:authentication="principal.authorities">ROLE_USER, ROLE_ADMIN</span>

