---
title: "Logging into websites with Jsoup"
slug: "logging-into-websites-with-jsoup"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## A more comprehensive authentication POST request with Jsoup
Most websites require a much more complicated process than the one demonstrated above.

Common steps for logging into a website are:

 1. Get the unique `cookie` from the initial login form.
 2. Inspect the login form to see what the destination url is for the authentication request
 3. Parse the login form to check for any `security token` that needs to be sent along with username and password.
 4. Send the request.

Below is an example request that will log you into the [GitHub][1] website

    // # Constants used in this example
    final String USER_AGENT = "Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36"; 
    final String LOGIN_FORM_URL = "https://github.com/login";  
    final String LOGIN_ACTION_URL = "https://github.com/session";  
    final String USERNAME = "yourUsername";  
    final String PASSWORD = "yourPassword";  

    // # Go to login page and grab cookies sent by server
    Connection.Response loginForm = Jsoup.connect(LOGIN_FORM_URL)
                                         .method(Connection.Method.GET)
                                         .userAgent(USER_AGENT)
                                         .execute();  
    Document loginDoc = loginForm.parse(); // this is the document containing response html
    HashMap<String, String> cookies = new HashMap<>(loginForm.cookies()); // save the cookies to be passed on to next request  

    // # Prepare login credentials 
    String authToken = loginDoc.select("#login > form > div:nth-child(1) > input[type=\"hidden\"]:nth-child(2)")  
                               .first()  
                               .attr("value");  

    HashMap<String, String> formData = new HashMap<>();
    formData.put("commit", "Sign in");  
    formData.put("utf8", "e2 9c 93");  
    formData.put("login", USERNAME);  
    formData.put("password", PASSWORD);  
    formData.put("authenticity_token", authToken);  

    // # Now send the form for login
    Connection.Response homePage = Jsoup.connect(LOGIN_ACTION_URL)  
         .cookies(cookies)  
         .data(formData)  
         .method(Connection.Method.POST)  
         .userAgent(USER_AGENT)  
         .execute();

    System.out.println(homePage.parse().html());  

  [1]: https://github.com/

## A simple authentication POST request with Jsoup
A simple POST request with authentication data is demonstrated below, note that the `username` and `password` field will vary depending on the website:

    final String USER_AGENT = "Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36";
    Connection.Response loginResponse = Jsoup.connect("yourWebsite.com/loginUrl")
                                            .userAgent(USER_AGENT)
                                            .data("username", "yourUsername")
                                            .data("password", "yourPassword")
                                            .method(Method.POST)
                                            .execute();

## Logging with FormElement
In this example, we will log into the [GitHub][1] website by using the [FormElement][2] class.

    // # Constants used in this example
    final String USER_AGENT = "Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36"; 
    final String LOGIN_FORM_URL = "https://github.com/login";
    final String USERNAME = "yourUsername";  
    final String PASSWORD = "yourPassword";  

    // # Go to login page
    Connection.Response loginFormResponse = Jsoup.connect(LOGIN_FORM_URL)
                                                 .method(Connection.Method.GET)
                                                 .userAgent(USER_AGENT)
                                                 .execute();  

    // # Fill the login form
    // ## Find the form first...
    FormElement loginForm = (FormElement)loginFormResponse.parse()
                                             .select("div#login > form").first();
    checkElement("Login Form", loginForm);

    // ## ... then "type" the username ...
    Element loginField = loginForm.select("#login_field").first();
    checkElement("Login Field", loginField);
    loginField.val(USERNAME);

    // ## ... and "type" the password
    Element passwordField = loginForm.select("#password").first();
    checkElement("Password Field", passwordField);
    passwordField.val(PASSWORD);        


    // # Now send the form for login
    Connection.Response loginActionResponse = loginForm.submit()
             .cookies(loginFormResponse.cookies())
             .userAgent(USER_AGENT)  
             .execute();

    System.out.println(loginActionResponse.parse().html());

    public static void checkElement(String name, Element elem) {
        if (elem == null) {
            throw new RuntimeException("Unable to find " + name);
        }
    }

All the form data is handled by the FormElement class for us (even the form method detection). A ready made [Connection][3] is built when invoking the [FormElement#submit][4] method. All we have to do is to complete this connection with addional headers (cookies, user-agent etc) and execute it.


  [1]: https://github.com/
  [2]: https://jsoup.org/apidocs/org/jsoup/nodes/FormElement.html
  [3]: https://jsoup.org/apidocs/org/jsoup/Connection.html
  [4]: https://jsoup.org/apidocs/org/jsoup/nodes/FormElement.html#submit--

