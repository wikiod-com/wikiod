---
title: "Getting started with thymeleaf"
slug: "getting-started-with-thymeleaf"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Configuration
 To get started with Thymeleaf visit [official download page](http://www.thymeleaf.org/download.html). 

**Maven dependency**

    <dependency> 
      <groupId>org.thymeleaf</groupId>
      <artifactId>thymeleaf</artifactId> 
      <version>3.0.1.RELEASE</version> 
    </dependency>

**Gradle dependency** 

    compile group: 'org.thymeleaf', name: 'thymeleaf', version: '3.0.1.RELEASE'

**Example configuration**

Starting from version 3.0, Thymeleaf supports only Java config. 

    public ViewResolver viewResolver() {
        ThymeleafViewResolver resolver = new ThymeleafViewResolver();
        resolver.setTemplateEngine(templateEngine());
        resolver.setCharacterEncoding("UTF-8");
        resolver.setContentType("text/html; charset=UTF-8");
        return resolver;
    }

In `viewResolver()` method you can setup e.g. encoding and content type for views. [more information][1]


    public TemplateEngine templateEngine() {
        SpringTemplateEngine engine = new SpringTemplateEngine();
        engine.setTemplateResolver(templateResolver());
        return engine;
    }

In `templateEngine()`, you can add custom dialects. For example to add Spring Security dialect you can do this like this `engine.addDialect(new SpringSecurityDialect());`


    public ITemplateResolver templateResolver() {
        SpringResourceTemplateResolver resolver = new SpringResourceTemplateResolver();
        resolver.setApplicationContext(applicationContext);
        resolver.setPrefix("/views/");
        resolver.setSuffix(".html");
        resolver.setTemplateMode(TemplateMode.HTML);
        resolver.setCharacterEncoding("UTF-8");
        return resolver;
    }

Look at setter for prefix and suffix in `templateResolver()` method. It tells Thymeleaf that, every time controller will return view, Thymeleaf will look these names that html in `webapp/views/` directory and append `.html` suffix for you. 

**Example**

    @RequestMapping(value = "/")
    public String homePage() {
        return "foo/my-index";
    }

Thymeleaf will be looking html named `my-index.html` in `webapp/views/foo/` directory. According to example configuration above. 

  [1]: http://www.thymeleaf.org/apidocs/thymeleaf-spring4/2.0.20/org/thymeleaf/spring4/view/ThymeleafViewResolver.html

## Using checkboxes
Example method in controller

    @RequestMapping(value = "/test")
    public String showCheckbox(Model model) {
        boolean myBooleanVariable = false;
        model.addAttribute("myBooleanVariable", myBooleanVariable);
        return "sample-checkbox";
    }

View: sample-checkbox.html

    <input 
          type="checkbox" 
          name="myBooleanVariable" 
          th:checked="${myBooleanVariable}"/>

Do not use `th:name` for checboxes, just `name`

## Form Submission
**Form object**

    package formSubmission;

    public class Person {

        private String name;
        private int age;

        public String getName() {
            return name;
        }
        public void setName(String name) {
            this.name= name;
        }
        public int getAge() {
            return age;
        }
        public void setAge(int age) {
            this.age = age;
        }

    }

**Controller**

    package formSubmission;

    import org.springframework.stereotype.Controller;
    import org.springframework.ui.Model;
    import org.springframework.web.bind.annotation.GetMapping;
    import org.springframework.web.bind.annotation.ModelAttribute;
    import org.springframework.web.bind.annotation.PostMapping;

    @Controller
    public class FriendsController {

        @GetMapping("/friends")
        public String friendForm(Model model) {
            model.addAttribute("personForm", new Person());
            return "friendsForm";
        }

        @PostMapping("/friends")
        public String submissionResult(@ModelAttribute("personForm") Person person) {
            return "result";
        }

    }

**friendsForm.html**

    <!DOCTYPE HTML>
    <html xmlns:th="http://www.thymeleaf.org">
    <head>
        <title>Friend form</title>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    </head>
    <body>
        <h1>Friend Form</h1>
        <form th:action="@{/friends}" th:object="${personForm}" method="post">
            <p>Name: <input type="text" th:field="*{name}"/></p>
            <p>Age: <input type="number" th:field="*{age}"/></p>
            <p><input type="submit" value="Submit"/></p>
        </form>
    </body>
    </html>

**result.html**

    <!DOCTYPE HTML>
    <html xmlns:th="http://www.thymeleaf.org">
    <head>
        <title>Submission result</title>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    </head>
    <body>
        <h1>th:text="'My friend ' + ${personForm.name} + ' is ' + ${personForm.age} + ' years old'"</h1>
    </body>
    </html>

## Ajax form submition with Jquery
To submit form via Ajax with Jquery :

        <div id="yourPanel" th:fragment="yourFragment">  
            <form id="yourForm" method="POST" 
                  th:action="@{/actions/postForm}"
                  th:object="${yourFormBean}">
            <div class="form-group">
                <label for="param1"></label>
                <input class="form-component" type="text" th:field="*{param1}" />
            </div>
            <div class="form-group">
                <label for="param2"></label>
                <input class="form-component" type="text" th:field="*{param2}" />
            </div>
            <div class="form-group">
                <label for="param3"></label>
                <input class="form-component" type="checkbox" th:field="*{param3}" />
            </div>

            <button type="submit" class="btn btn-success">Save</button>
            <a href='#' class="btn btn-default">Cancel</a>
        </form>
        </div>
    
    <script th:inline="javascript">
        /*<![CDATA[*/
        $(document).ready(function () {
            /*[+
             var postUrl = [[@{/actions/postForm(
             additionalParam=${#httpServletRequest.getParameter('additionalParam')}
             )}]]; 
             +]*/
            $("#yourForm").submit(function (e) {
                e.preventDefault();
                $.post(postUrl,
                        $(this).serialize(),
                        function (response) {
                            var isErr = 'hasError';
                            // when there are an error then show error
                            if (response.indexOf(isErr) > -1) {
                                $("#yourPanel").html(response);
                            } else {
                                var formData = $("#yourForm").serializeArray(),
                                        len = formData.length,
                                        urlEnd = '';
                                for (i = 0; i < len; i++) {
                                    urlEnd += formData[i].name + '=' + encodeURIComponent(formData[i].value) + '&';
                                }

                                /*[+
                                 var urlReplacement = [[@{/another/page(
                                 additionalParam=${#httpServletRequest.getParameter('additionalParam')}
                                 )}]] + urlEnd;
                                 +]*/

                                window.location.replace(urlReplacement);
                            }
                        }
                );
                return false;
            });
        });
        /*]]>*/
    </script>

YourFormBean class :

    @lombok.Getter
    @lombok.Setter
    @lombok.NoArgsConstructor
    public class YourFormBean {
        private String param1;
        private String param2;
        private boolean param3;
    }

Controller code :

    @RequestMapping(value = "/actions/postForm", method = RequestMethod.POST) 
    public String saveForm(Model model, 
            @RequestParam("additionalParam") Integer additionalParam, 
            @Valid @ModelAttribute("yourFormBean") YourFormBean yourFormBean,
            BindingResult bindingResult,
            RedirectAttributes redirectAttributes) {
        if (bindingResult.hasErrors()) {
            model.addAttribute("hasError", true);
            return "your/template :: yourFragment";
        }
        redirectAttributes.addAttribute("additionalParam", additionalParam);
        
        return "redirect:/another/page";
    }

## Replacing fragments with ajax
If you want to replace parts of your website, ajax is an easy way to do it.

The **website.html** where you want to replace the content based on the selected value:

    <!DOCTYPE html>
    <html xmlns="http://www.w3.org/1999/xhtml"
      xmlns:th="http://www.thymeleaf.org">

        <head>
            <title>Index</title>
        </head>

        <body>
            <select id="selection">
                <option>Content 1</option>
                <option>Content 2</option>
            </select>

            <div id="replace_div">
                Content goes here
            </div>
    
            <!-- JQury from Google CDN -->
            <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>

            <script>
                $(document).ready(function () {

                    //call function when page is loaded
                    getContent();

                    //set on change listener
                    $('#selection').change(getContent);

                    function getContent() {

                        //create url to request fragment
                        var url = /content/;
                        if ($('#selection').val() === "Content 1") {
                            url = url + "content1";
                        } else {
                            url = url + "content2";
                        }
    
                        //load fragment and replace content
                        $('#replace_div').load(url);
                    }
                })
            </script>
        </body>
    </html>

And the **content.html** with the fragments you want to include based on the selected value:

    <!DOCTYPE html>
    <html xmlns="http://www.w3.org/1999/xhtml"
          xmlns:th="http://www.thymeleaf.org">
        <head>
        </head>
    
        <body>
            <div th:fragment="content1">
                This is Content 1
            </div>

            <div th:fragment="content2">
                This is Content 2
             </div>
        </body>
    </html>

Last but not least the Spring MVC **ContentController.java**:

    @Controller
    @RequestMapping("content")
    public class ContentController {
    
        @RequestMapping("")
        public String loadContent() {
            return "website";
        }

        @RequestMapping("content1")
        public String getContent1() {
            return "content :: content1";
        }

        @RequestMapping("content2")
        public String getContent2() {
            return "content :: content2";
        }
    }

