---
title: "Externalizing Text in Thymeleaf"
slug: "externalizing-text-in-thymeleaf"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Localization
 1. Create files for your messages
     
   

     messages.properties   
     messages_en.properties
     messages_fr.properties   
     ...

2. Write messages in this files like this

 `header.label.title=Title`

 3. Configure path to this files (in this case in folder D:/project/messages) in application properties like:

    
    messages.basename.path=D:/project/messages/messages

 4. Configure MessageSource



    @Value("${messages.basename.path}")
    private String messagesBasename;

    @Bean
    public MessageSource messageSource() {
           ReloadableResourceBundleMessageSource messageSource = new ReloadableResourceBundleMessageSource();
           messageSource.setFallbackToSystemLocale(false);
           messageSource.setBasenames("file:" + messagesBasename);
           return messageSource;
       }

5. Use messages on pages

  `<p th:text="#{header.label.title}">Title</p>`

## Localization messages with parameters
Write message in messages.properties

    welcome.message=Hello, {0}!

Replace {0} with the user name inside thymeleaf tag

    <h3 th:text="#{welcome.message(${some.variable})}">Hello, Placeholder</h3>

