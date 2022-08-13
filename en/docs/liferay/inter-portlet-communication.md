---
title: "Inter portlet communication"
slug: "inter-portlet-communication"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

This manual contains the various ways in which portlet can co-ordinate or communicate amongst each other and the various scenarios for which a particular approach is used.

References:

 1. [Public render param][1]
 2. [JSR 286 specs][2]
 3. [Portlet session][3]


  [1]: https://blogs.oracle.com/deepakg/entry/jsr286_public_render_parameter_feature
  [2]: https://www.google.co.in/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=0ahUKEwiMjJudz4nRAhUE7hoKHVN1C9oQFggcMAA&url=https%3A%2F%2Fwiki.jasig.org%2Fdownload%2Fattachments%2F25002428%2Fportlet-2-spec.pdf&usg=AFQjCNEDhqnvuPOfFdRgn8KV1fq_RRyjvQ
  [3]: http://proliferay.com/liferay-portlet-session-attribute/

## Using Public render parameter
This approach was introduced in JSR 286.

In JSR 168,render parameters set in *processAction* of a portlet were available only in that portlet.With the Public Render Parameters feature, the render parameters set in the *processAction* of one portlet will be available in render of other portlets also.In order to configure this,for all the portlets supporting this:

Add `<supported-public-render-parameter>` tag ,just before the portlet tag ends in `portlet.xml`
    

    <security-role-ref>
        <role-name>user</role-name>
    </security-role-ref>
    <supported-public-render-parameter>{param-name}</supported-public-render-parameter>
    </portlet>

 Add `<public-render-parameter>` tag just before the `<portlet-app>` tag ends

      <public-render-parameter>
        <identifier>{param-name}</identifier>
        <qname xmlns:x="localhost">x:{param-name}</qname>
      </public-render-parameter>
    </portlet-app>

In the `processAction` method,the param value needs to be set in the response

    res.setRenderParameter({param-name},{param-value});

Post we are done with configuring this for all the required portlet,after executing the action phase of the concerned portlet,the param should be available in render phase for all supporting portlets on the page,irrespective of being part of same or different application(war).

## Using Portlet session
This is one approach which has been there since JSR 168.It allows us to share attributes using portlet session.A portlet session can have different types of scopes:

 1. Portlet scope(attributes available only within portlet)
 2. Application scope(attributes available within whole application[war])


In order to use this approach,we do not need to make any entries in portlet configuration,as portlet session is readily available in portlet request:

    PortletSession session = renderRequest.getPortletSession();
    session.setAttribute("attribute-name","attribute-value", PortletSession.APPLICATION_SCOPE);

or

    PortletSession session = renderRequest.getPortletSession();
    session.setAttribute("attribute-name","attribute-value", PortletSession.PORTLET_SCOPE);

The attribute can only be retrieved from the respective scope only.Like for attribute set in portlet scope,we need to fetch it using

    PortletSession session = renderRequest.getPortletSession();
    String attributeValue = (String) session.getAttribute("attribute-name", PortletSession.PORTLET_SCOPE);

The major limitation of this approach is lack of sharing among other portlet,outside of application scope.In order to overcome this,there is liferay specific approach to add `<private-session-attributes`> to `liferay-portlet.xml`

        <private-session-attributes>false</private-session-attributes>
        <header-portlet-css>/css/main.css</header-portlet-css>
        <footer-portlet-javascript>/js/main.js</footer-portlet-javascript>
        <css-class-wrapper>{portlet-name}</css-class-wrapper>
    </portlet>

for all portlets,where the attributes are set and retrieved.



## Using eventing feature
The eventing mechanism is an extended version of the public render param,with additonal feature to pass custom objects to other portlets,but with an overhead of event phase.

To achieve this,this mechanism consists of

 1. Publisher portlet
 2. Processor(consumer) portlet,where both may be part of different portlet applications.

To start with,

Add `<supported-publishing-event>` tag to the publisher portlet in `portlet.xml`


        <security-role-ref>
            <role-name>user</role-name>
        </security-role-ref>
        <supported-publishing-event>
             <qname xmlns:x="http:sun.com/events">x:Employee</qname>
        </supported-publishing-event>
      </portlet>
Add `<supported-processing-event>` tag to the processor portlet in `portlet.xml`


    <security-role-ref>
            <role-name>user</role-name>
        </security-role-ref>
        <supported-processing-event>
            <qname xmlns:x="http:sun.com/events">x:Employee</qname>
         </supported-processing-event>
    </portlet>
Add `<event-definition>`tag to both the portlets,defining event-name and type in `portlet.xml`

    <event-definition>   
      <qname xmlns:x="http:sun.com/events">x:Employee</qname>
      <value-type>com.sun.portal.portlet.users.Employee</value-type>
    </event-definition>
       </portlet-app>

Next we need to create class for the event type(in case of custom type)

    public class Employee implements Serializable {
      public Employee() {
      }
      private String name;
      private int userId; 

      public String getName() {
        return name;
      }

      public void setName(String name) {
        this.name = name;
      }

      public int getUserId() {
        return userId;
      }
      public void setUserId(int id)
      {
        this.userId = id;
      }
  }

Now,in the publisher portlet,the event needs to be published in the action phase

        QName qname = new QName("http:sun.com/events" , "Employee");
        Employee emp = new Employee();
        emp.setName("Rahul");
        emp.setUserId(4567);
        res.setEvent(qname, emp);

Post we have published the event,it needs to be processed by the publisher portlet in the event phase.

The event phase was introduced in JSR 286 and is executed before render phase of the portlet,when applicable

    @ProcessEvent(qname = "{http:sun.com/events}Employee")
    public void processEvent(EventRequest request, EventResponse response) {

        Event event = request.getEvent();
        if(event.getName().equals("Employee")){
          Employee payload = (Employee)event.getValue();
          response.setRenderParameter("EmpName",
          payload.getName());
        }

    }

which can then be retrieved from the render parameter via render request.

