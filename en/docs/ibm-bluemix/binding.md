---
title: "Binding"
slug: "binding"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Accessing credentials through the VCAP_SERVICES environment variable
When you bind a service to your application credentials become available through the VCAP_SERVICES environment variable.

This environment variable contains JSON containing the credentials for all bound services.

**Example VCAP_SERVICES environment variable**

    {
       "push-reappt": [
          {
             "name": "Reappt from Push Technology",
             "label": "push_reappt",
             "plan": "reappt:pushtechnology:free",
             "credentials": {
                "principal": "service-binding-abcd1234",
                "credentials": "XYZlmnop456",
                "host": "sniffingitchyPythagoras.eu.bluemix.reappt.io",
                "port": 443
             }
          }
       ]
    }

You can then access these credentials through your application.

**Javascript**

In a Node application you could do the following:

    var reappt_credentials = JSON.parse(process.env.VCAP_SERVICES)["push-reappt"][0].credentials;
    
    diffusion.connect({
        host : reappt_credentials.host,
        principal : reappt_credentials.principal,
        credentials : reappt_credentials.credentials
    }).then(connected, error);

**Java**

In a Java application the same could be done as follows:

        private static final JsonParser PARSER = new JsonParser();
        private static final JsonObject VCAP_SERVICES = PARSER.parse(System.getenv("VCAP_SERVICES")).getAsJsonObject();

        private static final JsonObject REAPPT_CREDENTIALS = VCAP_SERVICES.getAsJsonArray("push-reappt").get(0)
                    .getAsJsonObject().getAsJsonObject("credentials");
        protected static final String HOST = REAPPT_CREDENTIALS.getAsJsonPrimitive("host").getAsString();
        protected static final String PRINCIPAL = REAPPT_CREDENTIALS.getAsJsonPrimitive("principal").getAsString();
        protected static final String CREDENTIALS = REAPPT_CREDENTIALS.getAsJsonPrimitive("credentials").getAsString();

