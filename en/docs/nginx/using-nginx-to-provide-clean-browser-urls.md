---
title: "Using nginx to provide clean browser URLs"
slug: "using-nginx-to-provide-clean-browser-urls"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Redirect vs reverse proxy
Professionally made web applications don't expose the internal details of the server environment to the user. When you place an order at your retailer, you don't see (or have to type) https://mydealer.com:8443/Dealerapp/entryPage.html, but just mydealer.com, although the app server needs all the details given in the longer URL. This can be achieved via:

    if ($scheme = http) {
        return 301 https://$server_name$request_uri;
    }

This does a **redirect**. Even if the client didn't request a secure connection, the browser is at once redirected to it. In countries having strict data privacy laws, you might even be obliged to do this for any commercial site. The redirect way is taken because here the browser needs to know about the secure connection, otherwise it wouldn't negotiate with the server to make it secure.

    location /app/ {
        proxy_pass https://mydealer.com:8443/Dealerapp/entryPage.html;
    }

This is a **reverse proxy**. It is transparent to the browser. So you can completely hide from the end user whether you have one or more app servers, on which ports they listen, or how their applications are named. Otherwise, if your datacenter needs to move the app to another server that listens on 8543, all bookmarks of your clients would become invalid. This example also directs the user to your entry page. This could be omitted if you name the entry page index.html. But maybe you have several entry pages within an app that the user might want to bookmark, so you are more flexible if you're not bound to index.html.

I postfixed the company URL with /app. This could be omitted if your domain only serves this app. In case that in addition to the app, there also is some static content, like your company description, you may want to have the simple URL for that.

This proxy only works for the entry page. I usually need two more URL schemes, one for static content of the web app, like JavaScript, CSS, and images. I put all those in a folder structure below a folder called serverapp, and write the following proxy:

    location /app/serverapp/ {
        proxy_pass https://mydealer.com:8443/Dealerapp/serverapp/;
    }

And another path for REST services; here the rest URL path doesn't match a folder, but a path of a jax-rs REST service:

    location /rest/ {
        proxy_pass https://mydealer.com:8443/Dealerapp/rest/;
    }

One more step, rarely mentioned anywhere, needs to be taken. The app server runs under the URL path /Dealerapp, so it will issue a session cookie having property path=Dealerapp. The browser doesn't know this path, and due to its Same Origin Policy will ignore the cookie. We could convince it via Cross-Origin Resource Sharing to allow this, but it's probably easier to modify the cookie path by setting the path to /, writing something like

    <session-config>
        <session-timeout>720</session-timeout>
        <cookie-config>
            <name>SZSESSION</name>
            <path>/</path>
            <http-only>true</http-only>
            <secure>true</secure>
        </cookie-config>
    </session-config>

to our web.xml.


