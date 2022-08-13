---
title : xss Tutorial
slug : xss-tutorial
weight : 9995
draft : false
images : []
type : docs
---

**Overview**
Cross-Site Scripting, commonly referred to as XSS, is a type of web application injection attack in which malicious scripts are injected into trusted websites.

XSS attacks occur when an attacker takes advantage of, or "exploits," a flaw in a web application to send the attacker's payload to the client's browser. These flaws are typically encountered when a web application sends user-input to the browser without validating or encoding it beforehand.

An XSS payload executes within the domain of the "trusted site" and has the potential of accessing that website's cookies, modifying the page's DOM and even abusing the client's browser or extensions.

**XSS Types**

Though the end result is the same for all XSS attacks (an attacker controlled payload in the server's response), there are three different types of XSS vulnerabilities.

 - **Stored XSS** is an attack where the XSS payload is permanently _stored_ on the target website, such as in a database. When a client (e.g. victim) loads a page such as a forum board or comment section that loads the payload, it will execute in their browser.
 - **Reflected XSS** is an attack where the XSS payload is sent with the request to the server and _reflected_ back in the response. These attacks can be triggered from clicking a crafted link, submitting a form, or many other delivery mechanisms.
 - **Client Side XSS**, also referred to as **DOM Based XSS**, is an attack that takes place solely in the client's browser (i.e. it's not sent by the response from the server) by manipulating the DOM's environment to force existing trusted scripts on the page to execute the XSS payload.

