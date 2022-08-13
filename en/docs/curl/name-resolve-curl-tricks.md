---
title: "Name resolve curl tricks"
slug: "name-resolve-curl-tricks"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Editing the hosts file
The easiest way to connect via curl to a different server is to alter the hosts file on your machine.

On Linux and Unix systems, the hosts file is located in **/etc/hosts**, while on Windows systems it will be located in **c:\windows\system32\drivers\etc\hosts**.

Once you open the file with a text editor of your choice, add

`1.2.3.4 domain.tld www.domain.tld`

This is basically the IP of the server you would like to resolve the domain to followed by the domain and a www version of the domain.

Curl will then resolve to this domain until the added line in the hosts file is removed.

The limitation in this example is that editing the hosts file often requires admin access and also, it affects all applications connected to the domain at the same time.

## Providing custom IP address for a name
The most effective way to resolve curl to a different server is to use the `--resolve` option. This option will insert the address into curl's DNS cache, so it will effectively make curl believe that's the address it got when it resolved the name.
                                                                                                                                                                                                                                                        Like so:

`curl --resolve eaxmple.com:80:1.2.3.4 http://example.com/`

In the above example, firstly we specify the domain (example.com), then we ask it to connect on port 80 to the IP 1.2.3.4. Depending on the protocol used and the server's configuration, the port can vary. For HTTP the port is 80 and for HTTPS, the port is 443.

It is important to note here that the `--resolve` option will send SNI for the name in the URL. This means that when connecting to the server via HTTPS, curl will verify the server's response to make sure it servers for the name in the URL. In other words, it will ensure there is an SSL on the server installed for the domain.

## Change the "Host:" header
The "Host:" header is a normal way an HTTP client tells the HTTP server which server it speaks to. By passing custom modified "Host:" header you can have the server respond with the content of the site, even if you didn't actually connect to the host name.

For example, if you have a site on your localhost and you wish to have curl ask for its index page, the command is:

`curl -H "Host: example.com" http://localhost/`

The main disadvantage of modifying the "Host:" header is that curl will only extract the SNI name to send from the given URL. In other words, the "Host:" header modification is not enough when communication with a server via HTTPS.

