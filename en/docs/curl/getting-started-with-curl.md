---
title: "Getting started with curl"
slug: "getting-started-with-curl"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Using curl through the command line
Show curl version:

    curl --version

GET a remote resource and have it displayed in the terminal:

    curl http://stackoverflow.com

GET a remote resource and save it in a local file:

    curl -o file https://stackoverflow.com

Add headers to response:

    curl -i http://stackoverflow.com

Output only headers:

    curl -I http://stackoverflow.com


## Transfer data using curl
cURL is the name of the project which depicts: 'Client for URLs' and also be called as **Client URL Request Library**

it combines two separate packages: curl and libcurl. 

1) `curl` is a command line tool used to get documents/files from or send documents to a server, using any of the supported protocols: DICT, FILE, FTP, FTPS, Gopher, HTTP, HTTPS, IMAP, IMAPS, LDAP, LDAPS, POP3, POP3S, RTMP, RTSP, SCP, SFTP, SMB, SMTP, SMTPS, Telnet and TFTP.

2) `libcurl` is the underlying library curl uses to do the actual networking and transfer work. libcurl is used by thousands of services, applications and devices and very often through one of the "language bindings" that allows programmers of higher level languages to access its powers.

## Using curl in PHP to fetch data
    <?php
     
        $ch = curl_init(); //curl handler init

        curl_setopt($ch,CURLOPT_URL,"http://www.google.com/search?q=curl");
        curl_setopt($ch,CURLOPT_RETURNTRANSFER,true);// set optional params
        curl_setopt($ch,CURLOPT_HEADER, false); 
     
        $result=curl_exec($ch);
     
        curl_close($ch);
     
        echo $result;
    ?>

## Use the libcurl easy C API to get a remote resource
    #include <stdio.h>
    #include <curl/curl.h>
    
    int main(void)
    {
      CURL *curl;
      CURLcode res;
    
      curl = curl_easy_init();
      if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

        /* example.com is redirected, so we tell libcurl to follow redirection */
        curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    
        /* Perform the request, res will get the return code */
        res = curl_easy_perform(curl);
        /* Check for errors */
        if(res != CURLE_OK)
          fprintf(stderr, "curl_easy_perform() failed: %s\n",
                  curl_easy_strerror(res));
    
        /* always cleanup */
        curl_easy_cleanup(curl);
      }
      return 0;
    }



