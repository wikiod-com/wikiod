---
title: "SOAP Server"
slug: "soap-server"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Syntax
- [addFunction()][1] //Register one (or more) function into SOAP request handler
- [addSoapHeader()][2] //Add a SOAP header to the response
- [fault()][3] //Issue SoapServer fault indicating an error
- [getFunctions()][4] //Returns list of functions
- [handle()][5] //Handles a SOAP request
- [setClass()][6] //Sets the class which handles SOAP requests
- [setObject()][7] //Sets the object which will be used to handle SOAP requests
- [setPersistence()][8] //Sets SoapServer persistence mode


  [1]: http://php.net/manual/en/soapserver.addfunction.php
  [2]: http://php.net/manual/en/soapserver.addsoapheader.php
  [3]: http://php.net/manual/en/soapserver.fault.php
  [4]: http://php.net/manual/en/soapserver.getfunctions.php
  [5]: http://php.net/manual/en/soapserver.handle.php
  [6]: http://php.net/manual/en/soapserver.setclass.php
  [7]: http://php.net/manual/en/soapserver.setobject.php
  [8]: http://php.net/manual/en/soapserver.setpersistence.php

## Basic SOAP Server
    function test($x)
    {
        return $x;
    }
    
    $server = new SoapServer(null, array('uri' => "http://test-uri/"));
    $server->addFunction("test");
    $server->handle();

