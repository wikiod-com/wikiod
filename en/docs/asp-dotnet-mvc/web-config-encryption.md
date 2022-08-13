---
title: "Web.config Encryption"
slug: "webconfig-encryption"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## How to protect your Web.config file
It is a good practice to encrypt your Web.config file if you have sensitive information there, for example a connection string with password.

With the [ASP.NET IIS Registration tool][1] (Aspnet_regiis.exe) you can easily encrypt specific sections of Web.config file. A command with elevated privileges is required.

Example using [DataProtectionConfigurationProvider][2]. This provider uses [DPAPI][3] to encrypt and decrypt data:
<pre>aspnet_regiis.exe -pef "connectionStrings" c:\inetpub\YourWebApp -prov "DataProtectionConfigurationProvider"</pre>

Example using [RSAProtectedConfigurationProvider][4]: 
<pre>aspnet_regiis.exe -pef "connectionStrings" c:\inetpub\YourWebApp -prov "RSAProtectedConfigurationProvider"</pre>

If you do not specify the -prov parameter it uses **RSAProtectedConfigurationProvider** as default. This provider is recommended for Web Farm scenarios.

To get **connectionStrings** section back to clear text:
<pre>aspnet_regiis.exe -pdf "connectionStrings" c:\inetpub\YourWebApp</pre>

More information about the aspnet_regiis.exe is avaiable on [MSDN][1].


  [1]: https://msdn.microsoft.com/en-us/library/k6h9cz8h.aspx
  [2]: https://msdn.microsoft.com/en-us/library/ff647398.aspx
  [3]: https://msdn.microsoft.com/en-us/library/ms995355.aspx
  [4]: https://msdn.microsoft.com/en-us/library/ms998283.aspx

