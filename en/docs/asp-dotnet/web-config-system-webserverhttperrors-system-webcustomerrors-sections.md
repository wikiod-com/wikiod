---
title: "web.config > system.webServerhttpErrors & system.webcustomErrors sections"
slug: "webconfig--systemwebserverhttperrors--systemwebcustomerrors-sections"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

CustomErrors are a legacy (backwards compatable) element, used by Visual Studio Development Server (aka. VSDS or Cassini).

httpErrors are the new element which is only used by IIS7.

## What is the difference between customErrors and httpErrors?
**Both are used to define error handling for a website, but different software refers to different config elements.**

customErrors are a legacy (backwards compatable) element, used by Visual Studio Development Server (aka. VSDS or Cassini).

httpErrors are the new element which is only used by IIS7.

This highlights the possible problem when developing ASP.NET websites while using VSDS instead of the local IIS.

Also, [refer to this post][1] by myself about how to handle error messages with IIS7, if you wish to have full control of the error output.

Summary:

 1. Developing in VSDS - use customErrors
 2. Publishing the site to IIS6 - use customErrors
 3. Publishing the site to IIS7 - use httpErrors.
 4. and if you develop with VSDS but publish to IIS7, then i guess u'll
    need both.

  [1]: https://serverfault.com/questions/123729/iis-is-overriding-my-response-content-if-i-manually-set-the-response-statuscode/124074#124074

