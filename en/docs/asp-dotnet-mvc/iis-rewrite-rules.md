---
title: "IIS Rewrite Rules"
slug: "iis-rewrite-rules"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Force HTTPS using Rewrite rule
This example shows how you can use IIS Rewrite rules to force HTTPS by making all HTTP requests return a 301 (Permanent) Redirect to the HTTPS page.

This is usually better than using the `[RequireHttps]` attribute because the attribute uses a 302 redirect, and being in the MVC pipeline it is much slower than doing it at the IIS level.

       <rewrite xdt:Transform="Insert">
          <rules>
            <rule name="Enforce HTTPS WWW" stopProcessing="true">
              <match url=".*" />
              <conditions logicalGrouping="MatchAll" trackAllCaptures="true">
                <add input="{HTTP_HOST}" pattern="^(?!www)(.*)"/>
                <add input="{URL}" pattern="^(.*)"/>
                <!-- {URL} Gives the base portion of the URL, without any querystring or extra path information, for example, "/vdir/default.asp". -->
              </conditions>
              <action type="Redirect" url="https://www.{C:1}{C:2}" appendQueryString="true" redirectType="Permanent" />
            </rule>
          </rules>
        </rewrite>

