---
title: "Getting started with liferay"
slug: "getting-started-with-liferay"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## A basic installation for development and tests
Running the latest Liferay CE is straightforward:

1. Go to https://www.liferay.com/downloads.
2. Choose a bundle among the ones listed. For beginners, the Tomcat bundle is a good choice. Click in "Download."

[![Where to choose a bundle.][1]][1]

3. Unzip the download package whenever you find fit. The unzipped directory will be the `LIFERAY_HOME` directory.
4. To start Liferay, just run the script `LIFERAY_HOME/tomcat-x.xx.xx/bin/startup.sh`; only on Windows environments run the script `LIFERAY_HOME\tomcat-x.xx.xx\bin\startup.bat`.
5. By default, once Liferay is up, a browser will open its local URL (http://localhost:8080/).
6. To log in, use the email `test@liferay.com`, and the password `test`.
7. To stop Liferay, just run the script `LIFERAY_HOME/tomcat-x.xx.xx/bin/shutdown.sh`; only on Windows environments run the script `LIFERAY_HOME\tomcat-x.xx.xx\bin\shutdown.bat`.

With these steps, you will have Liferay up and running in a "demo" mode. Liferay will use an Hypersonic DB by default, but it is unfit for production. Also, `test@liferay.com` is an administrator account with a default password, so it should be changed eventually. Yet, these steps are good to get some idea on how Liferay looks like and works.

  [1]: http://i.stack.imgur.com/B8mdi.png

