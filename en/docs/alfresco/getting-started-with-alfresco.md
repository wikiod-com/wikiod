---
title: "Getting started with alfresco"
slug: "getting-started-with-alfresco"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
**Development and Evaluation Installations**

Alfresco provides a number of different installers for different operating systems and platforms. However, these installers are not recommended for production environments.

https://www.alfresco.com/alfresco-community-download

https://sourceforge.net/projects/alfresco - more Alfresco instalation files, and separate modules a avaiable also there

**Production Installations**

You can install Alfresco on Ubuntu distributions for by using Alfresco Ubuntu Install script from Loftux AB. 

While you are installing using the script, you can select between Alfresco Community Edition (No commercial support) and LXCommunity ECM (With commercial support).

https://loftux.com/en/products-and-add-ons/alfresco-utilities

**Addons**

Additionally you can extend you Alfresco with community created addons- they are avaiable on https://addons.alfresco.com

## About Alfresco
Alfresco is ECM (enterprise content management) system, based on java frameworks and javascript. Alfresco (as a "repository" is the base core of the Alfresco as a product).

It provides eg. encrypted content store, specific permissions settings, content types with specific properties (like name, date of creation, also custom properties can be included).

Alfresco also provides Workflow processes- separately or with documents, workflows are specified by diagram flow (Activiti and jbpm engine is supported; jbpm deprecated).

Product also partly supports Sharepoint (via Alfresco office services- "aos", CIFS, and other features. Key features are:
- Content storage 
- Content retrieval 
- Content modeling 
- Query interface
- Access control
- Audit
- Versioning

User content is wrapped with the security layer- binary content is saved on the disk, but the structure and metadata are saved in the database.

This part- "repository" of the Alfresco as product provides REST API for the extensions like Alfresco Share.

Alfresco Installation Link For Windows

http://docs.alfresco.com/community/tasks/simpleinstall-community-win.html

For Linux

http://docs.alfresco.com/community/tasks/simpleinstall-community-lin.html

Brief Description about Installation

http://docs.alfresco.com/community/concepts/install-community-intro.html

