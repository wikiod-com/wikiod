---
title: "Getting started with Big DataHadoop Security"
slug: "getting-started-with-big-datahadoop-security"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

We can secure the data in Hadoop using different methods. Each method has its own advantages. We can also combine more than one method for better result. This topic covers the advantages & limitations of each method



**1. Kerberos is a network authentication protocol:**

  **a. Advantage:** Authenticate users at the entry level.

  **b. Limitation:** Kerberos prevents unauthorized user access to the environment. But after login, it will not provide detailed level authentications like table, column, folder, file level, etc

**2. Apache Sentry is a system for enforcing fine grained role bas**

 **a. Advantage:** Application level authentications like Hive, Impala, Solr, etc. It can control access on DB, table, column level for a particular user/group.

 **b. Limitation:** It cannot control the HDFS folders which are underlined behind applications like Hive, Impala, etc.
Ex: Hive table prod.table1 stored in /user/hive/warehouse/prod.db/table1. The sentry role setup in Hue can control only table/column access in Hue but It is possible that user can manage to access folders directly in HDFS

  **c. Limitation:** HDFS folders which are not related to Hive, Impala, etc will not be controlled

**3. An access control list (ACL) is a list of access control entries (ACE).
Each ACE in an ACL identifies a trustee and specifies the access rights allowed, denied, or audited for that trustee**

**a. Advantage:** Folder level access is possible by users using 

**4. HDFS Encryption implements transparent, end-to-end encryption of data read from and written to HDFS**

**a. Advantage:** Encrypt the data will provide additional level security. In General, Data encryption is required by a number of different government, financial, and regulatory entities




## Example for ACL
    hadoop fs -setfacl

