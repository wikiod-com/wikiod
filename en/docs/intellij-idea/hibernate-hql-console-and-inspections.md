---
title: "Hibernate HQL console and inspections"
slug: "hibernate-hql-console-and-inspections"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

Intellij IDEA supports HQL auto completion and running HQL queries on console.
This is how you enable that support.

## Configuring HQL Inspections

1. Go to File -> Project Structure -> Modules. 
2. Add new Hibernate module.
3. Right click on the desired module -> Add -> Hibernate.
4. Select the newly created Hibernate configuration option, and click the (+) sign in the right pane to create hibernate.cfg.xml file.
5. Go to File -> Project Structure -> Facets, and add new JPA. 
6. Select the newly created JPA configuration option, and click the (+) sign in the right pane to assign it your Hibernate configuration file.
7. Open Persistence window, there you should see the list of your project modules.
8. Expand the module name, and assign your data source to the hibernate.cfg.xml file. 

Now you can write queries on hibernate console and get HQL auto completion.



