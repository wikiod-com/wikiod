---
title: "Cascades"
slug: "cascades"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Syntax


 - cascade="all-delete-orphan"

Entities has associations to other objects, this may be an association to a single item (many-to-one) or an association to a collection (one-to-many, many-to-any).

At any rate, you are able to tell NHibernate to automatically traverse an entity's associations, and act according to the cascade option. For instance, adding an unsaved entity to a collection with save-update cascade will cause it to be saved along with its parent object, without any need for explicit instructions on our side.

https://ayende.com/blog/1890/nhibernate-cascades-the-different-between-all-all-delete-orphans-and-save-update

## save-update
when the object is saved/updated, check the associations and save/update any object that require it (including save/update the associations in many-to-many scenario).


## none
do not do any cascades, let the users handles them by themselves.

## delete
when the object is deleted, delete all the objects in the association.

## delete-orphan
when the object is deleted, delete all the objects in the association. In addition to that, when an object is removed from the association and not associated with another object (orphaned), also delete it.

## all 
 when an object is save/update/delete, check the associations and save/update/delete all the objects found.

## all-delete-orphan
when an object is save/update/delete, check the associations and save/update/delete all the objects found. In additional to that, when an object is removed from the association and not associated with another object (orphaned), also delete it.

