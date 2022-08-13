---
title: "Diagnostics Asserts"
slug: "diagnostics-asserts"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Syntax
 - Assert.ArgumentCondition(bool condition, string argumentName, string message)
 - Assert.ArgumentNotNull(object argument, string argumentName)
 - Assert.ArgumentNotNull(object argument, Func&lt;string&gt; getArgumentName)
 - Assert.ArgumentNotNullOrEmpty(ID argument, string argumentName)
 - Assert.ArgumentNotNullOrEmpty(string argument, string argumentName)
 - Assert.ArgumentNotNullOrEmpty(string argument, Func&lt;string&gt; getArgumentName)
 - Assert.AreEqual(int value1, int value2, string message)
 - Assert.AreEqual(int value1, int value2, string format, params object[] args)
 - Assert.AreEqual(string value1, string value2, string message)
 - Assert.AreEqual(string value1, string value2, string format, params object[] args)
 - Assert.AreEqual(bool value1, bool value2, string message)
 - Assert.AreEqual(bool value1, bool value2, string format, params object[] args)
 - Assert.CanRunApplication(string application)
 - Assert.HasAccess(bool accessAllowed, string message)
 - Assert.HasAccess(bool accessAllowed, string format, params object[] args)
 - Assert.IsEditing(Item item)
 - Assert.IsFalse(bool condition, string message)
 - Assert.IsFalse(bool condition, Func&lt;string&gt; getMessage)
 - Assert.IsTrue(bool condition, string format, params object[] args)
 - Assert.IsNotNull(object value, string message)
 - Assert.IsNotNull(object value, string format, params object[] args)
 - Assert.IsNotNull(object value, Type type)
 - Assert.IsNotNull(object value, Type type, string format, params object[] args)
 - Assert.IsNotNullOrEmpty(string value, string message)
 - Assert.IsNotNullOrEmpty(string value, string format, params object[] args)
 - Assert.IsNull(object value, string message)
 - Assert.IsNull(object value, string format, params object[] args)
 - Assert.IsTrue(bool condition, string message)
 - Assert.IsTrue(bool condition, Func&lt;string&gt; getMessage)
 - Assert.IsTrue(bool condition, string format, params object[] args)
 - Assert.Required(object obj, string message)
 - Assert.Required(object obj, string format, params object[] args)
 - Assert.ResultNotNull&lt;T&gt;(T result, string message)
 - Assert.ResultNotNull&lt;T&gt;(T result)

## Argument Checks
## ArgumentCondition ##
This method checks to see if the argument specified is true. It also takes in the name of the argument that is logged if the condition fails.

    Assert.ArgumentCondition(pageIndex >= 0, "pageIndex", "Value must be greater than or equal to zero.");

## ArgumentNotNull ##
This method ensures that the argument passed is not null. There are two signatures for this method, the first takes in an object and a paramter name and does a simple null check.

    Assert.ArgumentNotNull(item, "item");

## ArgumentNotNullOrEmpty ##
This is similar to the ArgumentNotNull method, but will also check to see if the object is empty. There are three variants of this method. The first variant takes in a Sitecore ID and an argument name, and checks to see if the ID is null.

    var nullId = new new ID("{00000000-0000-0000-0000-000000000000}");
    
    // Both of these calls will result in an exception
    Assert.ArgumentNotNullOrEmpty((ID)null, "null");
    Assert.ArgumentNotNullOrEmpty(nullId, nameof(nullId));

The second method adds a check to see if the given string is null or empty.

    // Both of these calls will result in an exception
    Assert.ArgumentNotNullOrEmpty((string)null, "null");
    Assert.ArgumentNotNullOrEmpty("", nameof(emptyString));

## Ensure item is in editing mode
Checks to see if the passed `Item` is in Editing mode. If not, it throws an `EditingNotAllowedException`.

    Assert.IsEditing(Sitecore.Context.Item);


## Ensure two values are equal
Compares two values for equality. It can compare strings, integers, and Boolean values only.

    Assert.AreEqual(documentElement.LocalName, "xamlControls", "Xaml files must have a root node named 'xamlControls'.");

## Ensure a value is true or false
To assert that a value is either true or false,:

    Assert.IsFalse(Settings.DoBadThings, "Bad things should not happen, disable DoBadThings.");
    Assert.IsTrue(magicNumber =< 42, "The magic number is greater than 42!");

You can also pass formatting parameters for the exception message

    Assert.IsFalse(myValue > 5, "The value should not be greater than 5, it's currently {0}", myValue);

## ResultNotNull
`ResultNotNull()` checks to see if the object passed in is not null. If the object and message are not null it will then simply return the object that was passed in, otherwise it will throw `InvalidOperationException`.

    return Assert.ResultNotNull(this.Database.GetItem(this.ItemRootId), string.Concat("Root item not found. ID: ", this.ItemRootId));

## Required Object
This checks to see if the given object is null, and then throws `RequiredObjectIsNullException` if it is.

    Assert.Required(parameter, "parameter is required.");


## Null/Empty Checks

## IsNotNull ##
This is a very simple and popular method to use to check if an item is not null. It simply checks the object that is passed in to see if it is null.

    Assert.IsNotNull(database, type, "Name: {0}", item);

## IsNotNullOrEmpty ##
This is the same as IsNotNull above, but works on string values instead of objects.

    Assert.IsNotNullOrEmpty(propertyName, "user");

## IsNull ##
This is simply an inverse of the `IsNotNull()` method. This method asserts that the object is null.

    Assert.IsNull(this.StandardValues, "A Standard Values item has already been created for this template ");


## Security Asserts

## CanRunApplication ##
To check to see if the user has permission to run the given application. If not, `AccessDeniedException` is thrown.

    Assert.CanRunApplication("WebEdit");

## HasAccess ##
`HasAccess` will check if the given parameter is true, otherwise an `AccessDeniedException` will be thrown.

    Assert.HasAccess(Context.User.IsAdministrator, "Only administrators can create new domains");

