---
title: "Acumatica BQL Reference"
slug: "acumatica-bql-reference"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## BQL Parse and Verify
Any Acumatica application developer spends a great deal of their time writing BQL code. At the same time, not everybody knows the underlying details of how BQL types work under the hood.

At the heart of BQL lay two key methods: `Parse()` and `Verify()`, declared by the `IBqlCreator` interface. Most of the commonly used BQL types, such as `Where<>`, `And<>`, `Or<>` etc., derive from this interface.

It should be admitted that the names these methods historically stuck with are not very descriptive. Arguably better alternative names for them would be `PrepareCommandText` and `Evaluate`.

# Parse

    public void Parse(
        PXGraph graph, 
        List<IBqlParameter> pars, 
        List<Type> tables, 
        List<Type> fields, 
        List<IBqlSortColumn> sortColumns, 
        StringBuilder text, 
        BqlCommand.Selection selection)

The only purpose of `Parse()` is to translate BQL into an SQL command to be sent into DBMS. Therefore, this method accepts a `StringBuilder` parameter representing the SQL command currently being constructed, to which the BQL creator appends the SQL text representation of itself.

For example, the `And<>` predicate's `Parse()` method will append `" AND "` to the command text, and recursively request translation of all nested BQL creators.

In particular, `And<ARRegister.docType, Equal<ARDocType.invoice>>` will translate into something like `"AND "ARRegister.DocType = 'AR'"`.

# Verify

    public void Verify(
        PXCache cache, 
        object item, 
        List<object> pars, 
        ref bool? result, 
        ref object value)

In contrast to `Parse()`, `Verify()` operates purely at the application level.

Given a record (e.g. an `ARRegister` object), it can be used to calculate expressions on it, including calculating formulas and evaluating conditions.

The `result` parameter is used to store the boolean condition evaluation result. It is mostly used by _predicate_ BQL creators such as `Where<>`. 

The `value` parameter is used to store the expression calculation result. For example, the `value` of a BQL `Constant<string>` is the string representation of that constant. 

Most of the time, BQL creators will either affect the result or the value, but rarely both of them.

One notable usage of the `Verify()` method is in the static `BqlCommand.Meet()` method, used by `PXCache` to determine if a given item satisfies the BQL command:

    public bool Meet(PXCache cache, object item, params object[] parameters)
    {
        List<object> pars = new List<object>(parameters);
        bool? result = null;
        object value = null;
        try {
            Verify(cache, item, pars, ref result, ref value);
        }
        catch (SystemException ex) {
            throw new PXException(String.Format("BQL verification failed! {0}", this.ToString()), ex);
        }
        return result == null || result == true;
    }

# Conclusion

The real power and beauty of BQL creators lies in that most of them can be used at both the database and application level, enabling Acumatica's cache merging mechanism and providing a great opportunity for code reusability.

For instance, when you select records from the database, the `Where<>` clause of the BQL command:

* Will provide `Parse()` to translate itself into SQL text during command preparation.
* Will provide `Verify()` during cache merging to determine which items already residing in the cache `Meet()` the `Where<>` clause conditions so as to include such cached items into the result set.

