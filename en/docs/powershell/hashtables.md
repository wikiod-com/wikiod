---
title: "HashTables"
slug: "hashtables"
draft: false
images: []
weight: 9949
type: docs
toc: true
---

A Hash Table is a structure which maps keys to values.  See [Hash Table](https://en.wikipedia.org/wiki/Hash_table) for details.

An important concept which relies on Hash Tables is [Splatting](https://www.wikiod.com/powershell/splatting).  It is very useful for making a large number of calls with repetitive parameters.

## Access a hash table value by key.
An example of defining a hash table and accessing a value by the key
```
$hashTable = @{
    Key1 = 'Value1'
    Key2 = 'Value2'
}
$hashTable.Key1
#output
Value1
```
An example of accessing a key with invalid characters for a property name:
```
$hashTable = @{
    'Key 1' = 'Value3'
    Key2 = 'Value4'
}
$hashTable.'Key 1'
#Output
Value3
```

## Creating a Hash Table
Example of creating an empty HashTable:

```
$hashTable = @{}
```

Example of creating a HashTable with data:

```
$hashTable = @{
    Name1 = 'Value'
    Name2 = 'Value'
    Name3 = 'Value3'
}
```

## Add a key value pair to an existing hash table
An example, to add a "Key2" key with a value of "Value2" to the hash table, using the addition operator:
```
$hashTable = @{
    Key1 = 'Value1'
}
$hashTable += @{Key2 = 'Value2'}
$hashTable

#Output

Name                           Value
----                           -----
Key1                           Value1
Key2                           Value2
```
An example, to add a "Key2" key with a value of "Value2" to the hash table using the Add method:
```
$hashTable = @{
    Key1 = 'Value1'
}
$hashTable.Add("Key2", "Value2")
$hashTable

#Output

Name                           Value
----                           -----
Key1                           Value1
Key2                           Value2

## Looping over a hash table
```
$hashTable = @{
    Key1 = 'Value1'
    Key2 = 'Value2'
}

foreach($key in $hashTable.Keys)
{
    $value = $hashTable.$key
    Write-Output "$key : $value"
}
#Output
Key1 : Value1
Key2 : Value2
```

## Enumerating through keys and Key-Value Pairs
*Enumerating through Keys*

    foreach ($key in $var1.Keys) {
        $value = $var1[$key]
        # or
        $value = $var1.$key 
    }

*Enumerating through Key-Value Pairs*

    foreach ($keyvaluepair in $var1.GetEnumerator()) {
        $key1 = $_.Key1
        $val1 = $_.Val1
    }

## Remove a key value pair from an existing hash table
An example, to remove a "Key2" key with a value of "Value2" from the hash table, using the remove operator:

    $hashTable = @{
        Key1 = 'Value1'
        Key2 = 'Value2'
    }
    $hashTable.Remove("Key2", "Value2")
    $hashTable
    
    #Output
    
    Name                           Value
    ----                           -----
    Key1                           Value1

