---
title: "Using stdunordered_map"
slug: "using-stdunordered_map"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

std::unordered_map is just an associative container. It works on keys and their maps. Key as the names goes, helps to have uniqueness in the map. While the mapped value is just a content that is associated with the key. The data types of this key and map can be any of the predefined data type or user-defined.

As the name goes, the elements in unordered map are not stored in sorted sequence. They are stored according to their hash values and hence, usage of unordered map has many benefits such as it only takes O(1) to search any item from it. It is also faster than other map containers. It is also visible from the example that it is very easy to implement as the operator ( [] ) helps us to directly access the mapped value. 

## Declaration and Usage
As already mentioned you can declare an unordered map of any type. Let's have a unordered map named first with string and integer type.

    unordered_map<string, int> first; //declaration of the map 
    first["One"] = 1; // [] operator used to insert the value 
    first["Two"] = 2;
    first["Three"] = 3;
    first["Four"] = 4;
    first["Five"] = 5;

    pair <string,int> bar = make_pair("Nine", 9); //make a pair of same type
    first.insert(bar); //can also use insert to feed the values

## Some Basic Functions
    unordered_map<data_type, data_type> variable_name; //declaration
    
    variable_name[key_value] = mapped_value; //inserting values
    
    variable_name.find(key_value); //returns iterator to the key value
    
    variable_name.begin(); // iterator to the first element

    variable_name.end(); // iterator to the last + 1 element



