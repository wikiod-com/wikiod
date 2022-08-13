---
title: "Guid"
slug: "guid"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

GUID (or UUID) is an acronym for 'Globally Unique Identifier' (or 'Universally Unique Identifier'). It is a 128-bit integer number used to identify resources.

`Guid`s are *Globally Unique Identifiers*, also known as *UUID*'s, *Universally Unique Identifiers*.

They are 128-bit pseudorandom values. There are so many valid `Guid`s (about 10^18 `Guid`s for each cell of every people on Earth) that if they are generated by a good pseudorandom algorithm, they can be considered unique in the whole universe by all practical means.

`Guid`s are most often used as primary keys in databases. Their advantage is that you don't have to call the database to get a new ID that is (almost) guaranteed to be unique.

## Getting the string representation of a Guid
A string representation of a Guid can be obtained by using the built in `ToString` method

    string myGuidString = myGuid.ToString();

Depending on your needs you can also format the Guid, by adding a format type argument to the `ToString` call.

    var guid = new Guid("7febf16f-651b-43b0-a5e3-0da8da49e90d");

    // None          "7febf16f651b43b0a5e30da8da49e90d"
    Console.WriteLine(guid.ToString("N"));

    // Hyphens       "7febf16f-651b-43b0-a5e3-0da8da49e90d"
    Console.WriteLine(guid.ToString("D"));

    // Braces        "{7febf16f-651b-43b0-a5e3-0da8da49e90d}"
    Console.WriteLine(guid.ToString("B"));

    // Parentheses   "(7febf16f-651b-43b0-a5e3-0da8da49e90d)"
    Console.WriteLine(guid.ToString("P"));

    // Hex           "{0x7febf16f,0x651b,0x43b0{0xa5,0xe3,0x0d,0xa8,0xda,0x49,0xe9,0x0d}}"
    Console.WriteLine(guid.ToString("X"));


## Creating a Guid
These are the most common ways to create an instance of Guid:

- Creating an empty guid (`00000000-0000-0000-0000-000000000000`):


    Guid g = Guid.Empty;
    Guid g2 = new Guid();

- Creating a new (pseudorandom) Guid:


    Guid g = Guid.NewGuid();

- Creating Guids with a specific value:


    Guid g = new Guid("0b214de7-8958-4956-8eed-28f9ba2c47c6");
    Guid g2 = new Guid("0b214de7895849568eed28f9ba2c47c6");
    Guid g3 = Guid.Parse("0b214de7-8958-4956-8eed-28f9ba2c47c6");


## Declaring a nullable GUID
Like other value types, GUID also has a nullable type which can take null value.

Declaration : 

    Guid? myGuidVar = null;

This is particularly useful when retrieving data from the data base when there is a possibility that value from a table is NULL.
