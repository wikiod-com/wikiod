---
title: "Code Contracts"
slug: "code-contracts"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

In order to fully benefit from Code Contracts you need to install the [extension](https://github.com/Microsoft/CodeContracts/releases) for Visual Studio. There's also a [Code Contracts User Manual](http://research.microsoft.com/en-us/projects/contracts/userdoc.pdf).

## Standard precondition
    using System.Diagnostics.Contracts;
    
    public int DivideNumbers(int numerator, int denominator)
    {
        Contract.Requires(denominator != 0);
    
        return numerator / denominator;
    }

## Precondition that throws a specific Exception
    using System.Diagnostics.Contracts;
    
    public int DivideNumbers(int numerator, int denominator)
    {
        Contract.Requires<ArgumentOutOfRangeException>(denominator != 0);
    
        return numerator / denominator;
    }

## Pre and postconditions
    using System.Diagnostics.Contracts;

    public int IncrementByRandomAmount(int input)
    {   
        Contract.Requires<ArgumentNullException>(input != null); // Don't allow null parameter.
        Contract.Requires<ArgumentOutOfRangeException>(input < int.MaxValue); // We can't do anything if we're given int.MaxValue.
        Contract.Ensures(Contract.Result<int>() > input); // Return value will be greater than input value.

        Random rnd = new Random();
        input += rnd.Next(1, 13); // Creates a number between 1 and 12 and adds it to input.

        return input;
    }

