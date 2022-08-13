---
title: "Introduction to Pester"
slug: "introduction-to-pester"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

Pester is a test framework for PowerShell that allows you to run test cases for you PowerShell code. It can be used to run ex. unit tests to help you verify that your modules, scripts etc. work as intended.

[What is Pester and Why Should I Care?][1]


  [1]: https://blogs.technet.microsoft.com/heyscriptingguy/2015/12/14/what-is-pester-and-why-should-i-care/

## Getting Started with Pester
To get started with unit testing PowerShell code using the Pester-module, you need to be familiar with three keywords/commands:

- **Describe**: Defines a group of tests. All Pester test files needs at least one Describe-block.
- **It**: Defines an individual test. You can have multiple It-blocks inside a Descripe-block.
- **Should**: The verify/test command. It is used to define the result that should be considered a successful test.

Sample:

    Import-Module Pester

    #Sample function to run tests against    
    function Add-Numbers{
        param($a, $b)
        return [int]$a + [int]$b
    }
    
    #Group of tests
    Describe "Validate Add-Numbers" {

            #Individual test cases
            It "Should add 2 + 2 to equal 4" {
                Add-Numbers 2 2 | Should Be 4
            }
    
            It "Should handle strings" {
                Add-Numbers "2" "2" | Should Be 4
            }
    
            It "Should return an integer"{
                Add-Numbers 2.3 2 | Should BeOfType Int32
            }

    }

Output:

    Describing Validate Add-Numbers
     [+] Should add 2 + 2 to equal 4 33ms
     [+] Should handle strings 19ms
     [+] Should return an integer 23ms

