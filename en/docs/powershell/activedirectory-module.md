---
title: "ActiveDirectory module"
slug: "activedirectory-module"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

This topic will introduce you to some of the basic cmdlets used within the Active Directory Module for PowerShell, for manipulating Users, Groups, Computers and Objects.


Please remember that PowerShell's Help System is one of the best resources you can possibly utilize.

    Get-Help Get-ADUser -Full
    Get-Help Get-ADGroup -Full
    Get-Help Get-ADComputer -Full
    Get-Help Get-ADObject -Full

All of the help documentation will provide examples, syntax and parameter help.

## Users
Retrieve Active Directory User

    Get-ADUser -Identity JohnSmith
Retrieve All Properties Associated with User

    Get-ADUser -Identity JohnSmith -Properties *

Retrieve Selected Properties for User

    Get-ADUser -Identity JohnSmith -Properties * | Select-Object -Property sAMAccountName, Name, Mail

New AD User

    New-ADUser -Name "MarySmith" -GivenName "Mary" -Surname "Smith" -DisplayName "MarySmith" -Path "CN=Users,DC=Domain,DC=Local"

## Module
    #Add the ActiveDirectory Module to current PowerShell Session
    Import-Module ActiveDirectory 
 

## Groups
Retrieve Active Directory Group

    Get-ADGroup -Identity "My-First-Group" #Ensure if group name has space quotes are used
Retrieve All Properties Associated with Group

    Get-ADGroup -Identity "My-First-Group" -Properties *
Retrieve All Members of a Group

    Get-ADGroupMember -Identity "My-First-Group" | Select-Object -Property sAMAccountName
    Get-ADgroup "MY-First-Group" -Properties Members | Select -ExpandProperty Members

Add AD User to an AD Group

    Add-ADGroupMember -Identity "My-First-Group" -Members "JohnSmith"
New AD Group

    New-ADGroup -GroupScope Universal -Name "My-Second-Group"

## Computers
Retrieve AD Computer

    Get-ADComputer -Identity "JohnLaptop"
Retrieve All Properties Associated with Computer

    Get-ADComputer -Identity "JohnLaptop" -Properties *
Retrieve Select Properties of Computer

    Get-ADComputer -Identity "JohnLaptop" -Properties * | Select-Object -Property Name, Enabled

## Objects
Retrieve an Active Directory Object

    #Identity can be ObjectGUID, Distinguished Name or many more
    Get-ADObject -Identity "ObjectGUID07898" 

Move an Active Directory Object

    Move-ADObject -Identity "CN=JohnSmith,OU=Users,DC=Domain,DC=Local" -TargetPath "OU=SuperUser,DC=Domain,DC=Local"

Modify an Active Directory Object

    Set-ADObject -Identity "CN=My-First-Group,OU=Groups,DC=Domain,DC=local" -Description "This is My First Object Modification"

