---
title: "Comment-based help"
slug: "comment-based-help"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

PowerShell features a documentation mechanism called comment-based help. It allows documenting scripts and functions with code comments. Comment-based help is most of the time written in comment blocks containing multiple help keywords. Help keywords start with dots and identify help sections that will be displayed by running the `Get-Help` cmdlet.

## Function comment-based help
    <#
    
    .SYNOPSIS
        Gets the content of an INI file.
    
    .DESCRIPTION
        Gets the content of an INI file and returns it as a hashtable.
    
    .INPUTS
        System.String
    
    .OUTPUTS
        System.Collections.Hashtable
    
    .PARAMETER FilePath
        Specifies the path to the input INI file.
    
    .EXAMPLE
        C:\PS>$IniContent = Get-IniContent -FilePath file.ini
        C:\PS>$IniContent['Section1'].Key1
        Gets the content of file.ini and access Key1 from Section1.
    
    .LINK
        Out-IniFile
    
    #>
    function Get-IniContent
    {
        [CmdletBinding()]
        Param
        (
            [Parameter(Mandatory=$true,ValueFromPipeline=$true)]
            [ValidateNotNullOrEmpty()]
            [ValidateScript({(Test-Path $_) -and ((Get-Item $_).Extension -eq ".ini")})]
            [System.String]$FilePath
        )
    
        # Initialize output hash table.
        $ini = @{}
        switch -regex -file $FilePath
        {
            "^\[(.+)\]$" # Section
            {
                $section = $matches[1]
                $ini[$section] = @{}
                $CommentCount = 0
            }
            "^(;.*)$" # Comment
            {
                if( !($section) )
                {
                    $section = "No-Section"
                    $ini[$section] = @{}
                }
                $value = $matches[1]
                $CommentCount = $CommentCount + 1
                $name = "Comment" + $CommentCount
                $ini[$section][$name] = $value
            } 
            "(.+?)\s*=\s*(.*)" # Key
            {
                if( !($section) )
                {
                    $section = "No-Section"
                    $ini[$section] = @{}
                }
                $name,$value = $matches[1..2]
                $ini[$section][$name] = $value
            }
        }
            
        return $ini
    }

The above function documentation can be displayed by running `Get-Help -Name Get-IniContent -Full`:

[![enter image description here][1]][1]


Notice that the comment-based keywords starting with a `.` match the `Get-Help` result sections.

  [1]: https://i.stack.imgur.com/orT77.png

## Script comment-based help
    <#
    
    .SYNOPSIS
        Reads a CSV file and filters it.
    
    .DESCRIPTION
        The ReadUsersCsv.ps1 script reads a CSV file and filters it on the 'UserName' column.
    
    .PARAMETER Path
        Specifies the path of the CSV input file.
    
    .INPUTS
        None. You cannot pipe objects to ReadUsersCsv.ps1.
    
    .OUTPUTS
        None. ReadUsersCsv.ps1 does not generate any output.
    
    .EXAMPLE
        C:\PS> .\ReadUsersCsv.ps1 -Path C:\Temp\Users.csv -UserName j.doe
    
    #>
    Param
    (
        [Parameter(Mandatory=$true,ValueFromPipeline=$false)]
        [System.String]
        $Path,
        [Parameter(Mandatory=$true,ValueFromPipeline=$false)]
        [System.String]
        $UserName
    )
    
    Import-Csv -Path $Path | Where-Object -FilterScript {$_.UserName -eq $UserName}

The above script documentation can be displayed by running `Get-Help -Name ReadUsersCsv.ps1 -Full`:

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/u1pRK.png

