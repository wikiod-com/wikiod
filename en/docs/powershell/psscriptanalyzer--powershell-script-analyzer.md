---
title: "PSScriptAnalyzer - PowerShell Script Analyzer"
slug: "psscriptanalyzer---powershell-script-analyzer"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

PSScriptAnalyzer, https://github.com/PowerShell/PSScriptAnalyzer, is a static code checker for Windows PowerShell modules and scripts. PSScriptAnalyzer checks the quality of Windows PowerShell code by running a set of rules based on PowerShell best practices identified by the PowerShell Team and community. It generates DiagnosticResults (errors and warnings) to inform users about potential code defects and suggests possible solutions for improvements.

`PS> Install-Module -Name PSScriptAnalyzer`

## Syntax
 1. `Get-ScriptAnalyzerRule [-CustomizedRulePath <string[]>] [-Name <string[]>] [-Severity <string[]>] [<CommonParameters>]`
 2. `Invoke-ScriptAnalyzer  [-Path] <string> [-CustomizedRulePath <string[]>] [-ExcludeRule <string[]>] [-IncludeRule<string[]>] [-Severity <string[]>] [-Recurse] [-SuppressedOnly]                             [<CommonParameters>]`

## Analyzing scripts with the built-in preset rulesets
ScriptAnalyzer ships with sets of built-in preset rules that can be used to analyze scripts. These include: `PSGallery`, `DSC` and `CodeFormatting`. They can be executed as follows: 

**PowerShell Gallery rules**

To execute the PowerShell Gallery rules use the following command:

    Invoke-ScriptAnalyzer -Path /path/to/module/ -Settings PSGallery -Recurse

**DSC rules**

To execute the DSC rules use the following command:

    Invoke-ScriptAnalyzer -Path /path/to/module/ -Settings DSC -Recurse

**Code formatting rules**

To execute the code formatting rules use the following command:

    Invoke-ScriptAnalyzer -Path /path/to/module/ -Settings CodeFormatting -Recurse


## Analyzing scripts against every built-in rule
To run the script analyzer against a single script file execute:

    Invoke-ScriptAnalyzer -Path myscript.ps1

 This will analyze your script against every built-in rule.  If your script is sufficiently large that could result in a lot of warnings and/or errors.

To run the script analyzer against a whole directory, specify the folder containing the script, module and DSC files you want analyzed. Specify the Recurse parameter if you also want sub-directories searched for files to analyze.

    Invoke-ScriptAnalyzer -Path . -Recurse

## List all built-in rules
To see all the built-in rules execute:

    Get-ScriptAnalyzerRule

