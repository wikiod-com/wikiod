---
title: "Getting started with jasper-reports"
slug: "getting-started-with-jasper-reports"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Jasper report file formats
- `.jrxml` is the report design file, it's format is in human readable XML, it can be complied into a `JasperReport` object and saved as a `.jasper`


- `.jasper` is the compiled version of the `.jrxml` and can be loaded directly into a `JasperReport` object ready to be filled with data


- `.jrprint` is the serialized `JasperPrint` object, a report that have already been filled with data and can be loaded to be printed, viewed and/or exported to desired format.


- `.jrpxml` is the XML rappresentativo of a `JasperPrint` object it can be modified and then unmarshaled to retrieve the `JasperPrint` object

## Installation or Setup
# JasperReports Library

*JasperReports* is a open source Java based reporting tool. The *JasperReports* Library can be downloaded from the [Jaspersoft Community](http://community.jaspersoft.com/download) for the latest [release](http://community.jaspersoft.com/project/jasperreports-library/releases).

> In recent releases the third-party jars in the lib folder are **not**
> distributed, they need to be download from public repositories, see
> distributed `pom.xml` for dependencies. Maven can be used to
> retrieve all dependencies including the transient ones in the
> target/dependence folder.

    mvn dependency:copy-dependencies

# Jaspersoft Studio (IDE)

[Jaspersoft Studio][1] is the official design client for JasperReports--built on the Eclipse platform--to replace iReport Designer.

# iReport Designer (IDE)

[iReport Designer][2] is the previous report designer for JasperReports. Version 5.6.0 (released in May of 2014) was the last official version; vendor support ended at the end of 2015.

---

# JasperReport Commuity resources

### JasperReports Library FAQs

 - [FAQ][8]

### Source code

- [JasperReports Library source code][16]

### Tutorials

 - [Tutorials Point][9]
 - [JasperReports Ultimate Guide][14]

### Samples

 - [Sample Reference][13]


### References
 - [Official documentation][10]
 - [Community Wiki][11]


### Official Bug Tracker
 - [Bug Tracker][15]


  [1]: http://community.jaspersoft.com/project/jaspersoft-studio
  [2]: http://community.jaspersoft.com/project/ireport-designer
  [8]: http://community.jaspersoft.com/wiki/jasperreports-library-faqs
  [9]: http://www.tutorialspoint.com/jasper_reports/index.htm
  [10]: http://community.jaspersoft.com/documentation?version=13758
  [11]: http://community.jaspersoft.com/wiki/community-wiki
  [12]: http://sourceforge.net/projects/jasperreports/files/jasperreports/
  [13]: http://jasperreports.sourceforge.net/sample.reference.html
  [14]: http://jasperreports.sourceforge.net/JasperReports-Ultimate-Guide-3.pdf
  [15]: http://community.jaspersoft.com/bug-tracker
  [16]: https://sourceforge.net/p/jasperreports/code/ci/master/tree/

 

## Work flow
The work flow in jasper-reports is:

 1. Design the report, create the jrxml file that defines the report layout. The jrxml can be create by using a simple texteditor but normally an IDE (JasperSoft Studio or iReport) is used both to speed up report development but also to have a visual view of layout. 


 2. Compile the report (the jrxml) to get a .jasper file or a [JasperReport][1] object. This process can be compared with a `.java` file being compiled to `.class`. 


 3. [Fill the report][2], pass parameters and a datasource to the report to generate the print object [JasperPrint][3] that can also be saved to a `.jprint` file  


 4. View, print and/or export the JasperPrint. The most commons export format are supported as pdf, excel, word, html, cvs etc.

 
  [1]: http://jasperreports.sourceforge.net/api/net/sf/jasperreports/engine/JasperReport.html
  [2]: https://www.wikiod.com/jasper-reports/fill-report
  [3]: http://jasperreports.sourceforge.net/api/net/sf/jasperreports/engine/JasperPrint.html

## Understanding the different report bands
# Title 

This band is showed once at the beginning of the report. It can be used as first page by setting the attribute `isTitleNewPage="true"`

# Page Header

This appears at the beginning of each page excluding first page if Title band is used and last page if Summary band is used with setting `isSummaryWithPageHeaderAndFooter="false"`

# Column Header

This appears before the detail band on each page.

# Detail

This section is iterated **for each record** in datasource supplied. It is allowed to have multiple detail band (detail 1, detail 2 .. detail n), the are iterated as follows

    Row 1
        detail 1
        detail 2
        detail n
    Row 2
        detail 1
        detail 2
        detail n

# Column Footer

This appears below the detail band on each page where detail band is present. The default setting is end of page (before Page footer) but this can be switch to under last detail band (last record) by setting the attribute `isFloatColumnFooter="true"`

# Page Footer

This appears at the bottom of each page excluding title band, summary band (without page footer) and last non summary band if Last Page Footer is used.  

# Last Page Footer

This appears on last page (if not summary band without page footer) instead of normal Page Footer

# Summary

This appears at the end of the report in new page if `isSummaryNewPage="true"` is set and with page header and footer if `isSummaryWithPageHeaderAndFooter="true"`

    
# Group Header

This section appears if a group is defined every time the group expression change, before the detail band.

# Group Footer

This section appears if a group is defined every time *before* the group expression change, after the detail band.

# Background

This band is displayed on every page as background to all other bands.

# No Data

This appears only if no datasource was passed or the datasource is empty (0 records) and `whenNoDataType="NoDataSection"` is set.

