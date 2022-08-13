---
title: "Using subreports"
slug: "using-subreports"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Parameters
| Parameter | Details |
|-----------|---------|
| ***parametersMapExpression*** | The Map with parameters. *Not required* |
| ***subreportParameter*** | The pair of name and value (set with ***subreportParameterExpression***). *Not required*. Several parameters can be passed to subreport |
| ***connectionExpression*** | Connection for getting data. *Not required* |
| ***dataSourceExpression*** | Expression for passing Datasource. *Not required* |
| ***subreportExpression*** | The subreport's path/URI or even JasperReport object. *Not required* |
| ***returnValue*** | The pair of name and value. *Not required*. Several values can be returned from subreport to master report back |


 - Subreports can be used for constructing complex reports. The reusing of existing reports is another goal of using subreports.

 - The subreport will be shown as a part of master report in case using of `<subreport>` element.

 - The value of [***subreportExpression***](https://www.wikiod.com/jasper-reports/using-subreports) parameter is differ for using at *JasperReports Server* or just by *JasperReports* framework (some *API* using or using in IDE).

   For *JasperReports Server* it looks like: 

       <subreportExpression><![CDATA["repo:subreport.jrxml"]]></subreportExpression>

   For using by just *JasperReports* engine:

       <subreportExpression><![CDATA["/somePath/subreport.jasper"]]></subreportExpression>

   <sup>The great explanation by *@AndreasDietrich* can be found at *[JasperServer: Unable to locate the subreport exception](http://stackoverflow.com/a/20374515/876298)* post</sup>

 - For some reasons the subreport can be used as a common report - without calling from the master report (with help of `<subreport>` element). The subreport is always a report.


## Passing connection to subreport; return values back to the master report
This is a snippet of master report. Two parameters and the connection (for example, *jdbc*) are passing to the subreport. One value is returned from the subreport back to the master report, this value (*variable*) can be used in master report

    <subreport>
        <reportElement x="0" y="80" width="200" height="100"/>
        <subreportParameter name="someSubreportParameter">
            <subreportParameterExpression><![CDATA[$P{someMasterReportParamter}]]></subreportParameterExpression>
        </subreportParameter>
        <subreportParameter name="anotherSubreportParameter">
            <subreportParameterExpression><![CDATA["Some text - constant value"]]></subreportParameterExpression>
        </subreportParameter>
        <connectionExpression><![CDATA[$P{REPORT_CONNECTION}]]></connectionExpression>
        <returnValue subreportVariable="someVariableInSubreport" toVariable="someVariableInMasterReport"/>
        <subreportExpression><![CDATA["$P{SUBREPORT_DIR} + "subreport.jasper"]]></subreportExpression>
    </subreport>

## Passing datasoure to subreport
This is a snippet of master report. The datasource is passed to the subreport with help of  ***[net.sf.jasperreports.engine.data.JRBeanCollectionDataSource](http://jasperreports.sourceforge.net/api/net/sf/jasperreports/engine/data/JRBeanCollectionDataSource.html)*** constructor

    <field name="someFieldWithList" class="java.util.List"/>
    <!-- ...... -->
    <subreport>
        <reportElement x="0" y="0" width="200" height="70"/>
        <parametersMapExpression><![CDATA[$P{REPORT_PARAMETERS_MAP}]]></parametersMapExpression>
        <dataSourceExpression><![CDATA[net.sf.jasperreports.engine.data.JRBeanCollectionDataSource($F{someFieldWithList})]]></dataSourceExpression>
        <subreportExpression><![CDATA[$P{SUBREPORT_DIR} + "subreport.jasper"]]></subreportExpression>
    </subreport>



