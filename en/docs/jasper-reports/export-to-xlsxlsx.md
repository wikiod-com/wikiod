---
title: "Export to xlsxlsx"
slug: "export-to-xlsxlsx"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## With Java
Export to *xlsx* format

    try (InputStream inputStream = JRLoader.getResourceInputStream(path)) {  // read report as input stream 
        JasperReport  jasperReport = JasperCompileManager.compileReport(JRXmlLoader.load(inputStream)); // compile report

        Map<String, Object> params = new HashMap<>(); // init map with report's parameters
        params.put(JRParameter.REPORT_LOCALE, Locale.US);
        params.put(JRParameter.IS_IGNORE_PAGINATION, true);
        JasperPrint jasperPrint = JasperFillManager.fillReport(jasperReport, params, connection);  // prepare report - passs parameters and jdbc connection

        JRXlsxExporter exporter = new JRXlsxExporter(); // initialize exporter 
        exporter.setExporterInput(new SimpleExporterInput(jasperPrint)); // set compiled report as input
        exporter.setExporterOutput(new SimpleOutputStreamExporterOutput(destFile));  // set output file via path with filename
        SimpleXlsxReportConfiguration configuration = new SimpleXlsxReportConfiguration();
        configuration.setOnePagePerSheet(true); // setup configuration
        configuration.setDetectCellType(true);
        exporter.setConfiguration(configuration); // set configuration
        exporter.exportReport();
    }

## Adding autofilter for columns
The using of ***[net.sf.jasperreports.export.xls.auto.filter](http://jasperreports.sourceforge.net/config.reference.html#net.sf.jasperreports.export.xls.auto.filter)*** property allow to add autofilter in generated xls file.

    <columnHeader>
        <band height="30" splitType="Stretch">
            <staticText>
                <reportElement x="0" y="0" width="100" height="20">
                    <property name="net.sf.jasperreports.export.xls.auto.filter" value="Start"/>
                </reportElement>
                <text><![CDATA[First column with filter]]></text>
            </staticText>
            <staticText>
                <reportElement x="100" y="0" width="100" height="20"/>
                <text><![CDATA[Second column with filter]]></text>
            </staticText>
            <staticText>
                <reportElement x="200" y="0" width="100" height="20">
                    <property name="net.sf.jasperreports.export.xls.auto.filter" value="End"/>
                </reportElement>
                <text><![CDATA[Third (Last) column with filter]]></text>
            </staticText>
            <staticText>
                <reportElement x="300" y="0" width="100" height="20"/>
                <text><![CDATA[Fourth column without filter]]></text>
            </staticText>            
        </band>
    </columnHeader>

The property can be set in *[Jaspersoft Studio](https://www.wikiod.com/jasper-reports/getting-started-with-jasper-reports#Installation or Setup)* with help of context menu or manually by editing *[jrxml](https://www.wikiod.com/jasper-reports/getting-started-with-jasper-reports#Jasper report file formats)* file.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/2SwQ2.png

