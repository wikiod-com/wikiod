---
title: "Export to pdf"
slug: "export-to-pdf"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

To render **fonts** correctly in pdf [font-extensions][1] should always be used (in classpath) 


  [1]: https://www.wikiod.com/jasper-reports/font-extensions#Creating and using font extensions

## With Java
To export a you need to [fill the report][1] to get the [JasperPrint][2] object.

# Export single JasperPrint (single jrxml) to file

    // 1. Create exporter instance
    JRPdfExporter exporter = new JRPdfExporter();

    // 2. Set exporter input document
    exporter.setExporterInput(new SimpleExporterInput(jasperPrint));

    // 3. Set file path for exporter output
    exporter.setExporterOutput(new SimpleOutputStreamExporterOutput("/path/filename.pdf"));

    // 4. Create configuration instance
    SimplePdfExporterConfiguration configuration = new SimplePdfExporterConfiguration();

    // 5. Associate configuration with exporter
    exporter.setConfiguration(configuration);

    // 6. Fill export and write to file path
    exporter.exportReport();

# Export multiple JasperPrint's (multiple jrxml) to single file

Only the first steps differ from the previous set:

    List<JasperPrint> jasperPrintList = new ArrayList<>();
    jasperPrintList.add(jasperPrint1);
    jasperPrintList.add(jasperPrint2);

    JRPdfExporter exporter = new JRPdfExporter();
    exporter.setExporterInput(SimpleExporterInput.getInstance(jasperPrintList));

The remaining steps are the same:

    exporter.setExporterOutput(new SimpleOutputStreamExporterOutput("/path/filename.pdf"));
    SimplePdfExporterConfiguration configuration = new SimplePdfExporterConfiguration();
    exporter.setConfiguration(configuration);
    exporter.exportReport();

See [SimplePdfExporterConfiguration API][3] for configuration details.


  [1]: https://www.wikiod.com/jasper-reports/fill-report
  [2]: http://jasperreports.sourceforge.net/api/net/sf/jasperreports/engine/JasperPrint.html
  [3]: http://jasperreports.sourceforge.net/api/net/sf/jasperreports/export/SimplePdfExporterConfiguration.html

## With IDE (Integrated development environment)
# JasperSoft Studio

In Preview, run report by clicking green arrow, if no errors the export menu will be enable, click the export button (disk image) and select "Export As Pdf"
[![Export as pdf][1]][1]


  [1]: http://i.stack.imgur.com/If2S9.png

