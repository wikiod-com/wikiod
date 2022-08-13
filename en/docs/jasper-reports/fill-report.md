---
title: "Fill report"
slug: "fill-report"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Parameters
| Parameters | Column |
| ------ | ------ |
| jasperPrint | The output of the fill process that can be exported to desired format |
| reportTemplate | The compiled design file `.jasper` |
| parameters | The parameter [Map][1], that if defined can be references inside report by `$P{key}` |
| datasource | A [net.sf.jasperreports.engine.JRDataSource][2] |
| connection | A database connection [java.sql.Connection](https://docs.oracle.com/javase/8/docs/api/java/sql/Connection.html) |


  [1]: https://docs.oracle.com/javase/7/docs/api/java/util/Map.html
  [2]: http://jasperreports.sourceforge.net/api/net/sf/jasperreports/engine/JRDataSource.html

## Fill JasperReport Template using Java
# Common Requirements

All reports, regardless of how the data is presented, take a path to the report template and a parameter map. The variables are used in all examples that follow:

    // Parameters passed into the report.
    Map<String, Object> parameters = new HashMap<>();

    // Arbitrary parameter passed into the report.
    parameters.put("KEY", "Value");

    // The compiled report design.
    String path = "path/to/template.jasper";

Using a `.jrxml` file incurs an extra compilation step that isn't necessary in most situations. Unless you've written custom software to change the `.jrxml` before the report runs (e.g., adding or removing columns dynamically), use the `.jasper` file as shown in the subsequent examples.

# Using a Database Connection

    // Establish a database connection.
    Connection connection = DriverManager.getConnection(url, username, password); 

    // Fill the report, get the JasperPrint that can be exported to desired format.
    JasperPrint jasperPrint = JasperFillManager.fillReport(
      path, parameters, connection); 

# Using a Custom Data Source

    // Populate this list of beans as per your requirements.
    List<Bean> beans = new ArrayList<>();

    // Wrap the beans in a beans in a JRBeanCollectionDataSource.
    JRBeanCollectionDataSource datasource = new JRBeanCollectionDataSource(beans);

    // Fill the report, get the JasperPrint that can be exported to desired format.
    JasperPrint jasperPrint = JasperFillManager.fillReport(
      path, parameters, datasource);

# Without Data Source, unused Detail Band

    // Fill the report, get the JasperPrint that can be exported to desired format.
    JasperPrint jasperPrint = JasperFillManager.fillReport(path, parameters);

> Without a datas ource, the attribute `whenNoDataType="AllSectionsNoDetail"` on the `JasperReport` element must be set, otherwise an empty (blank) report will be generated.

## With IDE (Integrated development environment)
# JasperSoft Studio

1. If datasource or database connection is needed to fill report, create your Data Adapter 
in Repository Explorer by right clicking "Data Adapters" selecting "Create Data Adapter"

2. Enter preview mode by selecting the **Preview** tab (no errors in deign need to be present)

3. Select desired dastasource (if no datasource is required select "One Empty Record"

4. Set parameter as desired

5. Fill report by clicking the green arrow "Run the report"

[![Fill report][1]][1]


  [1]: http://i.stack.imgur.com/WRqWi.png

