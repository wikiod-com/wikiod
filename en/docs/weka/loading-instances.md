---
title: "Loading Instances"
slug: "loading-instances"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## ARFF Files
ARFF files (Attribute-Relation File Format) are the most common format for data used in Weka. Each ARFF file must have a header describing what each data instance should be like. The attributes that can be used are as follows:

 - Numeric

Real or integer numbers.

 - Nominal

Nominal attributes must provide a set of possible values. For example:

    @ATTRIBUTE class        {Iris-setosa,Iris-versicolor,Iris-virginica}

 - String

Allows for arbitrary string values. Usually processed later using the `StringToWordVector` filter.

 - Date

Allows for dates to be specified. As with Java's `SimpleDateFormat`, this date can also be formatted; it will default to ISO-8601 format.

An example header can be seen as follows:
    
    @RELATION iris

    @ATTRIBUTE sepallength  NUMERIC
    @ATTRIBUTE sepalwidth   NUMERIC
    @ATTRIBUTE petallength  NUMERIC
    @ATTRIBUTE petalwidth   NUMERIC
    @ATTRIBUTE class        {Iris-setosa,Iris-versicolor,Iris-virginica}

Following the header each instance must be listed with the correct number of instances; if an attributes value for an instance is not known a `?` can be used instead. The following shows an example of the set of instances in an ARFF file:

    @DATA
    5.1,3.5,1.4,0.2,Iris-setosa
    4.9,3.0,1.4,0.2,Iris-setosa
    4.7,3.2,1.3,0.2,Iris-setosa
    4.6,3.1,1.5,0.2,Iris-setosa
    5.0,3.6,1.4,0.2,Iris-setosa



## Loading ARFF Files
Depending on the version of Weka being used different methods for loading ARFF files should be utilised.

# Weka <3.5.5

The following sample code shows how to load an ARFF file:

    import weka.core.Instances;
    import java.io.BufferedReader;
    import java.io.FileReader;
    ...
    BufferedReader reader = new BufferedReader(new FileReader("data.arff"));
    Instances data = new Instances(reader);
    reader.close();
    data.setClassIndex(data.numAttributes() - 1);

The class index shows what attribute should be used for classification. In most ARFF files this is the last attribute which is why it is set to `data.numAttributes() - 1`. If you are using a Weka function, such as `buildClassifier`, you must set the class index.

# Weka >=3.5.5

In the latest version of Weka it is very easy to load an ARFF file. This method can also load CSV files and any other files Weka can understand.

    import weka.core.converters.ConverterUtils.DataSource;
    ...
    DataSource source = new DataSource("data.arff");
    Instances data = source.getDataSet();
    if (data.classIndex() == -1) {
      data.setClassIndex(data.numAttributes() - 1);
    }

## Loading from Database
Many databases can be used in Weka. Firstly, the DatabaseUtils.props file must be edited to match your database; specifically you must provide your database's name, location, port and correct driver.

    jdbcDriver=org.gjt.mm.mysql.Driver
    jdbcURL=jdbc:mysql://localhost:3306/my_database

Then the database can be loaded by using some simple code.

    import weka.core.Instances;
    import weka.experiment.InstanceQuery;
    ...
    InstanceQuery query = new InstanceQuery();
    query.setUsername("user");
    query.setPassword("pass");
    query.setQuery("select * from mytable");
    Instances data = query.retrieveInstances();

Some notes about loading from a database:

 - Make sure the correct JDBC driver is in your classpath.
 - If you are using Microsoft Access then the JDBC-ODBC-driver which comes with the JDK can be used.
 - The `InstanceQuery` method converts VARCHAR to nominal attributes and TEXT to string attributes. A filter, such as `NominalToString` or `StringToNormal`, can convert the attributes back to their correct type.

 

