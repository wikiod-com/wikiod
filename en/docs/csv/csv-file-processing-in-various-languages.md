---
title: "CSV file processing in various languages"
slug: "csv-file-processing-in-various-languages"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Small, simple csv files can be built using just a text editor. Reading and writing them, or otherwise processing their contents is done more efficiently using the products available for one's language or systems of choice.

## Reading and writing in Python
CSV(Comma Separated Values) is a simple file format used to store tabular data, such as a spreadsheet.
This is a minimal example of how to write & read data in Python.

Writing data to a CSV file:

<!-- language: lang-python -->

    import csv
    data = [["Ravi", "9", "550"], ["Joe", "8", "500"], ["Brian", "9", "520"]]
    with open('students.csv', 'wb') as csvfile:
        writer = csv.writer(csvfile, delimiter=',')
        writer.writerows(data)

Reading data from a CSV file:

<!-- language: lang-python -->

    import csv
    with open('students.csv', 'rb') as csvfile:
        spamreader = csv.reader(csvfile, delimiter=',',)
        for row in spamreader:
            print("Name: {} class: {} Marks: {}".format(row[0], row[1], row[2]))
    output:
    Name: Ravi class: 9 Marks: 550
    Name: Joe class: 8 Marks: 500
    Name: Brian class: 9 Marks: 520


## Reading and writing in Java
Below example shows ways to read and write csv file without any third party libraries.

**Write CSV**

<!-- language: lang-java -->

    public void writeToCsvFile(List<String[]> thingsToWrite, String separator, String fileName){
        try (FileWriter writer = new FileWriter(fileName)){
            for (String[] strings : thingsToWrite) {
                for (int i = 0; i < strings.length; i++) {
                    writer.append(strings[i]);
                    if(i < (strings.length-1))
                        writer.append(separator);
                }
                writer.append(System.lineSeparator());
            }
            writer.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

**Read CSV**

<!-- language: lang-java -->

    // Allows to define custom separator
    public List<String[]> readFromCsvFile(String separator, String fileName){
        try (BufferedReader reader = new BufferedReader(new FileReader(fileName))){
            List<String[]> list = new ArrayList<>();
            String line = "";
            while((line = reader.readLine()) != null){
                String[] array = line.split(separator);
                list.add(array);
            }
            return list;
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }  
    }

There are also some pre-compiled third party libraries which provide convenient ways to parse csv files. Below are some examples of such libraries.

**OpenCSV**

OpenCSV is considered very simple to use and provides flexible functionalities while parsing CSV files

<!-- language: lang-java -->

    /** Reading CSV **/
    // Allows varied parameters through constructors to define quote character, number of lines to skip, etc.
    try(CSVReader reader = new CSVReader(new FileReader("yourfile.csv"), separator)){
            List<String[]> = reader.readAll();
            // Do something with the data
    }

    /** Writing CSV **/
    List<String[]> listToWrite= //fetch the list of string array to write;
    try(CSVWriter writer = new CSVWriter(new FileWriter(fileName), separator)){
        writer.writeAll(listToWrite);
        writer.flush();
    }
    
    /** Dumping database records to CSV **/
    // Initialize CSVWriter and fetch resultSet from database ...
        writer.writeAll(resultSet, includeColumnNames); 

OpenCSV also allowes binding the records directly to JavaBeans. for more information refer official documentation [here][1].


Other known libraries include [SuperCSV][2] and [CommonsCSV][3] which provide some advanced functionalities as-well. Refer to official documentation for more information.


  [1]: http://opencsv.sourceforge.net/
  [2]: http://super-csv.github.io/super-csv/index.html
  [3]: https://commons.apache.org/proper/commons-csv/user-guide.html

