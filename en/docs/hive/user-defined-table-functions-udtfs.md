---
title: "User Defined Table Functions (UDTF's)"
slug: "user-defined-table-functions-udtfs"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## UDTF Example and Usage
User defined table functions represented by **org.apache.hadoop.hive.ql.udf.generic.GenericUDTF** interface. This function  allows to output multiple rows and multiple columns for a single input.

We have to overwrite below methods :

     1.we specify input and output parameters
    abstract StructObjectInspector initialize(ObjectInspector[] args) 
                                                    throws UDFArgumentException; 
    
     2.we process an input record and write out any resulting records 
    abstract void process(Object[] record) throws HiveException;
    
    3.function is Called to notify the UDTF that there are no more rows to process. 
        Clean up code or additional output can be produced here.
    abstract void close() throws HiveException;




    import java.util.ArrayList;
    import java.util.Iterator;
    import java.util.List;
    
    import org.apache.hadoop.hive.ql.exec.UDFArgumentException;
    import org.apache.hadoop.hive.ql.metadata.HiveException;
    import org.apache.hadoop.hive.ql.udf.generic.GenericUDTF;
    import org.apache.hadoop.hive.serde2.objectinspector.ObjectInspector;
    import org.apache.hadoop.hive.serde2.objectinspector.ObjectInspectorFactory;
    import org.apache.hadoop.hive.serde2.objectinspector.PrimitiveObjectInspector;
    import org.apache.hadoop.hive.serde2.objectinspector.StructObjectInspector;
    import org.apache.hadoop.hive.serde2.objectinspector.primitive.PrimitiveObjectInspectorFactory;
    
    public class NameParserGenericUDTF extends GenericUDTF {
          private PrimitiveObjectInspector stringOI = null;
          
           //Defining input argument as string.
          @Override
          public StructObjectInspector initialize(ObjectInspector[] args) throws UDFArgumentException {
            if (args.length != 1) {
              throw new UDFArgumentException("NameParserGenericUDTF() takes exactly one argument");
            }
    
            if (args[0].getCategory() != ObjectInspector.Category.PRIMITIVE
                && ((PrimitiveObjectInspector) args[0]).getPrimitiveCategory() != PrimitiveObjectInspector.PrimitiveCategory.STRING) {
              throw new UDFArgumentException("NameParserGenericUDTF() takes a string as a parameter");
            }
            
            // input
            stringOI = (PrimitiveObjectInspector) args[0];
    
            // output 
            List<String> fieldNames = new ArrayList<String>(2);
            List<ObjectInspector> fieldOIs = new ArrayList<ObjectInspector>(2);
            fieldNames.add("name");
            fieldNames.add("surname");
            fieldOIs.add(PrimitiveObjectInspectorFactory.javaStringObjectInspector);
            fieldOIs.add(PrimitiveObjectInspectorFactory.javaStringObjectInspector);
            return ObjectInspectorFactory.getStandardStructObjectInspector(fieldNames, fieldOIs);
          }
                
          public ArrayList<Object[]> processInputRecord(String name){
                ArrayList<Object[]> result = new ArrayList<Object[]>();
              
                // ignoring null or empty input
                if (name == null || name.isEmpty()) {
                  return result;
                }
                
                String[] tokens = name.split("\\s+");
                
                if (tokens.length == 2){
                    result.add(new Object[] { tokens[0], tokens[1] });
                }else if (tokens.length == 4 && tokens[1].equals("and")){
                    result.add(new Object[] { tokens[0], tokens[3] });
                    result.add(new Object[] { tokens[2], tokens[3] });
                }
                
                return result;
          }
          
          @Override
          public void process(Object[] record) throws HiveException {
            final String name = stringOI.getPrimitiveJavaObject(record[0]).toString();
            ArrayList<Object[]> results = processInputRecord(name);
            
            Iterator<Object[]> it = results.iterator();
            
            while (it.hasNext()){
                Object[] r = it.next();
                forward(r);
            }
          }
    
          @Override
          public void close() throws HiveException {
            // do nothing
          }
    }


Package the code into jar and need to add jar to hive context.

    hive> CREATE TEMPORARY FUNCTION process_names as 'jar.path.NameParserGenericUDTF'; 
    
    Here we will pass input as full name and break it into first and last name.

    hive> SELECT 
       t.name,
       t.surname 
    FROM people 
        lateral view process_names(name) t as name, surname;

    Teena Carter
    John Brownewr



