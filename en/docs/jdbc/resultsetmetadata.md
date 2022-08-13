---
title: "ResultSetMetaData"
slug: "resultsetmetadata"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

As we all know Metadata mean data about data.

To fetch metadata of a table like total number of column, column name, column type etc. , ResultSetMetaData interface is useful because it provides methods to get metadata from the ResultSet object.



## ResultSetMetaData


    import java.sql.*;
    
    class Rsmd {
    
        public static void main(String args[]) {
            try {
                Class.forName("oracle.jdbc.driver.OracleDriver");
                Connection con = DriverManager.getConnection(
                        "jdbc:oracle:thin:@localhost:1521:xe", "system", "oracle");
    
                PreparedStatement ps = con.prepareStatement("select * from emp");
                ResultSet rs = ps.executeQuery();
                ResultSetMetaData rsmd = rs.getMetaData();
    
                System.out.println("Total columns: " + rsmd.getColumnCount());
                System.out.println("Column Name of 1st column: " + rsmd.getColumnName(1));
                System.out.println("Column Type Name of 1st column: " + rsmd.getColumnTypeName(1));
    
                con.close();
            } catch (Exception e) {
                System.out.println(e);
            }
        }
    }



