---
title: "User Defined Aggregate Functions (UDAF)"
slug: "user-defined-aggregate-functions-udaf"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## UDAF mean example
- Create a Java class which extends `org.apache.hadoop.hive.ql.exec.hive.UDAF`
Create an inner class which implements `UDAFEvaluator`
 - Implement five methods
    
     - `init()` – This method initializes the evaluator and resets its internal state. We are using new Column() in the code below to indicate that no values have been aggregated yet.
     - `iterate()` – This method is called every time there is a new value to be aggregated. The evaluator should update its internal state with the result of performing the aggregation (we are doing sum – see below). We return true to indicate that the input was valid.
     - `terminatePartial()` – This method is called when Hive wants a result for the partial aggregation. The method must return an object that encapsulates the state of the aggregation.
    - `merge()` – This method is called when Hive decides to combine one partial aggregation with another.
    - `terminate()` – This method is called when the final result of the aggregation is needed.

        
        public class MeanUDAF extends UDAF {
        // Define Logging
        static final Log LOG = LogFactory.getLog(MeanUDAF.class.getName());
        public static class MeanUDAFEvaluator implements UDAFEvaluator {
        /**
         * Use Column class to serialize intermediate computation
         * This is our groupByColumn
         */
        public static class Column {
         double sum = 0;
         int count = 0;
         }
        private Column col = null;
        public MeanUDAFEvaluator() {
         super();
         init();
         }
        // A - Initalize evaluator - indicating that no values have been
        // aggregated yet.
        public void init() {
         LOG.debug("Initialize evaluator");
         col = new Column();
         }
        // B- Iterate every time there is a new value to be aggregated
         public boolean iterate(double value) throws HiveException {
         LOG.debug("Iterating over each value for aggregation");
         if (col == null)
         throw new HiveException("Item is not initialized");
         col.sum = col.sum + value;
         col.count = col.count + 1;
         return true;
         }
        // C - Called when Hive wants partially aggregated results.
         public Column terminatePartial() {
         LOG.debug("Return partially aggregated results");
         return col;
         }
         // D - Called when Hive decides to combine one partial aggregation with another
         public boolean merge(Column other) {
         LOG.debug("merging by combining partial aggregation");
         if(other == null) {
         return true;
         }
         col.sum += other.sum;
         col.count += other.count;
         return true; 
        }
         // E - Called when the final result of the aggregation needed.
         public double terminate(){
         LOG.debug("At the end of last record of the group - returning final result"); 
         return col.sum/col.count;
         }
         }
        }


        hive> CREATE TEMPORARY FUNCTION <FUNCTION NAME> AS 'JAR PATH.jar';
        hive> select id, mean_udf(amount) from table group by id;




