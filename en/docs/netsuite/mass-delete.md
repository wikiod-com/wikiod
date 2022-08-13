---
title: "Mass Delete"
slug: "mass-delete"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

This sample shows how to mass delete records in NetSuite by leveraging the Mass Update feature. Typically, we're told not to delete records, but to make records inactive, but if you must, then this small script does just that. Once the script is deployed as a 'Mass Update' script type, simply go to Lists > Mass Update > Mass Updates > Custom Updates. You should see your mass delete. Next, set up your search criteria in your mass delete and do a preview to validate your data before deleting. 

## Delete based on Search Criteria
    /**
     *  NetSuite will loop through each record in your search 
     *  and pass the record type and id for deletion
     *  Try / Catch is useful if you wish to handle potential errors
     */
    
    function MassDelete(record_type, record_id)
    {
        try
        {
            nlapiDeleteRecord(record_type, record_id)
        }
        catch (err)
        {
            var errMessage = err;
            if(err instanceof nlobjError)
            {
                errMessage = errMessage + ' ' + err.getDetails() + ' ' + 'Failed to Delete ID : ' + record_id;
            }
            nlapiLogExecution('ERROR', 'Error', errMessage);
            return err
        }
    }

