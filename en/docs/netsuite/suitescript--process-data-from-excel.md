---
title: "SuiteScript - Process Data from Excel"
slug: "suitescript---process-data-from-excel"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

Sometimes the returned search results in a Mass Update isn't the same as the results in a standard search, this is due to some limitations in a Mass Update Search. An example of this is Rev Rec Journal entries. Therefore, the workaround for this was to get the data from the standard saved search and use a script to read the excel data and update, as opposed to using the mass update feature. 

## Update Rev Rec Dates and Rule
    /**
     * Save the results from the saved search as .csv and store in file cabinet 
     * Get the file's internal id when loading the file
     * Use \n to process each row
     * Get the internal id and whatever columns that need updating
     * Create a filtered search and pass the internal id
     * If the passed in internal id finds a record match, then update the rev rec dates and rule
     */
    
    function ProcessSearchData()
    {
        var loaded_file = nlapiLoadFile(4954);//loads from file cabinet 
        var loaded_string = loaded_file.getValue();
        var lines = loaded_string.split('\n');//split on newlines
        nlapiLogExecution('DEBUG', 'lines', lines);
        var values;
        for (var i = 1; i < lines.length; i++)
        {
            nlapiLogExecution('DEBUG', 'count', i);
            values = lines[i].split(',');//split by comma
            var internal_id = values[0];//first column value
            nlapiLogExecution('DEBUG', 'internal_id', internal_id);
            var start_date = values[1];
            var end_date = values[2];
    
            if(internal_id)
            {
                UpdateDates(internal_id, start_date, end_date)
                nlapiLogExecution('DEBUG', '"""REV REC PLANs UPDATED"""');
            }
        }
        return true;
    }
    
    
    function UpdateDates(internal_id, start_date, end_date)
    {
        var filters = new Array();
        filters[0] = new nlobjSearchFilter('internalid', null, 'is', internal_id);
    
        var columns = [];
        columns[0] = new nlobjSearchColumn('internalid');
        columns[1] = new nlobjSearchColumn('revrecstartdate');
        columns[2] = new nlobjSearchColumn('revrecenddate');
    
        var rev_rec_plan = nlapiSearchRecord('revenueplan', null, filters, columns);
        if(rev_rec_plan)
        {
            for (var i = 0; rev_rec_plan != null && i < rev_rec_plan.length; i++)
            {
                var record = nlapiLoadRecord('revenueplan', rev_rec_plan[0].getValue(columns[0]));
                var id = record.getId();
                record.setFieldValue('revrecstartdate', start_date);
                record.setFieldValue('revrecenddate', end_date);
                record.setFieldValue('revenuerecognitionrule', 2)//Exact days based on Arrangement dates
                nlapiSubmitRecord(record);
            }
        }
        return internal_id;
    }



