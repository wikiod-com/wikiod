---
title: "Tabular Forms"
slug: "tabular-forms"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Process a Tabular Form
When a tabular form is posted, the items in each record are available to PL/SQL code via the "APEX_APPLICATION.G_Fnn" arrays. G_F01 corresponds to the first editable column, G_F02 to the 2nd, and so on. For example, you can loop through each record and access the posted values for each field as follows:

    FOR i IN 1..APEX_APPLICATION.G_F01.COUNT LOOP 
      htp.p('Column A row '||I||' has a value of '||APEX_APPLICATION.G_F01(i)); 
      htp.p('Column B row '||I||' has a value of '||APEX_APPLICATION.G_F02(i)); 
    END LOOP;

If the first column is a checkbox, only checkboxes that were checked will have an entry in the array:

    FOR i IN 1..APEX_APPLICATION.G_F01.COUNT LOOP 
      htp.p('Checkbox on row '||APEX_APPLICATION.G_F01(i)||' was checked');
      htp.p('Column B on that row has a value of '
            ||APEX_APPLICATION.G_F02(APEX_APPLICATION.G_F01(i))); 
    END LOOP;


