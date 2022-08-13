---
title: "How to update second datepicker from first datepicker and value of dropdown field"
slug: "how-to-update-second-datepicker-from-first-datepicker-and-value-of-dropdown-field"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

This example has a datepicker for the 'Start Date', an input for the 'Duration' (in weeks) and a second datepicker that is disabled for user input as it is updated on change of either the first datepicker or the duration inputs.

## Example
**HTML HEAD**

    <!-- JQuery -->
    <link rel="stylesheet" href="//code.jquery.com/ui/1.12.0/themes/base/jquery-ui.css" />
    <script src="https://code.jquery.com/jquery-1.12.4.js" type="text/javascript"></script>
    <script src="https://code.jquery.com/ui/1.12.0/jquery-ui.js" type="text/javascript"></script>

**HTML BODY**

    <form>
        <label for="startDate">Start Date</label>
        <input placeholder="Start Date" type="text" id="startDate" name="startDate">
                            
        <label for="duration">Duration (Weeks)</label>
        <select id="duration" name="duration">
            <option value="1">1</option>
            <option value="2">2</option>
            <option value="3">3</option>
            <option value="4">4</option>
            <option value="5">5</option>
            <option value="6">6</option>
            <option value="7">7</option>
            <option value="8">8</option>
            <option value="9">9</option>
            <option value="10">10</option>
            <option value="11">11</option>
            <option value="12">12</option>
        </select>
                            
        <label for="endDate">End Date</label>
        <input type="text" value="" id="endDate" disabled="disabled" />
    </form>

**Javascript**

    <script> 
    $(document).ready(function() {
                        
        $('#endDate').datepicker({
            dateFormat: "dd/mm/yy"
        });
                        
        $('#startDate').datepicker( {
            dateFormat: "dd/mm/yy",
            showOn: "both",
            buttonImage: "https://jqueryui.com/resources/demos/datepicker/images/calendar.gif",
            buttonImageOnly: true,
            minDate: 0,
            onSelect: function(dateStr) {
                var weeks = $('#duration').val();
                var newDate = $('#startDate').datepicker('getDate');
                var days = weeks * 7;
                newDate.setDate(newDate.getDate()+days);
                $('#endDate').datepicker('setDate', newDate);
            }
        });
                            
        $("#startDate").datepicker("setDate", new Date());
                        
        $("#duration").change(function(){
            var weeks = $('#duration').val();
            var newDate2 = $('#startDate').datepicker('getDate');
                            var days = weeks * 7;
                            newDate2.setDate(newDate2.getDate()+days);
                            $('#endDate').datepicker('setDate', newDate2);
        });
    });
    </script>


