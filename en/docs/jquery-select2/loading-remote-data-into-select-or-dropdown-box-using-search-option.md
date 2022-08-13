---
title: "Loading remote data into Select or DropDown box using Search option"
slug: "loading-remote-data-into-select-or-dropdown-box-using-search-option"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

When the record count is too high , loading all records at once can make application slow in addition user will  not like the idea of scrolling thousand of records to find what he is looking for. Its better to give user a power search and filter the records as he types the character.

## Creating searchable Dropdown / Select box using Jquery - select2 in C# MVC
This example will demonstrate the searchable select box in MVC. it uses Ajax to get all records from database as user types the new character. 
I'll consider Country and  its City example to illustrate the functionality of Searchable dropdown box. Code behind is c# with MVC, but its easy to grasp whole concept.

First I have created a basic background in step wise manner. Last steps is Select2 jqery method which execute this code.

**Step 1:** Declare DropDownListFor control in razor. It creates Select box in HTML mark up. 

The hidden variable "ajaxUrlGetAutoCompleteSearchSuggestion" will be used by Ajax call at  Jquery - select2 library method. 

Hidden variable Value field also contains razor library method url.action. it has two parameters. First parameter,  GetAutoCompleteSearchSuggestion is C# method, which is the entry point at server side for Ajax call to fetch the records. Second parameter is controller name "Country".


      <h2> 
                    <label>@Html.LabelFor(m => m.ddlCountry)</label>
                    @Html.DropDownListFor(m => m.ddlCountry, Model.Station, new { @class = "select2Style" })
    </h2>

    <input type="hidden" id="ajaxUrlGetAutoCompleteSearchSuggestion" value='@Url.Action("GetAutoCompleteSearchSuggestion", "Country")' />

**Step 2:** These are Predefined CSS class available for Select box. You can customized the control using these classes. In addition you can add your own css class for controls.

    /* Input field */
    .select2-selection__rendered { font-size:medium; font-weight:normal; }
    
    /* Around the search field */
    .select2-search { font-size:small; font-weight:normal;  }
    
    /* Search field */
    .select2-search input { font-size:medium; font-weight:normal; }
    
    /* Each result */
    .select2-results {
        font-family: Arial, Helvetica, sans-serif;
        font-size: medium;
         font-weight:normal;
        
      }
    
    /* Higlighted (hover) result */
    .select2-results__option--highlighted { font-size:medium;  font-weight:normal; }
    
    /* Selected option */
    .select2-results__option[aria-selected=true] { 
         background: #3ea211;
         font-family: Arial, Helvetica, sans-serif;
        font-size:medium;
        font-weight:normal;   
     }
    
    /* My css class*/
    .select2Style {
        width:200px;
    }


**Step 3:**  Create the Model. Note that same variable name is in razor declaration.
   
    [Display(Name = "Country:")]
    public string ddlCountry { get; set; }
    public IEnumerable<SelectListItem> Country { get; set; }

**Step 4:** Create "Country" Controller and C# method which will be called by jquery ajax method every time user enters a text in searchable dropdown's textbox.
  
    public ActionResult GetAutoCompleteSearchSuggestion(CountryQuery queryParameters)
            {
                string ErrorMessage = "success";
                Dictionary<double, string> suggestions = new Dictionary<double, string>();
                 
                try
                {   
            suggestions =  GetCountryList( queryParameters.query);
                }
                catch (Exception Ex)
                {
                    ErrorMessage = Ex.Message;               
                    return Json(ErrorMessage);
                }
                return Json(suggestions.Select(c => new { Name = c.Value, ID = c.Key }).ToList(), 
        JsonRequestBehavior.AllowGet);
                }

**Step 5:** Set the database query and operation to fetch records. it gets the list of country. please note the db query, it affects the way item for dropdown is fetched and displayed. you'll have to customize your query to fetch the result according to your requirement.

     public Dictionary<string, string> GetCountryList(string filterId)
            {
                Dictionary<string, string> ddlcountryDictionary = new Dictionary<string, string>();
                OracleDataReader reader = null;
                OracleConnection oraConnection = new OracleConnection(oracleConnStr);
                string firstItem = string.Empty;
                try
                { 
                    oraConnection.Open();
                    string ddlQuery = "SELECT CountryId, countryName FROM tblCountry WHERE UPPER(countryName) LIKE UPPER ('"+filterId+"%')  ORDER BY 2 ASC";
                    oraCommand = new OracleCommand(ddlQuery, oraConnection);
                    reader = oraCommand.ExecuteReader();
                    string countryValue = "Select Item";
                    string countryId = -1;
                 
                    if (reader.HasRows)
                    {
                        while (reader.Read())
                        {
                            countryId =  reader[0].ToString();
                            countryValue = reader[1].ToString();                        
                            ddlcountryDictionary.Add(countryId, countryValue);
                        }
                    }                
                }
                catch (Exception ex)
                {
                     
                    throw new Exception("No Country name Exists.");
                }
                finally
                {
                    reader.Dispose();
                    oraCommand.Dispose();
                    oraConnection.Close();
                }             
                return ddlcountryDictionary;
            }

**Step 6:**  Jquery function will list country name starting with the entered text by user, if user select **"a"** then all country with starting with name **"a"** will be listed and next if user types **"n"** then country name **not** starting with **"an"** will be filtered out.
  

     $("#ddlCountry").select2({
            ajax: {
                url: $("#ajaxUrlGetAutoCompleteSearchSuggestion").val(),
                data: function (params) {
                    var queryParameters = {  
                        //restrictedCountry: $("#resCountry").val(),  // pass your own parameter                
                        query: params.term, // search term like "a" then "an"
                        page: params.page
                    };
                    return queryParameters;
                },
                dataType: "json",
                cache: true,
                delay: 250,
                //type: 'POST',
                contentType: "application/json; charset=utf-8",
                processResults: function (data, params) {
                    params.page = params.page || 1;
                    return {
                        results: $.map(data, function (val, item) {
                            return { id: val.ID, text: val.Name };
                        }),
                       // if more then 30 items in dropdown, remaining set of items  will be show on numbered page link in dropdown control.
                        pagination: { more: (params.page * 30) < data.length }
                    };
                }
            },
            minimumInputLength: 1 // Minimum length of input in search box before ajax call triggers
        });


Please go through the various example of Select2 at [this][1] site 


  [1]: https://select2.github.io/examples.html

## Cascade dropdown / Select box in Jquery - Select2
This is continuation of previous example.

Cascading DropDown for country's city name. This method will be called by Jquery when user is done with country selection in parent dropdown. I have followed MVC concept and provided the basic approach for cascading dropdown. 

Ajax will call GetCityName method on code behind on Server and received information is recursively used to create City dropdown.

Please note the syntax of Select2 for cascade dropdown.  

 
    $('#ddlCountry').on("select2:select", function (event) {
            var countryParam =
                {                    
                    "countryId": $("#ddlCountry option:selected").val()
                };
            $.ajax({
                url: $("#ajaxUrlGetCityName").val(),
                data: JSON.stringify({ ddlParams: countryParam}),
                type: 'POST',
                cache: false,
                contentType: 'application/json; charset=utf-8',
                dataType: "json",
                success: function (result) {
                    var markup;
                    var dbSelect = $('#ddlCity');
                    dbSelect.empty();
                    for (var i = 0; i < result.length; i++) {
                        dbSelect.append($('<option/>', {
                            value: result.City[i].Value,
                            text: result.City[i].Text
                        }));
                    }
                },
                error: function (xhr, ajaxOptions, thrownError) {
                    alert(thrownError);
                }
            });
        }); 

