---
title: "Displaying Google Analytics data in your Website"
slug: "displaying-google-analytics-data-in-your-website"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Displaying Google Analytics data in your Website
This document explains how to get Google Access tokens and use them to get Google Analytics data to be displayed in our websites.


**Example:** A live example is available in

> https://newtonjoshua.com

 note: Use the same gmail account for all the below steps.

----------

STEP 1: Set Up Google Analytics
-------------------------------

Follow the below steps to set up Google Analytics in your website

 1. Sign in to your Analytics account.
 2. Select the Admin tab.
 3. Select an account from the drop-down menu in the ACCOUNT column.
 4. Select a property from the drop-down menu in the PROPERTY column.
 5. Under PROPERTY, click Tracking Info > Tracking Code.
 6. To collect data, you must copy and paste the Analytics tracking code
    into the source code on every web page you wish to track.
 7. Once you have the Javascript tracking code snippet for your
    property, copy the snippet exactly without editing it.
 8. Paste your tracking code snippet (unaltered, in its entirety) before
    the closing </head> tag on every web page on your site you wish to
    track.
 9. Once you have successfully installed Analytics tracking, it may take
    up to 24 hours for data such as traffic referral information, user
    characteristics, and browsing information to appear in your reports

refer,

 1. https://support.google.com/analytics/answer/1008080?hl=en
 2. https://analytics.google.com

------------------

STEP 2: Get Tokens
------------------



**Google Project:**

To Create a Google Cloud Platform project, open the Google Developers Console ( https://console.developers.google.com )  and click *Create Project.*

**Enable OAuth 2.0 API access:**

Your app will need to access user data and contact other Google services on your behalf. Use OAuth 2.0 to grant your app API access.

To enable that, you need a Client ID:

 

 1. Open the Google API Console Credentials page (https://console.developers.google.com/apis/credentials).
 2. From the project
   drop-down, select your project. 
 3. Select *Create credential*s and choose    *OAuth client ID*.
 4. Under Application type, select *Web application*,    enter a Name and
 5. set
            the Restrictions by entering *JavaScript origins*, ***Redirect URIs*** to point the website where you are planning to display the data, then
    click *Create*.

  
 5. Make note of the
            OAuth 2.0 ***client_id*** and ***client_secret***. You will need    them to
            configure the UI.

**Get Authorization code:**

Enter in browser, 

> https://accounts.google.com/o/oauth2/auth?scope=https://www.googleapis.com/auth/analytics.readonly&response_type=code&client_id={{
> client_id}}&redirect_uri={{redirect_uri }}
> &approval_prompt=force&access_type=offline

You will get redirected to 

> {{redirect_uri }}?code=={{***authorization_code***}}#

**Get Refresh Token:**

Send a POST request, possibly via a REST console to

> https://www.googleapis.com/oauth2/v3/token?code={{authorization_code}}
> &client_id=*{{client_id}}*&client_secret=*{{client_secret}}*
> &redirect_uri=*{{redirect_uri }}*&grant_type=authorization_code

You will get a JSON response with 

> {"refresh_token": ***refresh_token***}

You can use the refresh toke to get access token to access to Google APIs

**Get Access Token:**

Send a POST request to,

> https://www.googleapis.com/oauth2/v3/token?client_id=*{{client_id}}*
> &client_secret=*{{client_id}}*
> &grant_type=refresh_token&refresh_token=*{{refresh_token}}*

   You will get a JSON with access_token in the response.

> {access_token: ***{{access_token}}***}


*Example:*

     var access_token = '';
    function getAccessToken(){
        $.post('https://www.googleapis.com/oauth2/v3/token', {
                client_id: {{client_id}},
                client_secret: {{client_secret}},
                grant_type: 'refresh_token',
                refresh_token: {{refresh_token}}
            }, function (data, status) {
                if (status === 'success') {
                    access_token = data.access_token;
                    // Do something eith the access_token
                } else {
                    console.error(status);
                }
            });
    }

**Check Token validity:**

Send a POST request to,
> https://www.googleapis.com/oauth2/v1/tokeninfo?access_token={{access_token}}

*Example:*

    function checkValidity() {
        $.post('https://www.googleapis.com/oauth2/v1/tokeninfo', {
                access_token:{{access_token}}
            }).done(function (data, status) {
                if (status === 'success') {
                    console.debug(data.expires_in);
                    var check = false;
                    check = data.hasOwnProperty('expires_in');
                    if (check) {
                        // Token is valid
                    }
                    if (!check) {
                        getAccessToken();
                    }
                } else {
                    console.debug(status);
                }
    
            })
            .fail(function (data) {
                console.error(data);
                getAccessToken();
            });
    }


----------

Step 3: Fetch Data
-----------------


**Embed API:**

The GA Embed API is a JavaScript library that allows you to easily create and embed your GA dashboard on your website in a matter of minutes.

refer https://developers.google.com/analytics/devguides/reporting/embed/v1/getting-started

**Query Explorer:**
visit Embed API Query Explorer and authorize
> https://ga-dev-tools.appspot.com/query-explorer/

Select the view for wich you want to fetch the data.

Select the required metrics and dimensions.

*Example:* 

Get Country Data (I want to know the number of users accessing my website from each country)

To get that data, select the metrics as 'users' and the dimensions as 'country'


Click on *Run Query*

You will find the analytics data for the query displayed in a table.

Copy the ***API Query URI***. And add access_token=*{{access_token}}* to the uri

*Example:*

> https://www.googleapis.com/analytics/v3/data/ga?ids={{ids}}&start-date=2015-07-01&end-date=today&metrics=ga%3Ausers&dimensions=ga%3A*country*&access_token=*{{access_token}}*



Send POST request to the URIs to get the data in your browser.

*Example:*

    function gaGetCountry() {
        $.get('https://www.googleapis.com/analytics/v3/data/ga?' +
            'ids={{ids}}' +
            'start-date=2015-07-01&' +
            'end-date=today&' +
            'metrics=ga%3Ausers&' +
            'dimensions=ga%3Acountry&' +
            'sort=ga%3Ausers&' +
            'filters=ga%3Ausers%3E10&' +
            'max-results=50' +
            '&access_token=' + {{access_token}},
            function (data, status) {
                if (status === 'success') {
   
                    // Display the Data
                    drawRegionsMap(data.rows);

                } else {
                    console.debug(status);
                }
    
            });
    }

----------


Step 4: Display Data
--------------------

Now we have gathered the data. Finally we have to diaplay them in our website.

"*Display live data on your site*" is the title of Google Charts. And that is what we are going to do.

refer https://developers.google.com/chart/

   The following example will draw a [GeoChart][1] in the div with id='countryChart'

     //Draw country Chart
     function drawRegionsMap(data) {

            var head = data[0];
            head[0] = 'Country';
            head[1] = 'Users';
            for (var i = 1; i < data.length; i++) {
                var d = data[i];
                d[1] = Number(d[1]);
            }

            var chartData = google.visualization.arrayToDataTable(data);
            var options = {
                title: 'My Website is viewed from,',
                domain: '{{Country Code eg: IN for India}}',
                tooltip: {
                    textStyle: {
                        color: 'navy'
                    },
                    showColorCode: true
                },
                legend: {
                    textStyle: {
                        color: 'navy',
                        fontSize: 12
                    }
                },
                colorAxis: {
                    colors: ['#00FFFF', '#0000FF']
                }
            };
        
            var chart = new google.visualization.GeoChart(document.getElementById('countryChart'));
        
            chart.draw(chartData, options); 
    }

Refer https://newtonjoshua.com  to view the above exple in action.

  [1]: https://developers.google.com/chart/interactive/docs/gallery/geochart

