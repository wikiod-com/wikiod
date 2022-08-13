---
title: "Configure a Web API application to respond with prettyformatted JSON data by defualt"
slug: "configure-a-web-api-application-to-respond-with-prettyformatted-json-data-by-defualt"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Default JSON formatting: Efficiency at the cost of readability
Lets say you have a simple ApiController like this:

        [HttpGet]
        [Route("test")]
        public dynamic Test()
        {
            dynamic obj = new ExpandoObject();
            obj.prop1 = "some string";
            obj.prop2 = 11;
            obj.prop3 = "another string";

            return obj;
        }

The resulting JSON representation of this object will look like this:

    {"prop1":"some string","prop2":11,"prop3":"another string"}

This is probably fine for simple responses like this, but imagine if you have a large/complex object sent as the response:


    "response": { "version": "0.1", "termsofService": "http://www.wunderground.com/weather/api/d/terms.html", "features": { "history": 1 } }, "history": { "date": { "pretty": "July 16, 2016", "year": "2016", "mon": "07", "mday": "16", "hour": "12", "min": "00", "tzname": "America/Indianapolis" }, "utcdate": { "pretty": "July 16, 2016", "year": "2016", "mon": "07", "mday": "16", "hour": "16", "min": "00", "tzname": "UTC" }, "observations": [{ "date": { "pretty": "12:15 AM EDT on July 16, 2016", "year": "2016", "mon": "07", "mday": "16", "hour": "00", "min": "15", "tzname": "America/Indianapolis" }, "utcdate": { "pretty": "4:15 AM GMT on July 16, 2016", "year": "2016", "mon": "07", "mday": "16", "hour": "04", "min": "15", "tzname": "UTC" }, "tempm": "18.2", "tempi": "64.8", "dewptm": "16.4", "dewpti": "61.5", "hum": "89", "wspdm": "9.3", "wspdi": "5.8", "wgustm": "-9999.0", "wgusti": "-9999.0", "wdird": "20", "wdire": "NNE", "vism": "16.1", "visi": "10.0", "pressurem": "1018.2", "pressurei": "30.07", "windchillm": "-999", "windchilli": "-999", "heatindexm": "-9999", "heatindexi": "-9999", "precipm": "-9999.00", "precipi": "-9999.00", "conds": "Clear", "icon": "clear", "fog": "0", "rain": "0", "snow": "0", "hail": "0", "thunder": "0", "tornado": "0", "metar": "METAR KTYQ 160415Z AUTO 02005KT 10SM CLR 18/16 A3007 RMK AO2 T01820164" }, { "date": { "pretty": "12:35 AM EDT on July 16, 2016", "year": "2016", "mon": "07", "mday": "16", "hour": "00", "min": "35", "tzname": "America/Indianapolis" }, "utcdate": { "pretty": "4:35 AM GMT on July 16, 2016", "year": "2016", "mon": "07", "mday": "16", "hour": "04", "min": "35", "tzname": "UTC" }, "tempm": "17.7", "tempi": "63.9", "dewptm": "16.3", "dewpti": "61.3", "hum": "91", "wspdm": "7.4", "wspdi": "4.6", "wgustm": "-9999.0", "wgusti": "-9999.0", "wdird": "10", "wdire": "North", "vism": "16.1", "visi": "10.0", "pressurem": "1018.2", "pressurei": "30.07", "windchillm": "-999", "windchilli": "-999", "heatindexm": "-9999", "heatindexi": "-9999", "precipm": "-9999.00", "precipi": "-9999.00", "conds": "Clear", "icon": "clear", "fog": "0", "rain": "0", "snow": "0", "hail": "0", "thunder": "0", "tornado": "0", "metar": "METAR KTYQ 160435Z AUTO 01004KT 10SM CLR 18/16 A3007 RMK AO2 T01770163" } } }

That isn't what you would consider highly readable data.  This is easily solved by setting setting a single property on the default JsonFormatter in App_Start/ApiConfig.cs: 

        // Either one of these will format your JSON in a readable format.  Setting both provides no additional benefit.
        config.Formatters.JsonFormatter.SupportedMediaTypes.Add(new MediaTypeHeaderValue("text/html"));
        // OR
        config.Formatters.JsonFormatter.Indent = true;

