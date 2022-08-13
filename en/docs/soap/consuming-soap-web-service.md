---
title: "Consuming SOAP Web Service"
slug: "consuming-soap-web-service"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

This section should provide details of all the possible ways to consume a SOAP web service.

## Parameters
| Parameter   | Details           |
| ----------- | ----------------- |
| CountryName | String such as UK |

## Without creating Stub or Java files
    public String getCitiesByCountry(String countryName) throws MalformedURLException, IOException {
     
        //Code to make a webservice HTTP request
        String responseString = "";
        String outputString = "";
        String wsURL = "http://www.webservicex.com/globalweather.asmx";// Endpoint of the webservice to be consumed
        URL url = new URL(wsURL);
        URLConnection connection = url.openConnection();
        HttpURLConnection httpConn = (HttpURLConnection)connection;
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        String xmlInput = 
            "<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"         xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">
              <soap:Body>
                <GetCitiesByCountry xmlns=\"http://www.webserviceX.NET\">
                  <CountryName>" + countryName + "</CountryName>
                </GetCitiesByCountry>
              </soap:Body>
            </soap:Envelope>"; //entire SOAP Request
 
        byte[] buffer = new byte[xmlInput.length()];
        buffer = xmlInput.getBytes();
        bout.write(buffer);
        byte[] b = bout.toByteArray();
        String SOAPAction = "http://www.webserviceX.NET/GetCitiesByCountry"; // SOAP action of the webservice to be consumed
        // Set the appropriate HTTP parameters.
        httpConn.setRequestProperty("Content-Length",
        String.valueOf(b.length));
        httpConn.setRequestProperty("Content-Type", "text/xml; charset=utf-8");
        httpConn.setRequestProperty("SOAPAction", SOAPAction);
        httpConn.setRequestMethod("POST");
        httpConn.setDoOutput(true);
        httpConn.setDoInput(true);
        OutputStream out = httpConn.getOutputStream();
        //Write the content of the request to the outputstream of the HTTP Connection.
        out.write(b);
        out.close();
        //Ready with sending the request.
         
        //Read the response.
        InputStreamReader isr = null;
        if (httpConn.getResponseCode() == 200) {
          isr = new InputStreamReader(httpConn.getInputStream());
        } else {
          isr = new InputStreamReader(httpConn.getErrorStream());
        }
        
        BufferedReader in = new BufferedReader(isr);
         
        //Write the SOAP message response to a String.
        while ((responseString = in.readLine()) != null) {
            outputString = outputString + responseString;
        }
        //Parse the String output to a org.w3c.dom.Document and be able to reach every node with the org.w3c.dom API.
        Document document = parseXmlFile(outputString);
        NodeList nodeLst = document.getElementsByTagName("GetCitiesByCountryResult"); // TagName of the element to be retrieved
        String elementValue = nodeLst.item(0).getTextContent();
        System.out.println(elementValue);
        
        return elementValue;
    }

    public Document parseXmlFile(String in) {
        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            InputSource is = new InputSource(new StringReader(in));
            return db.parse(is);
        } catch (ParserConfigurationException e) {
            throw new RuntimeException(e);
        } catch (SAXException e) {
            throw new RuntimeException(e);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

