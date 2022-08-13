---
title: "Using Restful web service in Liferay"
slug: "using-restful-web-service-in-liferay"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Consume Liferay JSON service for GET requests
Liferay exposes many default and custom services available to other systems via JSON. To explore services on a particular liferay instance, use a given URL - A local instance in this case:

    http://localhost:8080/api/jsonws/

[![enter image description here][1]][1]

Select the required service, consume the service with the given syntax and parameters:

    /user/get-user-by-email-address

Use `companyId and emailAddress` to retrieve the user with the expected datatypes, as well as possible exceptions to be handled by the consumer.

The following example consumes this service from a portlet. The given utility class  method makes a call to the webservice, passing the necessary arguments:

    import java.io.BufferedReader;
    import java.io.IOException;
    import java.io.InputStreamReader;
    import java.net.HttpURLConnection;
    import java.net.URL;
    
    import org.json.simple.JSONObject;
    import org.json.simple.parser.JSONParser;
    import org.json.simple.parser.ParseException;
    
    import sun.misc.BASE64Encoder;
    
    import com.liferay.portal.kernel.util.StringUtil;
    import com.liferay.portal.theme.ThemeDisplay;
    
    public class WebServiceUtil {
    
    public static String requestWebService(ThemeDisplay themeDisplay) {
        
        String url="http://localhost:8080/api/jsonws/user/get-user-by-email-address/company-id/{company-id}/email-address/{email-address}";
        
        String groupId= Long.toString(themeDisplay.getCompanyId());
        String userEmail="test@liferay.com";
        
        String[] searchList={"{company-id}","{email-address}"};
        String[] replList={groupId,userEmail};

        //Path params are replaced with args to make web service call
        url=StringUtil.replace(url, searchList, replList);
        
        System.out.println(url);
        StringBuilder sb = new StringBuilder();
        JSONObject jsonObject=new JSONObject();
        try
        {
            URL urlVal = new URL(url);
            HttpURLConnection conn = (HttpURLConnection) urlVal.openConnection();
            
            //The user credentials are directly used here only for the purpose of example,always fetech these details from an external props file.
            
            String uname ="test@liferay.com";
            String pswd="test";
            String authStr=uname+":"+pswd;
            
            //Encoding username+pswd to be added to request header for making web service             call
            String authStrEnc=new BASE64Encoder().encode(authStr.getBytes());

            /*Authorization type is set to consume web service
            and encoded combination is set in header to autheticate caller*/

            conn.setRequestMethod("GET");
            conn.setRequestProperty("Accept", "application/json");
            conn.setRequestProperty("Authorization", "Basic "+authStrEnc);
            
            BufferedReader brf = new BufferedReader(new InputStreamReader(conn.getInputStream()));
            
            JSONParser json=new JSONParser();
            jsonObject=(JSONObject)json.parse(brf);
            
            
            int cp;
            while ((cp = brf.read()) != -1) {
              sb.append((char) cp);
            }
        }
        catch(IOException e)
        {
            System.out.println("Something went wrong while reading/writing in stream!!");
        }
        catch (ParseException e) {
            System.out.println("Parse error");
        }
        
        //For purpose of simplicity we have fetched one of the fields from JSON response
        return (String)jsonObject.get("firstName");
        
    }

    }


  [1]: https://i.stack.imgur.com/08Jc9.png

