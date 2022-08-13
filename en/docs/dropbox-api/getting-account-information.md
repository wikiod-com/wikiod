---
title: "Getting account information"
slug: "getting-account-information"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

## Getting account information for the linked user via curl
    curl -X POST https://api.dropboxapi.com/2/users/get_current_account \
        --header "Authorization: Bearer <ACCESS_TOKEN>"

 `<ACCESS_TOKEN>` should be replaced with your access token.


## Getting account information for the linked user via curl in C++
    #include <stdio.h>
    #include <curl/curl.h>
    
    
    int main (int argc, char *argv[])
    {
          CURL *curl;
          CURLcode res;
    
          /* In windows, this will init the winsock stuff */ 
          curl_global_init(CURL_GLOBAL_ALL);
         
          /* get a curl handle */ 
          curl = curl_easy_init();
          if(curl) {
    
                printf ("Running curl test.\n");
    
                struct curl_slist *headers=NULL; /* init to NULL is important */
                headers = curl_slist_append(headers, "Authorization: Bearer <ACCESS_TOKEN>");
                headers = curl_slist_append(headers, "Content-Type: application/json");
                curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
                 
                curl_easy_setopt(curl, CURLOPT_URL, "https://api.dropbox.com/2/users/get_current_account");
                curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "null");
    
                /* Perform the request, res will get the return code */ 
                res = curl_easy_perform(curl);
    
                /* Check for errors */ 
                if(res != CURLE_OK)
                      fprintf(stderr, "curl_easy_perform() failed: %s\n",
                        curl_easy_strerror(res));
    
                /* always cleanup */
                  curl_easy_cleanup(curl);
    
                  printf ("\nFinished curl test.\n");
    
          }
          curl_global_cleanup();
    
          printf ("Done!\n");
          return 0;
    
    }

 `<ACCESS_TOKEN>` should be replaced with your access token.


## Getting account information using the Dropbox Python library
This uses the [Dropbox Python SDK][1] to get the user's account information from the Dropbox API.
 

    import dropbox
    dbx = dropbox.Dropbox("<ACCESS_TOKEN>")
    dbx.users_get_current_account()  
     

`<ACCESS_TOKEN>` should be replaced with the access token.
 
 
 [1]: https://github.com/dropbox/dropbox-sdk-python

## Getting space usage information for the linked user via curl in PHP
    <?php
    
    $headers = array("Authorization: Bearer <ACCESS_TOKEN>",
                     "Content-Type: application/json");
    
    $ch = curl_init('https://api.dropboxapi.com/2/users/get_space_usage');
    curl_setopt($ch, CURLOPT_HTTPHEADER, $headers);
    curl_setopt($ch, CURLOPT_POST, true);
    curl_setopt($ch, CURLOPT_POSTFIELDS, "null");
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
    $response = curl_exec($ch);
    
    curl_close($ch);
    echo $response;
    
    ?>

`<ACCESS_TOKEN>` should be replaced with the OAuth 2 access token.

## Getting account information for the linked user via HttpWebRequest in PowerShell
    $url = "https://api.dropboxapi.com/2/users/get_current_account"
    
    $req = [System.Net.HttpWebRequest]::Create($url)
    $req.headers["Authorization"] = "Bearer <ACCESS_TOKEN>"
    $req.Method = "POST"
    
    $res = $req.GetResponse()
    Write-Host "Response Status Code: "$res.StatusCode
    Write-Host "Response Status Description: "$res.StatusDescription
    $readStream = new-object System.IO.StreamReader $res.GetResponseStream()
    $result = $readStream.ReadToEnd() | ConvertFrom-Json
    Write-Host $result
    $readStream.Close()
    $res.Close()

`<ACCESS_TOKEN>` should be replaced with your access token.


##  Getting account information via jQuery in JavaScript
    jQuery.ajax({
        url: 'https://api.dropboxapi.com/2/users/get_current_account',
        type: 'POST',
        headers: {
            "Authorization": "Bearer <ACCESS_TOKEN>"
        },
        success: function (data) {
            console.log(data);
        },
        error: function (error) {
            console.log(error);
        }
    })

`<ACCESS_TOKEN>` should be replaced with the OAuth 2 access token.


## Getting account information using the Dropbox Objective-C library
This uses the [Dropbox Objective-C SDK][1] to get the user's account information from the Dropbox API.

    [[client.usersRoutes getCurrentAccount] response:^(DBUSERSFullAccount *account, DBNilObject *_, DBRequestError *error) {
        if (account) {
            NSLog(@"%@", account);
        } else if (error) {
            NSLog(@"%@", error);
        }
    }];


 [1]: https://github.com/dropbox/dropbox-sdk-obj-c


