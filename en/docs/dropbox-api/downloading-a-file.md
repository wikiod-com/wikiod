---
title: "Downloading a file"
slug: "downloading-a-file"
draft: false
images: []
weight: 9525
type: docs
toc: true
---

## Downloading a file using the Dropbox Python library
This uses the [Dropbox Python SDK][1] to download a file from the Dropbox API at the remote path `/Homework/math/Prime_Numbers.txt` to the local file `Prime_Numbers.txt`:

    import dropbox
    dbx = dropbox.Dropbox("<ACCESS_TOKEN>")
    
    with open("Prime_Numbers.txt", "wb") as f:
        metadata, res = dbx.files_download(path="/Homework/math/Prime_Numbers.txt")
        f.write(res.content)

`<ACCESS_TOKEN>` should be replaced with your access token.


  [1]: https://github.com/dropbox/dropbox-sdk-python

## Downloading a piece of a file via curl using Range Retrieval Requests
This downloads just a piece of a file, using [Range Retrieval Requests][1], from the Dropbox API at the remote path `/Homework/math/Prime_Numbers.txt` to the local path `Prime_Numbers.txt.partial` in the current folder:
 

     curl -X GET https://content.dropboxapi.com/2/files/download \
         --header "Authorization: Bearer <ACCESS_TOKEN>" \
         --header "Dropbox-API-Arg: {\"path\": \"/Homework/math/Prime_Numbers.txt\"}" \
         --header "Range:bytes=0-10" \
         -o "./Prime_Numbers.txt.partial"
         
The range specified, `0-10`, tells the API to return just the first 10 bytes. If the API responds with a 206 status code, that indicates that the Range was accepted, and only the partial request range was returned.

 `<ACCESS_TOKEN>` should be replaced with your access token.


  [1]: https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.2

## Downloading a file via curl
This downloads a file from the Dropbox API at the remote path `/Homework/math/Prime_Numbers.txt` to the local path `Prime_Numbers.txt` in the current folder:

    curl -X POST https://content.dropboxapi.com/2/files/download \
        --header "Authorization: Bearer <ACCESS_TOKEN>" \
        --header "Dropbox-API-Arg: {\"path\": \"/Homework/math/Prime_Numbers.txt\"}" \
        -o "./Prime_Numbers.txt"

`<ACCESS_TOKEN>` should be replaced with your access token.

## Downloading a file with progress information using the SwiftyDropbox library
Adapted from the [tutorial][1], this uses the [SwiftyDropbox library][2] to download a file, with a progress callback on the download method to get progress information:

    // Download a file
    let destination : (NSURL, NSHTTPURLResponse) -> NSURL = { temporaryURL, response in
        let fileManager = NSFileManager.defaultManager()
        let directoryURL = fileManager.URLsForDirectory(.DocumentDirectory, inDomains: .UserDomainMask)[0]
        // generate a unique name for this file in case we've seen it before
        let UUID = NSUUID().UUIDString
        let pathComponent = "\(UUID)-\(response.suggestedFilename!)"
        return directoryURL.URLByAppendingPathComponent(pathComponent)
    }
    
    Dropbox.authorizedClient!.files.download(path: "/path/to/Dropbox/file", destination: destination)
    
        .progress { bytesRead, totalBytesRead, totalBytesExpectedToRead in
    
            print("bytesRead: \(bytesRead)")
            print("totalBytesRead: \(totalBytesRead)")
            print("totalBytesExpectedToRead: \(totalBytesExpectedToRead)")
    
        }
    
        .response { response, error in
    
            if let (metadata, url) = response {
                print("*** Download file ***")
                print("Downloaded file name: \(metadata.name)")
                print("Downloaded file url: \(url)")
            } else {
                print(error!)
            }
    
        }

You can then use that raw progress information to back the progress UI in your app.


  [1]: https://www.dropbox.com/developers/documentation/swift#tutorial
  [2]: https://github.com/dropbox/SwiftyDropbox

## Downloading a file with every error case handled using the SwiftyDropbox library
    Dropbox.authorizedClient!.files.download(path: path, destination: destination).response { response, error in
        if let (metadata, url) = response {
            print("*** Download file ***")
            print("Downloaded file name: \(metadata.name)")
            print("Downloaded file url: \(url)")
        } else if let callError = error {
            switch callError as CallError {
                case .RouteError(let boxed, let requestId):
                    print("RouteError[\(requestId)]:")
                    switch boxed.unboxed as Files.DownloadError {
                    case .Path(let fileLookupError):
                        print("PathError: ")
                        switch fileLookupError {
                        case .MalformedPath(let malformedPathError):
                            print("MalformedPath: \(malformedPathError)")
                        case .NotFile:
                            print("NotFile")
                        case .NotFolder:
                            print("NotFolder")
                        case .NotFound:
                            print("NotFound")
                        case .RestrictedContent:
                            print("RestrictedContent")
                        case .Other:
                            print("Other")
                        }
                    case .Other:
                        print("Other")
                }
                case .BadInputError(let message, let requestId):
                    print("BadInputError[\(requestId)]: \(message)")
                case .HTTPError(let code, let message, let requestId):
                    print("HTTPError[\(requestId)]: \(code): \(message)")
                case .InternalServerError(let code, let message, let requestId):
                    print("InternalServerError[\(requestId)]: \(code): \(message)")
                case .OSError(let err):
                    print("OSError: \(err)")
                case .RateLimitError:
                    print("RateLimitError")
                }
                
        }
    }


## Downloading a file via curl in C++
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
                headers = curl_slist_append(headers, "Content-Type:");
                headers = curl_slist_append(headers, "Dropbox-API-Arg: {\"path\":\"/test.txt\"}");
                curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    
                curl_easy_setopt(curl, CURLOPT_URL, "https://content.dropboxapi.com/2/files/download");
                curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "");
    
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

## Downloading a file using the Dropbox .NET library
This uses the [Dropbox .NET SDK][1] to download a file from the Dropbox API at the remote path `/Homework/math/Prime_Numbers.txt` to the local file `Prime_Numbers.txt`:

    using (var response = await client.Files.DownloadAsync("/Homework/math/Prime_Numbers.txt"))
    {
        using (var fileStream = File.Create("Prime_Numbers.txt"))
        {
            (await response.GetContentAsStreamAsync()).CopyTo(fileStream);
        }

    }


  [1]: https://github.com/dropbox/dropbox-sdk-dotnet


## Downloading a file using the Dropbox .NET library with progress tracking
This uses the [Dropbox .NET SDK][1] to download a file from the Dropbox API at the remote `path` to the local file "Test", while tracking progress:

    var response = await client.Files.DownloadAsync(path);
    ulong fileSize = response.Response.Size;
    const int bufferSize = 1024 * 1024;

    var buffer = new byte[bufferSize];

    using (var stream = await response.GetContentAsStreamAsync())
    {
        using (var file = new FileStream("Test", FileMode.OpenOrCreate))
        {
            var length = stream.Read(buffer, 0, bufferSize);

            while (length > 0)
            {
                file.Write(buffer, 0, length);
                var percentage = 100 * (ulong)file.Length / fileSize;
                // Update progress bar with the percentage.
                // progressBar.Value = (int)percentage
                Console.WriteLine(percentage);

                length = stream.Read(buffer, 0, bufferSize);
            }
        }
    }


  [1]: https://github.com/dropbox/dropbox-sdk-dotnet


## Downloading a file with metadata via Requests in PHP
    $response = Requests::post("https://content.dropboxapi.com/2/files/download", array(
        'Authorization' => "Bearer <ACCESS_TOKEN>",
        'Dropbox-Api-Arg' => json_encode(array('path' => '/test.txt')),
    ));

    $fileContent = $response->body;
    $metadata = json_decode($response->headers['Dropbox-Api-Result'], true);

    echo "File " . $metadata["name"] . " has the rev " . $metadata["rev"] . ".\n";


`<ACCESS_TOKEN>` should be replaced with the OAuth 2 access token.


## Downloading a file with metadata via curl in PHP
    <?php

    function dbx_get_file($token, $in_filepath, $out_filepath)
        {
        $out_fp = fopen($out_filepath, 'w+');
        if ($out_fp === FALSE)
            {
            echo "fopen error; can't open $out_filepath\n";
            return (NULL);
            }

        $url = 'https://content.dropboxapi.com/2/files/download';

        $header_array = array(
            'Authorization: Bearer ' . $token,
            'Content-Type:',
            'Dropbox-API-Arg: {"path":"' . $in_filepath . '"}'
        );

        $ch = curl_init();

        curl_setopt($ch, CURLOPT_URL, $url);
        curl_setopt($ch, CURLOPT_POST, TRUE);
        curl_setopt($ch, CURLOPT_HTTPHEADER, $header_array);
        curl_setopt($ch, CURLOPT_FILE, $out_fp);

        $metadata = null;
        curl_setopt($ch, CURLOPT_HEADERFUNCTION, function ($ch, $header) use (&$metadata)
            {
            $prefix = 'dropbox-api-result:';
            if (strtolower(substr($header, 0, strlen($prefix))) === $prefix)
                {
                $metadata = json_decode(substr($header, strlen($prefix)), true);
                }
            return strlen($header);
            }
        );

        $output = curl_exec($ch);

        if ($output === FALSE)
            {
            echo "curl error: " . curl_error($ch);
            }

        curl_close($ch);
        fclose($out_fp);

        return($metadata);
        } // dbx_get_file()

    $metadata = dbx_get_file("<ACCESS_TOKEN>", '/test.txt', 'test.txt');
    echo "File " . $metadata['name'] . " has the rev " . $metadata['rev'] . ".\n";

    ?>


`<ACCESS_TOKEN>` should be replaced with the OAuth 2 access token.


## Downloading a file using the Dropbox Java library
This uses the [Dropbox Java SDK][1] to download a file from the Dropbox API at the remote path `/Homework/math/Prime_Numbers.txt` to the local file `Prime_Numbers.txt`:

    String localPath = "Prime_Numbers.txt";
    OutputStream outputStream = new FileOutputStream(localPath);
    FileMetadata metadata = client.files()
            .downloadBuilder("/Homework/math/Prime_Numbers.txt")
            .download(outputStream);

  [1]: https://github.com/dropbox/dropbox-sdk-java


## Downloading a file using the Dropbox Objective-C library with progress tracking
This uses the [Dropbox Objective-C SDK][1] to download a file from Dropbox at "/test.txt".

    [[[client.filesRoutes downloadData:@"/test.txt"] response:^(DBFILESFileMetadata *metadata, DBFILESDownloadError *downloadError, DBRequestError *error, NSData *fileData) {
        if (metadata) {
            NSLog(@"The download completed successfully.");
            NSLog(@"File metadata:");
            NSLog(@"%@", metadata);
            NSLog(@"File data length:");
            NSLog(@"%lu", (unsigned long)[fileData length]);
        } else if (downloadError) {
            NSLog(@"Something went wrong with the data:");
            NSLog(@"%@", downloadError);
        } else if (error) {
            NSLog(@"Something went wrong with the API call:");
            NSLog(@"%@", error);
        }
    }] progress:^(int64_t bytesWritten, int64_t totalBytesWritten, int64_t totalBytesExpectedToWrite) {
        // Here we can monitor the progress of the transfer:
        NSLog(@"bytesWritten: %lld, totalBytesWritten: %lld, totalBytesExpectedToWrite: %lld", bytesWritten, totalBytesWritten, totalBytesExpectedToWrite);
    }];

 [1]: https://github.com/dropbox/dropbox-sdk-obj-c


