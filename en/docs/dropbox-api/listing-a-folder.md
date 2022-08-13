---
title: "Listing a folder"
slug: "listing-a-folder"
draft: false
images: []
weight: 9787
type: docs
toc: true
---

## Listing the root folder via curl
This lists the root folder, which is identified by the empty string `""` for Dropbox API v2, using curl, using [/files/list_folder][1]:

    curl -X POST https://api.dropboxapi.com/2/files/list_folder \
        --header "Authorization: Bearer <ACCESS_TOKEN>" \
        --header "Content-Type: application/json" \
        --data "{\"path\": \"\"}"

`<ACCESS_TOKEN>` should be replaced with the OAuth 2 access token.

Note that the response may contain `has_more=true`, in which case your app should call back to [/files/list_folder/continue][2] to continue getting more entries.


  [1]: https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder
  [2]: https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder-continue

## Listing the root folder via curl in PHP and the cURL extension
    <?php

    $parameters = array('path' => '','include_deleted' => true,'recursive' => true);

    $headers = array('Authorization: Bearer <ACCESS_TOKEN>',
                     'Content-Type: application/json');

    $curlOptions = array(
            CURLOPT_HTTPHEADER => $headers,
            CURLOPT_POST => true,
            CURLOPT_POSTFIELDS => json_encode($parameters),
            CURLOPT_RETURNTRANSFER => true,
            CURLOPT_VERBOSE => true
        );

    $ch = curl_init('https://api.dropboxapi.com/2/files/list_folder');
    curl_setopt_array($ch, $curlOptions);

    $response = curl_exec($ch);
    echo $response;

    curl_close($ch);

    ?>

Note that the response may contain `has_more=true`, in which case your app should call back to [/files/list_folder/continue][1] to continue getting more entries.


`<ACCESS_TOKEN>` should be replaced with the OAuth 2 access token.

  [1]: https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder-continue

## Listing the root folder using the SwiftyDropbox library, distinguishing files and folders in the response
    Dropbox.authorizedClient!.files.listFolder(path: "").response { response, error in
        print("*** List folder ***")
        if let result = response {
            print("Folder contents:")
            for entry in result.entries {
                print(entry.name)
                if let file = entry as? Files.FileMetadata {
                    print("\tThis is a file with path: \(file.pathLower) and size: \(file.size)")
                } else if let folder = entry as? Files.FolderMetadata {
                    print("\tThis is a folder with path: \(folder.pathLower)")
                }
            }
        } else if let callError = error {
            switch callError {
            case .RouteError(let boxed, _):
                switch boxed.unboxed {
                case .Path(let lookupError):
                    print("lookupError:")
                    print(lookupError)
                case .Other:
                    print("Other")
                }
            default:
                print("default")
            }
        }
    }

Note that the response may contain [`ListFolderResult.hasMore=true`][1], in which case your app should call back using [`listFolderContinue`][2] to continue getting more entries.


  [1]: https://dropbox.github.io/SwiftyDropbox/api-docs/latest/Classes/Files/ListFolderResult.html#/s:vCC13SwiftyDropbox5Files16ListFolderResult7hasMoreSb
  [2]: https://dropbox.github.io/SwiftyDropbox/api-docs/latest/Classes/FilesRoutes.html#/s:FC13SwiftyDropbox11FilesRoutes18listFolderContinueFS0_FT6cursorSS_GCS_15BabelRpcRequestCCS_5Files26ListFolderResultSerializerCS2_33ListFolderContinueErrorSerializer_

## Attempting to list a non-existant folder using the SwiftyDropbox library, as an example of error handling
    // List folder
    Dropbox.authorizedClient!.files.listFolder(path: "/nonexistantpath").response { response, error in
        print("*** List folder ***")
        if let result = response {
            print("Folder contents:")
            for entry in result.entries {
                print(entry.name)
            }
        } else if let callError = error {
            switch callError {
            case .RouteError(let boxed, _):
                switch boxed.unboxed {
                case .Path(let lookupError):
                    print("lookupError:")
                    print(lookupError)
                case .Other:
                    print("Other")
                }
            default:
                print("default")
            }
        }
    }

## File Listing using PHP and cURL extension
    <?php
    
    $ch = curl_init();
    
    curl_setopt($ch, CURLOPT_URL, "https://api.dropboxapi.com/2/files/list_folder");
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
    curl_setopt($ch, CURLOPT_CAINFO, "cacert.pem");
    curl_setopt($ch, CURLOPT_SSL_VERIFYHOST, 0);
    curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, 0);
    curl_setopt($ch, CURLOPT_POSTFIELDS, "{\"path\":\"/<FOLDER PATH>\"}");
    curl_setopt($ch, CURLOPT_POST, 1);
    
    $headers = array();
    $headers[] = "Authorization: Bearer <ACCESS_TOKEN>";
    $headers[] = "Content-Type: application/json";
    curl_setopt($ch, CURLOPT_HTTPHEADER, $headers);
    
    $result = curl_exec($ch);
    if (curl_errno($ch)) {
        echo 'Error:' . curl_error($ch);
    }
    curl_close ($ch);
    
    
    $json = json_decode($result, true);
    foreach ($json['entries'] as $data) {
        echo 'File Name: ' . $data['name'];
    }
    
    ?>

Note that the response may contain `has_more=true`, in which case your app should call back to /files/list_folder/continue to continue getting more entries.

`<ACCESS_TOKEN>` **should be replaced** with the OAuth 2 access token.

