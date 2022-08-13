---
title: "Getting a shared link for a file or folder"
slug: "getting-a-shared-link-for-a-file-or-folder"
draft: false
images: []
weight: 9865
type: docs
toc: true
---

## Creating a shared link for a file using curl in PHP
    <?php

    $parameters = array('path' => '/test.txt');

    $headers = array('Authorization: Bearer <ACCESS_TOKEN>',
                     'Content-Type: application/json');

    $curlOptions = array(
            CURLOPT_HTTPHEADER => $headers,
            CURLOPT_POST => true,
            CURLOPT_POSTFIELDS => json_encode($parameters),
            CURLOPT_RETURNTRANSFER => true,
            CURLOPT_VERBOSE => true
        );

    $ch = curl_init('https://api.dropboxapi.com/2/sharing/create_shared_link_with_settings');
    curl_setopt_array($ch, $curlOptions);

    $response = curl_exec($ch);
    echo $response;

    curl_close($ch);

    ?>


`<ACCESS_TOKEN>` should be replaced with the OAuth 2 access token.


## Creating a shared link for a folder using the Dropbox Python library
This uses the [Dropbox Python SDK][1] to create a shared link for a folder:
 

    import dropbox
    dbx = dropbox.Dropbox("<ACCESS_TOKEN>")
    shared_link_metadata = dbx.sharing_create_shared_link_with_settings("/Testing")
    print shared_link_metadata.url

`<ACCESS_TOKEN>` should be replaced with the access token.
 
 
 [1]: https://github.com/dropbox/dropbox-sdk-python


## Retrieving an existing shared link for a specific file using curl
    curl -X POST https://api.dropboxapi.com/2/sharing/list_shared_links \
        --header "Authorization: Bearer <ACCESS_TOKEN>" \
        --header "Content-Type: application/json" \
        --data "{\"path\": \"/test.txt\", \"direct_only\": true}"

`<ACCESS_TOKEN>` should be replaced with the OAuth 2 access token.

## Creating a shared link for a file using the SwiftyDropbox library
    Dropbox.authorizedClient!.sharing.createSharedLink(path: "/test.txt").response({ response, error in
        if let link = response {
            print(link.url)
        } else {
            print(error!)
        }
    })

## Creating a shared link for a file using curl
    curl -X POST https://api.dropboxapi.com/2/sharing/create_shared_link_with_settings \
        --header "Authorization: Bearer <ACCESS_TOKEN>" \
        --header "Content-Type: application/json" \
        --data "{\"path\": \"/Prime_Numbers.txt\",\"settings\": {\"requested_visibility\": \"public\"}}"

`<ACCESS_TOKEN>` should be replaced with the access token.

## Creating a shared link for a file with expiration and visibility settings using the Dropbox Python library
This uses the [Dropbox Python SDK][1] to [create a shared link][2] for a file and also supplies a [requested visibility][3] and expiration in the [settings][4]:


    import datetime

    import dropbox

    dbx = dropbox.Dropbox("<ACCESS_TOKEN>")

    expires = datetime.datetime.now() + datetime.timedelta(days=30)
    requested_visibility = dropbox.sharing.RequestedVisibility.team_only
    desired_shared_link_settings = dropbox.sharing.SharedLinkSettings(requested_visibility=requested_visibility, expires=expires)

    shared_link_metadata = dbx.sharing_create_shared_link_with_settings("/test.txt", settings=desired_shared_link_settings)

    print(shared_link_metadata)

`<ACCESS_TOKEN>` should be replaced with the access token.


 [1]: https://github.com/dropbox/dropbox-sdk-python
 [2]: https://dropbox-sdk-python.readthedocs.io/en/master/moduledoc.html#dropbox.dropbox.Dropbox.sharing_create_shared_link_with_settings
 [3]: https://dropbox-sdk-python.readthedocs.io/en/master/moduledoc.html#dropbox.sharing.RequestedVisibility
 [4]: https://dropbox-sdk-python.readthedocs.io/en/master/moduledoc.html#dropbox.sharing.SharedLinkSettings


## Creating a shared link for a file using the Dropbox Java library
This uses the [Dropbox Java SDK][1] to create a shared link for a file at the Dropbox path /test.txt:

    try {
        SharedLinkMetadata sharedLinkMetadata = client.sharing().createSharedLinkWithSettings("/test.txt");
        System.out.println(sharedLinkMetadata.getUrl());
    } catch (CreateSharedLinkWithSettingsErrorException ex) {
        System.out.println(ex);
    } catch (DbxException ex) {
        System.out.println(ex);
    }

This assumes `client` is a pre-existing and authorized `DbxClientV2` object, and does some basic exception handling to print the output.

 [1]: https://github.com/dropbox/dropbox-sdk-java


## Getting a shared link for a file using the Dropbox .NET library
This example uses the [Dropbox .NET library][1] to get a shared link for a file, either by creating a new one, or retrieving an existing one:

    SharedLinkMetadata sharedLinkMetadata;
    try {
        sharedLinkMetadata = await this.client.Sharing.CreateSharedLinkWithSettingsAsync (path);
    } catch (ApiException<CreateSharedLinkWithSettingsError> err) {
        if (err.ErrorResponse.IsSharedLinkAlreadyExists) {
            var sharedLinksMetadata = await this.client.Sharing.ListSharedLinksAsync (path, null, true);
            sharedLinkMetadata = sharedLinksMetadata.Links.First();
        } else {
            throw err;
        }
    }
    Console.WriteLine (sharedLinkMetadata.Url);


  [1]: https://github.com/dropbox/dropbox-sdk-dotnet


## Retrieving an existing shared link for a specific file using the Dropbox Java library
This uses the [Dropbox Java SDK][1] to retrieve an existing shared link for /Testing/test.txt specifically:

    ListSharedLinksResult listSharedLinksResult = client.sharing()
            .listSharedLinksBuilder()
            .withPath("/Testing/test.txt").withDirectOnly(true)
            .start();
    System.out.println(listSharedLinksResult.getLinks());

  [1]: https://github.com/dropbox/dropbox-sdk-java


