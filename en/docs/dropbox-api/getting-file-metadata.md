---
title: "Getting file metadata"
slug: "getting-file-metadata"
draft: false
images: []
weight: 9899
type: docs
toc: true
---

## Handling the error when getting metadata for a non-existing path using the Dropbox .NET library
This example uses the [Dropbox .NET library][1] to try to get the metadata for an item at a particular path, and checks for a `NotFound` error:

    try {
        var metadata = await this.client.Files.GetMetadataAsync("/non-existant path");
        Console.WriteLine(metadata.Name);
    } catch (Dropbox.Api.ApiException<Dropbox.Api.Files.GetMetadataError> e) {
    
        if (e.ErrorResponse.IsPath) {
            var pathError = e.ErrorResponse.AsPath.Value;
            if (pathError.IsNotFound) {
                Console.WriteLine ("File or folder not found.");
            } else {
                Console.WriteLine (pathError);
            }
        } else {
            Console.WriteLine (e.ErrorResponse);
        }
    }

  [1]: https://github.com/dropbox/dropbox-sdk-dotnet

## Get file metadata for a file, including media information, using the SwiftyDropbox library
    Dropbox.authorizedClient!.files.getMetadata(path: "/test.jpg", includeMediaInfo: true).response { response, error in
        if let result = response as? Files.FileMetadata {
            print(result.name)
    
            if result.mediaInfo != nil {
                switch result.mediaInfo! as Files.MediaInfo {
                case .Pending:
                    print("Media info is pending...")
                case .Metadata(let mediaMetadata):
                    print(mediaMetadata.dimensions)
                    print(mediaMetadata.location)
                    print(mediaMetadata.timeTaken)
                }
            }
        } else {
            print(error!)
        }
    }

## Get file metadata for a file, including media information, using curl
    curl -X POST https://api.dropboxapi.com/2/files/get_metadata \
        --header "Authorization: Bearer <ACCESS_TOKEN>" \
        --header "Content-Type: application/json" \
        --data "{\"path\": \"/test.jpg\",\"include_media_info\": true}"

`<ACCESS_TOKEN>` should be replaced with the OAuth 2 access token.

