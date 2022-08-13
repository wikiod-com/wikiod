---
title: "Uploading a file"
slug: "uploading-a-file"
draft: false
images: []
weight: 9770
type: docs
toc: true
---

## Uploading a file using the Dropbox .NET library
This example uses the [Dropbox .NET library][1] to upload a file to a Dropbox account, using upload sessions for larger files:

    private async Task Upload(string localPath, string remotePath)
    {
        const int ChunkSize = 4096 * 1024;
        using (var fileStream = File.Open(localPath, FileMode.Open))
        {
            if (fileStream.Length <= ChunkSize)
            {
                await this.client.Files.UploadAsync(remotePath, body: fileStream);
            }
            else
            {
                await this.ChunkUpload(remotePath, fileStream, (int)ChunkSize);
            }
        }
    }

    private async Task ChunkUpload(String path, FileStream stream, int chunkSize)
    {
        ulong numChunks = (ulong)Math.Ceiling((double)stream.Length / chunkSize);
        byte[] buffer = new byte[chunkSize];
        string sessionId = null;
        for (ulong idx = 0; idx < numChunks; idx++)
        {
            var byteRead = stream.Read(buffer, 0, chunkSize);

            using (var memStream = new MemoryStream(buffer, 0, byteRead))
            {
                if (idx == 0)
                {
                    var result = await this.client.Files.UploadSessionStartAsync(false, memStream);
                    sessionId = result.SessionId;
                }
                else
                {
                    var cursor = new UploadSessionCursor(sessionId, (ulong)chunkSize * idx);

                    if (idx == numChunks - 1)
                    {
                        FileMetadata fileMetadata = await this.client.Files.UploadSessionFinishAsync(cursor, new CommitInfo(path), memStream);
                        Console.WriteLine (fileMetadata.PathDisplay);
                    }
                    else
                    {
                        await this.client.Files.UploadSessionAppendV2Async(cursor, false, memStream);
                    }
                }
            }
        }
    }
    

  [1]: https://github.com/dropbox/dropbox-sdk-dotnet


## Uploading a file via curl
This uploads a file from the local path `matrices.txt` in the current folder to `/Homework/math/Matrices.txt` in the Dropbox account, and returns the metadata for the uploaded file:

    echo "some content here" > matrices.txt
    
    curl -X POST https://content.dropboxapi.com/2/files/upload \
        --header "Authorization: Bearer <ACCESS_TOKEN>" \
        --header "Dropbox-API-Arg: {\"path\": \"/Homework/math/Matrices.txt\"}" \
        --header "Content-Type: application/octet-stream" \
        --data-binary @matrices.txt

`<ACCESS_TOKEN>` should be replaced with the OAuth 2 access token.

## Uploading a file via curl in PHP
    <?php
    
    $path = 'test_php_upload.txt';
    $fp = fopen($path, 'rb');
    $size = filesize($path);
    
    $cheaders = array('Authorization: Bearer <ACCESS_TOKEN>',
                      'Content-Type: application/octet-stream',
                      'Dropbox-API-Arg: {"path":"/test/'.$path.'", "mode":"add"}');
    
    $ch = curl_init('https://content.dropboxapi.com/2/files/upload');
    curl_setopt($ch, CURLOPT_HTTPHEADER, $cheaders);
    curl_setopt($ch, CURLOPT_PUT, true);
    curl_setopt($ch, CURLOPT_CUSTOMREQUEST, 'POST');
    curl_setopt($ch, CURLOPT_INFILE, $fp);
    curl_setopt($ch, CURLOPT_INFILESIZE, $size);
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
    $response = curl_exec($ch);
    
    echo $response;
    curl_close($ch);
    fclose($fp);
    
    ?>

`<ACCESS_TOKEN>` should be replaced with the OAuth 2 access token.

## Uploading a file using the Dropbox Python SDK
This uses the [Dropbox Python SDK][1] to upload a file to the Dropbox API from the local file as specified by `file_path` to the remote path as specified by `dest_path`. It also chooses whether or not to use an upload session based on the size of the file:

    f = open(file_path)
    file_size = os.path.getsize(file_path)

    CHUNK_SIZE = 4 * 1024 * 1024

    if file_size <= CHUNK_SIZE:

        print dbx.files_upload(f.read(), dest_path)

    else:

        upload_session_start_result = dbx.files_upload_session_start(f.read(CHUNK_SIZE))
        cursor = dropbox.files.UploadSessionCursor(session_id=upload_session_start_result.session_id,
                                                   offset=f.tell())
        commit = dropbox.files.CommitInfo(path=dest_path)

        while f.tell() < file_size:
            if ((file_size - f.tell()) <= CHUNK_SIZE):
                print dbx.files_upload_session_finish(f.read(CHUNK_SIZE),
                                                cursor,
                                                commit)
            else:
                dbx.files_upload_session_append(f.read(CHUNK_SIZE),
                                                cursor.session_id,
                                                cursor.offset)
                cursor.offset = f.tell()

    f.close()

  [1]: https://github.com/dropbox/dropbox-sdk-python

## Uploading a file via curl in C++
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
                headers = curl_slist_append(headers, "Content-Type: application/octet-stream");
                headers = curl_slist_append(headers, "Dropbox-API-Arg: {\"path\":\"/test_c++_upload_test.txt\"}");
                curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    
                curl_easy_setopt(curl, CURLOPT_URL, "https://content.dropboxapi.com/2/files/upload");
                curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "test data for upload");
    
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

`<ACCESS_TOKEN>` should be replaced with the OAuth 2 access token.

## Uploading a file from NSData with every error case handled using the SwiftyDropbox library
This uses the [SwiftyDropbox library][1] to upload a file from a `NSData` to the Dropbox account, using upload sessions for larger files, handling every error case:

    import UIKit
    import SwiftyDropbox

    class ViewController: UIViewController {

        // replace this made up data with the real data
        let data = String(count: 20 * 1024 * 1024, repeatedValue: Character("A")).dataUsingEncoding(NSUTF8StringEncoding)!


        let chunkSize = 5 * 1024 * 1024  // 5 MB
        var offset = 0
        var sessionId = ""

        // replace this with your desired destination path:
        let destPath = "/SwiftyDropbox_upload.txt"

        override func viewDidLoad() {
            super.viewDidLoad()

            Dropbox.authorizedClient = DropboxClient(...)

            doUpload()
        }

        override func didReceiveMemoryWarning() {
            super.didReceiveMemoryWarning()
        }

        func doUpload() {

            let fileSize = data.length

            print("Have \(fileSize) bytes to upload.")

            if (fileSize < chunkSize) {

                print("Using non-chunked uploading...")

                Dropbox.authorizedClient!.files.upload(path:destPath, input:data).response { response, error in
                    if let metadata = response {
                        print(metadata)
                    } else if let callError = error {
                        print("upload failed")
                        switch callError as CallError {
                        case .RouteError(let boxed, let requestId):
                            print("RouteError[\(requestId)]:")
                            switch boxed.unboxed as Files.UploadError {
                            case .Path(let uploadError):
                                print("Path:")
                                switch uploadError.reason as Files.WriteError {
                                case .MalformedPath(let malformedPathError):
                                    print("MalformedPath: \(malformedPathError)")
                                case .Conflict(let writeConflictError):
                                    print("Conflict:")
                                    switch writeConflictError {
                                    case .File:
                                        print("File")
                                    case .FileAncestor:
                                        print("FileAncestor")
                                    case .Folder:
                                        print("Folder")
                                    case .Other:
                                        print("Other")
                                    }
                                case .DisallowedName:
                                    print("DisallowedName")
                                case .InsufficientSpace:
                                    print("InsufficientSpace")
                                case .NoWritePermission:
                                    print("NoWritePermission")
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

            } else {

                print("Using chunked uploading...")

                uploadFirstChunk()

            }
        }

        func uploadFirstChunk() {
            let size = min(chunkSize, data.length)
            Dropbox.authorizedClient!.files.uploadSessionStart(input:
                data.subdataWithRange(NSMakeRange(0, size)))
                .response { response, error in
                    if let result = response {
                        self.sessionId = result.sessionId
                        self.offset += size
                        print("So far \(self.offset) bytes have been uploaded.")
                        self.uploadNextChunk()
                    } else if let callError = error {
                        print("uploadSessionStart failed")
                        switch callError as CallError {
                        case .RouteError(let error, let requestId):
                            print("RouteError[\(requestId)]: \(error)")
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
        }

        func uploadNextChunk() {
            if data.length - offset <= chunkSize {
                let size = data.length - offset
                Dropbox.authorizedClient!.files.uploadSessionFinish(
                    cursor: Files.UploadSessionCursor(
                        sessionId: self.sessionId, offset: UInt64(offset)),
                    commit: Files.CommitInfo(path:destPath),
                    input: data.subdataWithRange(NSMakeRange(offset, size)))
                    .response { response, error in
                        if let callError = error {
                            print("uploadSessionFinish failed")
                            switch callError as CallError {
                            case .RouteError(let boxed, let requestId):
                                print("RouteError[\(requestId)]:")
                                switch boxed.unboxed as Files.UploadSessionFinishError {
                                case .Path(let writeError):
                                    print("Path: ")
                                    switch writeError {
                                    case .MalformedPath(let malformedPathError):
                                        print("MalformedPath: \(malformedPathError)")
                                    case .Conflict(let writeConflictError):
                                        print("Conflict:")
                                        switch writeConflictError {
                                        case .File:
                                            print("File")
                                        case .FileAncestor:
                                            print("FileAncestor")
                                        case .Folder:
                                            print("Folder")
                                        case .Other:
                                            print("Other")
                                        }
                                    case .DisallowedName:
                                        print("DisallowedName")
                                    case .InsufficientSpace:
                                        print("InsufficientSpace")
                                    case .NoWritePermission:
                                        print("NoWritePermission")
                                    case .Other:
                                        print("Other")
                                    }
                                case .LookupFailed(let uploadSessionLookupError):
                                    print("LookupFailed:")
                                    switch uploadSessionLookupError {
                                    case .Closed:
                                        print("Closed")
                                    case .IncorrectOffset(let uploadSessionOffsetError):
                                        print("IncorrectOffset: \(uploadSessionOffsetError)")
                                    case .NotFound:
                                        print("NotFound")
                                    case .NotClosed:
                                        print("NotFound")
                                    case .Other:
                                        print("Other")
                                    }
                                case .TooManySharedFolderTargets:
                                    print("TooManySharedFolderTargets")
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
                        } else if let result = response {
                            print("Done!")
                            print(result)
                        }
                }
            } else {
                Dropbox.authorizedClient!.files.uploadSessionAppendV2(
                    cursor: Files.UploadSessionCursor(sessionId: self.sessionId, offset: UInt64(offset)),
                    input: data.subdataWithRange(NSMakeRange(offset, chunkSize)))
                    .response { response, error in
                        if error == nil {
                            self.offset += self.chunkSize
                            print("So far \(self.offset) bytes have been uploaded.")
                            self.uploadNextChunk()
                        } else if let callError = error {
                            print("uploadSessionAppend failed")
                            switch callError as CallError {
                            case .RouteError(let boxed, let requestId):
                                print("RouteError[\(requestId)]:")
                                switch boxed.unboxed as Files.UploadSessionLookupError {
                                case .Closed:
                                    print("Closed")
                                case .IncorrectOffset(let uploadSessionOffsetError):
                                    print("IncorrectOffset: \(uploadSessionOffsetError)")
                                case .NotFound:
                                    print("NotFound")
                                case .NotClosed:
                                    print("NotClosed")
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
            }
        }

    }


  [1]: https://github.com/dropbox/SwiftyDropbox/


## Uploading a file via jQuery in JavaScript
    // ... file selected from a file <input>
    file = event.target.files[0];
    $.ajax({
        url: 'https://content.dropboxapi.com/2/files/upload',
        type: 'post',
        data: file,
        processData: false,
        contentType: 'application/octet-stream',
        headers: {
            "Authorization": "Bearer <ACCESS_TOKEN>",
            "Dropbox-API-Arg": '{"path": "/test_upload.txt","mode": "add","autorename": true,"mute": false}'
        },
        success: function (data) {
            console.log(data);
        },
        error: function (data) {
            console.error(data);
        }
    })

`<ACCESS_TOKEN>` should be replaced with the OAuth 2 access token.


## Uploading a file from text via jQuery in JavaScript
    var data = new TextEncoder("utf-8").encode("Test");
    $.ajax({
        url: 'https://content.dropboxapi.com/2/files/upload',
        type: 'post',
        data: data,
        processData: false,
        contentType: 'application/octet-stream',
        headers: {
            "Authorization": "Bearer <ACCESS_TOKEN>",
            "Dropbox-API-Arg": '{"path": "/test_upload.txt","mode": "add","autorename": true,"mute": false}'
        },
        success: function (data) {
            console.log(data);
        },
        error: function (data) {
            console.error(data);
        }
    })

`<ACCESS_TOKEN>` should be replaced with the OAuth 2 access token.


## Uploading a file from text via XMLHttpRequest in JavaScript
    var path = "/test_javascript_upload.txt";
    var content = "data to upload";
    var accessToken = "<ACCESS_TOKEN>";
    var uploadUrl = "https://content.dropboxapi.com/2/files/upload"
    var result;

    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function() {
        if (xhr.readyState === 4) {
            result = xhr.responseText;
            console.log(result);
        }
    };
    xhr.open("POST", uploadUrl, true);
    xhr.setRequestHeader("Authorization", "Bearer " + accessToken);
    xhr.setRequestHeader("Content-type", "application/octet-stream");
    xhr.setRequestHeader("Dropbox-API-Arg", '{"path": "' + path + '"}');
    xhr.send(content);

`<ACCESS_TOKEN>` should be replaced with the OAuth 2 access token.


## Uploading a file from NSFileHandle using upload sessions with every error case handled using the SwiftyDropbox library
This uses the [SwiftyDropbox library][1] to upload a file from a `NSFileHandle` to the Dropbox account using upload sessions, handling every error case:

    import UIKit
    import SwiftyDropbox

    class ViewController: UIViewController {

        // filled in later in doUpload:
        var fileHandle : NSFileHandle? = nil
        var data : NSData? = nil

        let chunkSize = 5 * 1024 * 1024  // 5 MB
        var offset = 0
        var sessionId = ""

        // replace this with your desired destination path:
        let destPath = "/SwiftyDropbox_upload.txt"

        override func viewDidLoad() {
            super.viewDidLoad()

            Dropbox.authorizedClient = DropboxClient(...)

            doUpload()
        }

        override func didReceiveMemoryWarning() {
            super.didReceiveMemoryWarning()
        }

        func doUpload() {

            // replace this with the path to the file you want to upload
            let filePath = "/path/to/file"
            print("Getting file at \(filePath) for uploading...")
            fileHandle = NSFileHandle.init(forReadingAtPath: filePath)!

            print("Using chunked uploading with chunk size \(chunkSize)...")
            uploadFirstChunk()

        }

        func uploadFirstChunk() {
            data = fileHandle!.readDataOfLength(chunkSize)
            let size = data!.length
            print("Have \(size) bytes to upload.")
            Dropbox.authorizedClient!.files.uploadSessionStart(input:data!)
                .response { response, error in
                    if let result = response {
                        self.sessionId = result.sessionId
                        self.offset += size
                        print("So far \(self.offset) bytes have been uploaded.")
                        self.uploadNextChunk()
                    } else if let callError = error {
                        print("uploadSessionStart failed")
                        switch callError as CallError {
                        case .RouteError(let error, let requestId):
                            print("RouteError[\(requestId)]: \(error)")
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
        }

        func uploadNextChunk() {
            data = fileHandle!.readDataOfLength(chunkSize)
            let size = data!.length
            print("Have \(size) bytes to upload.")
            if size < chunkSize {
                print("Last chunk!")
                Dropbox.authorizedClient!.files.uploadSessionFinish(
                    cursor: Files.UploadSessionCursor(sessionId: self.sessionId, offset: UInt64(offset)),
                    commit: Files.CommitInfo(path:destPath),
                    input: data!)
                    .response { response, error in
                        if let callError = error {
                            print("uploadSessionFinish failed")
                            switch callError as CallError {
                            case .RouteError(let boxed, let requestId):
                                print("RouteError[\(requestId)]:")
                                switch boxed.unboxed as Files.UploadSessionFinishError {
                                case .Path(let writeError):
                                    print("Path: ")
                                    switch writeError {
                                    case .MalformedPath(let malformedPathError):
                                        print("MalformedPath: \(malformedPathError)")
                                    case .Conflict(let writeConflictError):
                                        print("Conflict:")
                                        switch writeConflictError {
                                        case .File:
                                            print("File")
                                        case .FileAncestor:
                                            print("FileAncestor")
                                        case .Folder:
                                            print("Folder")
                                        case .Other:
                                            print("Other")
                                        }
                                    case .DisallowedName:
                                        print("DisallowedName")
                                    case .InsufficientSpace:
                                        print("InsufficientSpace")
                                    case .NoWritePermission:
                                        print("NoWritePermission")
                                    case .Other:
                                        print("Other")
                                    }
                                case .LookupFailed(let uploadSessionLookupError):
                                    print("LookupFailed:")
                                    switch uploadSessionLookupError {
                                    case .Closed:
                                        print("Closed")
                                    case .IncorrectOffset(let uploadSessionOffsetError):
                                        print("IncorrectOffset: \(uploadSessionOffsetError)")
                                    case .NotFound:
                                        print("NotFound")
                                    case .NotClosed:
                                        print("NotFound")
                                    case .Other:
                                        print("Other")
                                    }
                                case .TooManySharedFolderTargets:
                                    print("TooManySharedFolderTargets")
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
                        } else if let result = response {
                            print("Done!")
                            print(result)
                        }
                }
            } else {
                Dropbox.authorizedClient!.files.uploadSessionAppendV2(
                    cursor: Files.UploadSessionCursor(sessionId: self.sessionId, offset: UInt64(offset)),
                    input: data!)
                    .response { response, error in
                        if error == nil {
                            self.offset += self.chunkSize
                            print("So far \(self.offset) bytes have been uploaded.")
                            self.uploadNextChunk()
                        } else if let callError = error {
                            print("uploadSessionAppend failed")
                            switch callError as CallError {
                            case .RouteError(let boxed, let requestId):
                                print("RouteError[\(requestId)]:")
                                switch boxed.unboxed as Files.UploadSessionLookupError {
                                case .Closed:
                                    print("Closed")
                                case .IncorrectOffset(let uploadSessionOffsetError):
                                    print("IncorrectOffset: \(uploadSessionOffsetError)")
                                case .NotFound:
                                    print("NotFound")
                                case .NotClosed:
                                    print("NotClosed")
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
            }
        }

    }


  [1]: https://github.com/dropbox/SwiftyDropbox/


## Uploading a file using the Dropbox Objective-C SDK
This uses the [Dropbox Objective-C SDK][1] to upload a local file to Dropbox as "/test.txt".

    [[client.filesRoutes uploadUrl:@"/test.txt" inputUrl:[NSURL fileURLWithPath:@"/local/path/to/test.txt"]] response:^(DBFILESFileMetadata *metadata, DBFILESUploadError *uploadError, DBRequestError *error) {
        if (metadata) {
            NSLog(@"The upload completed successfully.");
            NSLog(@"File metadata:");
            NSLog(@"%@", metadata);
        } else if (uploadError) {
            NSLog(@"Something went wrong with the upload:");
            NSLog(@"%@", uploadError);
        } else if (error) {
            NSLog(@"Something went wrong with the API call:");
            NSLog(@"%@", error);
        }
    }];

  [1]: https://github.com/dropbox/dropbox-sdk-obj-c


