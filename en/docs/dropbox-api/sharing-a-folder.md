---
title: "Sharing a folder"
slug: "sharing-a-folder"
draft: false
images: []
weight: 9765
type: docs
toc: true
---

## Inviting a member to a shared folder using curl
    curl -X POST https://api.dropboxapi.com/2/sharing/add_folder_member \
        --header "Authorization: Bearer <ACCESS_TOKEN>" \
        --header "Content-Type: application/json" \
        --data "{\"shared_folder_id\": \"<SHARED_FOLDER_ID\",\"members\": [{\"member\": {\".tag\": \"email\",\"email\": \"<EMAIL_ADDRESS_TO_INVITE>\"},\"access_level\": {\".tag\": \"editor\"}}],\"quiet\": false,\"custom_message\": \"Code examples\"}"

`<ACCESS_TOKEN>` should be replaced with the OAuth 2 access token.

`<SHARED_FOLDER_ID>` should be replaced with the shared folder ID, e.g., as returned by [`/2/sharing/share_folder`][1] or [`/2/sharing/list_folders`][2].

`<EMAIL_ADDRESS_TO_INVITE>` should be replaced with the email address of the user to invite. Also, `members` is an array and can contain multiple users.


  [1]: https://www.dropbox.com/developers/documentation/http/documentation#sharing-share_folder
  [2]: https://www.dropbox.com/developers/documentation/http/documentation#sharing-list_folders

## Sharing a folder with every error case handled using the SwiftyDropbox library
This uses the [SwiftyDropbox library][1] to share a folder, handling every error case:

    Dropbox.authorizedClient!.sharing.shareFolder(path: "/folder_path").response { response, error in
        if let result = response {
            print("response: \(result)")
        } else if let callError = error {
            switch callError as CallError {
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
            case .RouteError(let boxed, let requestId):
                print("RouteError[\(requestId)]:")
                switch boxed.unboxed as Sharing.ShareFolderError {
                case .BadPath(let sharePathError):
                    print("BadPath: \(sharePathError)")
                    switch sharePathError as Sharing.SharePathError {
                    case .AlreadyShared:
                        print("AlreadyShared")
                    case .ContainsSharedFolder:
                        print("ContainsSharedFolder")
                    case .InsideAppFolder:
                        print("InsideAppFolder")
                    case .InsideSharedFolder:
                        print("InsideSharedFolder")
                    case .InvalidPath:
                        print("InvalidPath")
                    case .IsAppFolder:
                        print("IsAppFolder")
                    case .IsFile:
                        print("IsFile")
                    case .Other:
                        print("Other")
                    }
                case .EmailUnverified:
                    print("EmailUnverified")
                case .TeamPolicyDisallowsMemberPolicy:
                    print("TeamPolicyDisallowsMemberPolicy")
                case .Other:
                    print("Other")
                }
            }
        }
    }


  [1]: https://github.com/dropbox/SwiftyDropbox/

## Sharing a folder using curl
        curl -X POST https://api.dropboxapi.com/2/sharing/share_folder \
            --header "Authorization: Bearer <ACCESS_TOKEN>" \
            --header "Content-Type: application/json" \
            --data "{\"path\": \"/folder_path\",\"member_policy\": \"team\",\"acl_update_policy\": \"editors\",\"shared_link_policy\": \"members\",\"force_async\": false}"
    
`<ACCESS_TOKEN>` should be replaced with the OAuth 2 access token.
    

## Inviting a member to a shared folder with every error case handled using the SwiftyDropbox library
    let toInvite = [Sharing.AddMember(member: Sharing.MemberSelector.Email("<EMAIL_ADDRESS_TO_INVITE>"))]

    Dropbox.authorizedClient!.sharing.addFolderMember(sharedFolderId: "<SHARED_FOLDER_ID>", members: toInvite).response { response, error in

        if (response != nil) {
            print("Invited member.")
        } else if let callError = error {
            switch callError as CallError {
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
            case .RouteError(let boxed, let requestId):
                print("RouteError[\(requestId)]:")
                switch boxed.unboxed as Sharing.AddFolderMemberError {
                case .AccessError(let sharedFolderAccessError):
                        print("AccessError")
                        switch sharedFolderAccessError {
                        case .EmailUnverified:
                            print("EmailUnverified")
                        case .InvalidId:
                            print("InvalidId")
                        case .NoPermission:
                            print("NoPermission")
                        case .NotAMember:
                            print("NotAMember")
                        case .TeamFolder:
                            print("TeamFolder")
                        case .Unmounted:
                            print("Unmounted")
                        case .Other:
                            print("Other")
                    }
                case .BadMember(let addMemberSelectorError):
                    switch addMemberSelectorError {
                    case .GroupDeleted:
                        print("GroupDeleted")
                    case .GroupNotOnTeam:
                        print("GroupNotOnTeam")
                    case .InvalidDropboxId(let invalidDropboxId):
                        print("InvalidDropboxId: \(invalidDropboxId)")
                    case .InvalidEmail(let invalidEmail):
                        print("InvalidEmail: \(invalidEmail)")
                    case .UnverifiedDropboxId(let unverifiedDropboxId):
                        print("UnverifiedDropboxId: \(unverifiedDropboxId)")
                    case .Other:
                        print("Other")
                    }
                case .CantShareOutsideTeam:
                    print("CantShareOutsideTeam")
                case .EmailUnverified:
                    print("EmailUnverified")
                case .InsufficientPlan:
                    print("InsufficientPlan")
                case .NoPermission:
                    print("NoPermission")
                case .RateLimit:
                    print("RateLimit")
                case .TooManyMembers(let limit):
                    print("TooManyMembers: \(limit)")
                case .TooManyPendingInvites(let limit):
                    print("TooManyPendingInvites: \(limit)")
                case .Other:
                    print("Other")
                }
            }
        }

    }

`<SHARED_FOLDER_ID>` should be replaced with the shared folder ID, e.g., as returned by [`sharing.shareFolder`][1] or [`sharing.listFolders`][2].

`<EMAIL_ADDRESS_TO_INVITE>` should be replaced with the email address of the user to invite. Also, members is an array and can contain multiple users.


  [1]: https://dropbox.github.io/SwiftyDropbox/api-docs/latest/Classes/SharingRoutes.html#/s:FC13SwiftyDropbox13SharingRoutes11shareFolderFS0_FT4pathSS12memberPolicyOCS_7Sharing12MemberPolicy15aclUpdatePolicyOS1_15AclUpdatePolicy16sharedLinkPolicyOS1_16SharedLinkPolicy10forceAsyncSb_GCS_15BabelRpcRequestCS1_27ShareFolderLaunchSerializerCS1_26ShareFolderErrorSerializer_
  [2]: https://dropbox.github.io/SwiftyDropbox/api-docs/latest/Classes/SharingRoutes.html#/s:FC13SwiftyDropbox13SharingRoutes11listFoldersFS0_FT_GCS_15BabelRpcRequestCCS_7Sharing27ListFoldersResultSerializerCS_14VoidSerializer_

## Sharing a folder via HttpWebRequest in PowerShell
    $url = "https://api.dropboxapi.com/2/sharing/share_folder"
    
    $req = [System.Net.HttpWebRequest]::Create($url)
    $req.headers["Authorization"] = "Bearer <ACCESS_TOKEN>"
    $req.Method = "POST"
    $req.ContentType = "application/json"
    $enc = [system.Text.Encoding]::UTF8
    $params = @{path="/new shared folder path"} | ConvertTo-Json -compress
    $params = $enc.GetBytes($params)
    $req.GetRequestStream().Write($params, 0, $params.Length)
    
    $res = $req.GetResponse()
    Write-Host "Response Status Code: "$res.StatusCode
    Write-Host "Response Status Description: "$res.StatusDescription
    $readStream = new-object System.IO.StreamReader $res.GetResponseStream()
    $result = $readStream.ReadToEnd() | ConvertFrom-Json
    Write-Host $result
    $readStream.Close()
    $res.Close()

`<ACCESS_TOKEN>` should be replaced with your access token.

## Inviting a member to a shared folder via jQuery in JavaScript
    $.ajax({
        url: 'https://api.dropboxapi.com/2/sharing/add_folder_member',
        type: 'POST',
        processData: false,
        data: JSON.stringify({"shared_folder_id": "84528192421","members": [{"member": {".tag": "email","email": "justin@example.com"},"access_level": {".tag": "editor"}},{"member": {".tag": "dropbox_id","dropbox_id": "dbid:AAEufNrMPSPe0dMQijRP0N_aZtBJRm26W4Q"},"access_level": {".tag": "viewer"}}],"quiet": false,"custom_message": "Documentation for launch day"}),
        contentType: 'application/json',
        headers: {
            "Authorization": "Bearer <ACCESS_TOKEN>"
        },
        success: function(data) {
            console.log(data);
        },
        error: function(data) {
            console.error(data);
        }
    })

`<ACCESS_TOKEN>` should be replaced with the OAuth 2 access token.

