---
title: "Sharing a file"
slug: "sharing-a-file"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Inviting a member to a shared file using the Dropbox Java library
This uses the [Dropbox Java SDK][1] to share a file at "/test.txt" with a specific user:

    List<MemberSelector> newMembers = new ArrayList<MemberSelector>();
    MemberSelector newMember = MemberSelector.email("<EMAIL_ADDRESS_TO_INVITE>");
    newMembers.add(newMember);

    List<FileMemberActionResult> fileMemberActionResults = client.sharing().addFileMember("/test.txt", newMembers);
    System.out.print(fileMemberActionResults);

`<EMAIL_ADDRESS_TO_INVITE>` should be replaced with the email address of the user to invite. Also, `newMembers` is an array and can contain multiple users.


  [1]: https://github.com/dropbox/dropbox-sdk-java


