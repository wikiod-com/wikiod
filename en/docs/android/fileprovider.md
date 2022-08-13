---
title: "FileProvider"
slug: "fileprovider"
draft: false
images: []
weight: 9377
type: docs
toc: true
---

## Sharing a file
In this example you'll learn how to share a file with other apps. We'll use a pdf file in this example although the code works with every other format as well.

The roadmap:

<h1>Specify the directories in which the files you want to share are placed</h1>

To share files we'll use a FileProvider, a class allowing secure file sharing between apps. A FileProvider can only share files in predefined directories, so let's define these.

 1. Create a new XML file that will contain the paths, e.g. *res/xml/filepaths.xml*
 2. Add the paths

        <paths xmlns:android="http://schemas.android.com/apk/res/android">
            <files-path name="pdf_folder" path="documents/"/>
        </paths>

<h1> Define a FileProvider and link it with the file paths </h1>
This is done in the manifest:

    <manifest>
        ...
        <application>
            ...
            <provider
                android:name="android.support.v4.context.FileProvider"
                android:authorities="com.mydomain.fileprovider"
                android:exported="false"
                android:grantUriPermissions="true">
                <meta-data
                    android:name="android.support.FILE_PROVIDER_PATHS"
                    android:resource="@xml/filepaths" />
            </provider>
            ...
        </application>
        ...
    </manifest>
    
<h1> Generate the URI for the file </h1>
To share the file we must provide an identifier for the file. This is done by using a URI (Uniform Resource Identifier).

    // We assume the file we want to load is in the documents/ subdirectory
    // of the internal storage
    File documentsPath = new File(Context.getFilesDir(), "documents");
    File file = new File(documentsPath, "sample.pdf");
    // This can also in one line of course:
    // File file = new File(Context.getFilesDir(), "documents/sample.pdf");
    
    Uri uri = FileProvider.getUriForFile(getContext(), "com.mydomain.fileprovider", file);

As you can see in the code we first make a new File class representing the file. To get a URI we ask FileProvider to get us one. The second argument is important: it passes the authority of a FileProvider. It must be equal to the authority of the FileProvider defined in the manifest.

<h1> Share the file with other apps </h1>
We use ShareCompat to share the file with other apps:

    Intent intent = ShareCompat.IntentBuilder.from(getContext())
        .setType("application/pdf")
        .setStream(uri)
        .setChooserTitle("Choose bar")
        .createChooserIntent()
        .addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);

    Context.startActivity(intent);

A chooser is a menu from which the user can choose with which app he/she wants to share the file. The flag Intent.FLAG_GRANT_READ_URI_PERMISSION is needed to grant temporary read access permission to the URI.

