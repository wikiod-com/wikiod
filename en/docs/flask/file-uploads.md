---
title: "File Uploads"
slug: "file-uploads"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## Syntax
- request.files['name']  # single required file
- request.files.get('name')  # None if not posted
- request.files.getlist('name')  # list of zero or more files posted
- CombinedMultiDict((request.files, request.form))  # combine form and file data

## Uploading Files
# HTML Form

* Use a [`file` type input][1] and the browser will provide a field that lets the user select a file to upload.
* Only forms with the `post` method can send file data.
* Make sure to set the form's `enctype=multipart/form-data` attribute.  Otherwise the file's name will be sent but not the file's data.
* Use the `multiple` attribute on the input to allow selecting multiple files for the single field.

<!-- language: lang-html -->

    <form method=post enctype=multipart/form-data>
        <!-- single file for the "profile" field -->
        <input type=file name=profile>
        <!-- multiple files for the "charts" field -->
        <input type=file multiple name=charts>
        <input type=submit>
    </form>

# Python Requests

[Requests][2] is a powerful Python library for making HTTP requests.  You can use it (or other tools) to [post files][3] without a browser.

* Open the files to read in binary mode.
* There are multiple data structures that `files` takes.  This demonstrates a list of `(name, data)` tuples, which allows multiple files like the form above.

<!-- language: lang-python -->

    import requests

    with open('profile.txt', 'rb') as f1, open('chart1.csv', 'rb') as f2, open('chart2.csv', 'rb') as f3:
        files = [
            ('profile', f1),
            ('charts', f2),
            ('charts', f3)
        ]
        requests.post('http://localhost:5000/upload', files=files)

---

<sub>This is not meant to be an exhaustive list.  For examples using your favorite tool or more complex scenarios, see the docs for that tool.</sub>


  [1]: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/Input
  [2]: http://docs.python-requests.org/en/latest/
  [3]: http://docs.python-requests.org/en/master/user/quickstart/#post-a-multipart-encoded-file

## Passing data to WTForms and Flask-WTF
WTForms provides a `FileField` to render a file type input.  It doesn't do anything special with the uploaded data.  However, since Flask splits the form data (`request.form`) and the file data (`request.files`), you need to make sure to pass the correct data when creating the form.  You can use a `CombinedMultiDict` to combine the two into a single structure that WTForms understands.

    form = ProfileForm(CombinedMultiDict((request.files, request.form)))

If you're using [Flask-WTF][1], an extension to integrate Flask and WTForms, passing the correct data will be handled for you automatically.

Due to a bug in WTForms, only one file will be present for each field, even if multiple were uploaded.  See [this issue][2] for more details.  It will be fixed in 3.0.


  [1]: http://flask-wtf.readthedocs.io/en/latest/
  [2]: https://github.com/wtforms/wtforms/pull/281

## Save uploads on the server
Uploaded files are available in `request.files`, a [`MultiDict`][1] mapping field names to file objects.  Use `getlist` — instead of `[]` or `get` — if multiple files were uploaded with the same field name.

    request.files['profile']  # single file (even if multiple were sent)
    request.files.getlist('charts')  # list of files (even if one was sent)

The objects in `request.files` have a `save` method which saves the file locally.  Create a common directory to save the files to.

The `filename` attribute is the name the file was uploaded with.  This can be set arbitrarily by the client, so pass it through the `secure_filename` method to generate a valid and safe name to save as.  <sub>This doesn't ensure that the name is *unique*, so existing files will be overwritten unless you do extra work to detect that.</sub>

    import os
    from flask import render_template, request, redirect, url_for
    from werkzeug import secure_filename

    # Create a directory in a known location to save files to.
    uploads_dir = os.path.join(app.instance_path, 'uploads')
    os.makedirs(uploads_dir, exists_ok=True)

    @app.route('/upload', methods=['GET', 'POST'])
    def upload():
        if request.method == 'POST':
            # save the single "profile" file
            profile = request.files['profile']
            profile.save(os.path.join(uploads_dir, secure_filename(profile.filename)))

            # save each "charts" file
            for file in request.files.getlist('charts'):
                file.save(os.path.join(uploads_dir, secure_filename(file.name)))

            return redirect(url_for('upload'))

        return render_template('upload.html')


  [1]: http://werkzeug.pocoo.org/docs/0.11/datastructures/#werkzeug.datastructures.MultiDict

## PARSE CSV FILE UPLOAD AS LIST OF DICTIONARIES IN FLASK WITHOUT SAVING
Developers often need to design web sites that allow users to upload a CSV file.  Usually there is **no reason** to **save** the actual CSV file since the data will processed and/or stored in a database once uploaded.  However, many if not most, PYTHON methods of parsing CSV data requires the data to be read in as a file.  This may present a bit of a headache if you are using **FLASK** for web development. 
 
Suppose our CSV has a header row and looks like the following:

    h1,h2,h3
    'yellow','orange','blue'
    'green','white','black'
    'orange','pink','purple'

Now, suppose the html form to upload a file is as follows:

    <form action="upload.html" method="post" enctype="multipart/form-data">
        <input type="file" name="fileupload" id="fileToUpload">
        <input type="submit" value="Upload File" name="submit">
    </form>

Since no one wants to reinvent the wheel you decide to **IMPORT csv** into your **FLASK** script.  There is no guarantee that people will upload the csv file with the columns in the correct order.  If the csv file has a header row, then with the help of the **csv.DictReader** method you can read the CSV file as a list of dictionaries, keyed by the entries in the header row.  However, **csv.DictReader** needs a file and does not directly accept strings.  You may think you need to use **FLASK** methods to first save the uploaded file, get the new file name and location, open it using **csv.DictReader**, and then delete the file. Seems like a bit of a waste.

Luckily, we can get the file contents as a string and then split the string up by terminated lines.  The csv method **csv.DictReader** will accept this as a substitute to a file.  The following code demonstrates how this can be accomplished without temporarily saving the file. 

    @application.route('upload.html',methods = ['POST'])
    def upload_route_summary():
        if request.method == 'POST':

            # Create variable for uploaded file
            f = request.files['fileupload']  

            #store the file contents as a string
            fstring = f.read()
            
            #create list of dictionaries keyed by header row
            csv_dicts = [{k: v for k, v in row.items()} for row in csv.DictReader(fstring.splitlines(), skipinitialspace=True)]

            #do something list of dictionaries
        return "success"

The variable **csv_dicts** is now the following list of dictionaries:

       csv_dicts = 
        [
            {'h1':'yellow','h2':'orange','h3':'blue'},
            {'h1':'green','h2':'white','h3':'black'},
            {'h1':'orange','h2':'pink','h3':'purple'}
        ]

In case you are new to PYTHON, you can access data like the following:

    csv_dicts[1]['h2'] = 'white'
    csv_dicts[0]['h3'] = 'blue'
    

Other solutions involve importing the **io** module and use the **io.Stream** method.  I feel that this is a more straightforward approach.  I believe the code is a little easier to follow than using the **io** method. This approach is specific to the example of parsing an uploaded CSV file.


