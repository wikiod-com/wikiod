---
title: "New Project with Cloud Resource Manager API Client for Python"
slug: "new-project-with-cloud-resource-manager-api-client-for-python"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

We will use [Google API Client Libraries](https://developers.google.com/discovery/libraries) for Python for this sample.

There are other libraries. Please see [Google Client Libraries Explained](https://cloud.google.com/apis/docs/client-libraries-explained) for details.

We will use the [Cloud Resource Manager](https://cloud.google.com/resource-manager/) API for [Creating and Managing Projects](https://cloud.google.com/resource-manager/docs/creating-managing-projects).

Let's get started.

Putting it all together...

If you followed the examples above, you should in a directory called `my_project_folder` and it hopefully contains a subdirectory called `venv`.

Ensure your virtualenv is activated.

All the code can be placed in one file, let's call it `create_project.py`.

Create the file `create_project.py` in `my_project_folder`.
```
import json
import time

from apiclient.discovery import build
from oauth2client.client import GoogleCredentials

SERVICE_NAME = "cloudresourcemanager"
SERVICE_VERSION = "v1"

# Don't forget to replace YOUR-PROJECT-ID with your choice of Project ID
# Even though the code deletes the Project, change this value each time
PROJECT_ID = "YOUR-PROJECT-ID"

credentials = GoogleCredentials.get_application_default()

service = build(
    SERVICE_NAME,
    SERVICE_VERSION,
    credentials=credentials)

operation1 = service.projects().create(
    body={
        "project_id": PROJECT_ID
    }
).execute()

print(json.dumps(
    operation1,
    sort_keys=True,
    indent=3))

name = operation1["name"]
while True:
    operation2 = service.operations().get(
        name=name
    ).execute()
    print(json.dumps(
        operation2,
        sort_keys=True,
        indent=3))
    if "done" in operation2:
        if (operation2["done"]):
            break
    time.sleep(1)

raw_input("Press Enter to delete the Project...")

operation3 = service.projects().delete(
    projectId=PROJECT_ID
).execute()

```
Save the file and, from the command-prompt type:
```
python create_project.py
```
The output should be similar to:
```
{
   "metadata": {
      "@type": "type.googleapis.com/google.cloudresourcemanager.v1.ProjectCreationStatus", 
      "createTime": "2017-12-31T00:00:00.000Z"
   }, 
   "name": "operations/pc.1234567890123456789"
}
...
{
   "done": true, 
   "metadata": {
      "@type": "type.googleapis.com/google.cloudresourcemanager.v1.ProjectCreationStatus", 
      "createTime": "2017-12-31T00:00:00.000Z" 
      "gettable": true, 
      "ready": true
   }, 
   "name": "operations/pc.1234567890123456789", 
   "response": {
      "@type": "type.googleapis.com/google.cloudresourcemanager.v1.Project", 
      "createTime": "2017-12-31T00:00:00.000Z", 
      "lifecycleState": "ACTIVE", 
      "projectId": "your-project-id", 
      "projectNumber": "123456789012"
   }
}
...
Press Enter to delete the Project...
```


## Getting Started
You will need to be able to run Python.

Python is available for Linux, Mac OS X and Windows.

I recommend [pip](https://pypi.python.org/pypi/pip) and [virtualenv](https://python-docs.readthedocs.io/en/latest/dev/virtualenvs.html).

Pip is the recommend tool for installing Python packages.

The Google Cloud API Libraries are available as pip packages.

A Virtual Environment (aka "virtualenv") is a "tool to keep dependencies required by different projects in separate places".

Create a directory for your project and then:
```
cd my_project_folder
virtualenv venv
```
You may name your virtualenv folder other than "venv" but this name is a useful reminder of the directory's purpose. virtualenv should display something similar to:
```
New python executable in venv/bin/python
Installing setuptools, pip, wheel...done.
```
The virtualenv is created but, to use it, we must `activate` it. When we're done using it, it's a good practice (although not mandatory) to `deactivate` it too.

```
source venv/bin/activate
```
To indicate that we're in the virtualenv called venv, the command-prompt on my Linux machine changes. You will remain in your project directory. Your mileage may vary, but:
```
(venv) user@host
```
Now let's upgrade pip and install the Google API Client Libraries

```
pip install --upgrade pip
pip install --upgrade google-api-python-client
```

All being well if you type `pip freeze`, you should see something similar to this list. Your versions may be higher:
```
pip freeze
google-api-python-client==1.6.2
httplib2==0.10.3
oauth2client==4.0.0
pyasn1==0.2.3
pyasn1-modules==0.0.8
rsa==3.4.2
six==1.10.0
uritemplate==3.0.0
```

Your Python environment is now ready, the Google API Client Libraries are installed. We can write our Python code. Please continue to the next example.

When you're finished with the project, it's a good practice to `deactivate` the virtualenv. Please don't do this now as we're going to write some code. But, when you're done, please return here and:
```
deactivate
```
If you wish to return to the virtualenv, just re-run the `activate` command

## Application Default Credentials
I won't repeat it all here: "[Application Default Credentials](https://developers.google.com/identity/protocols/application-default-credentials) provide a simple way to get authorization credentials for use calling Google APIs."

If you can use Application Default Credentials, do.

There is an extra step you will need to perform the before first using Application Default Credentials as your identity when calling APIs from your machine:

```
gcloud auth application-default login [yourname@gmail.com]
```
Here's why you'll prefer to use Application Default Credentials:
```
scopes = [
    "https://www.googleapis.com/auth/cloud-platform"
]
credentials = GoogleCredentials.get_application_default()
if credentials.create_scoped_required():
    credentials = credentials.create_scoped(scopes)
```
In truth, you can generally get away with just:
```
credentials = GoogleCredentials.get_application_default()
```
...That's all the code you need to authorize calls to (any) Google Cloud API!

We'll use the `credential` object in the next step to make calls against the Google service...

## Calling any method (!) on any Google service (!)
Once you become familiar with the code to call one method on one Google service, you will be able to infer how to call **any** method on **any** Google service.

First, we make a connection to the service using the `credential` object instantiated in the previous example:
```
service = build(
    SERVICE_NAME,
    SERVICE_VERSION,
    credentials=credentials)
```
Then, we can call methods provided by the service. What methods are available?

https://cloud.google.com/resource-manager/docs/apis

What is the underlying REST request for Projects.Create?

https://cloud.google.com/resource-manager/reference/rest/v1/projects/create

OK... Let's write the code.

The `create` method expects a body minimally containing the Project ID. Project IDs are unique identifiers. I recommend that you use a system for naming your projects to help you identify them. The method also accepts a Project Name, Labels, details of the Project's parents etc.

```
operation1 = service.projects().create(
  body={
    "project_id": PROJECT_ID
  }
).execute()
```
Project creation is handled asynchronously. We are given an Operation object that we must poll to determine when the Project is created. Operations have a Name property that uniquely identifies the operation. The next section of code polls the platform "Are we done yet?". The project will be created when our new operation includes a `Done` property that is `True`.

```
name = operation1["name"]
while True:
    operation2 = service.operations().get(
        name=name
    ).execute()
    if "done" in operation2:
        if (operation2["done"]):
            break
    time.sleep(1)
```

For completeness, and hopefully many years from now after much happy use of your project, you may need to delete your project. We simply call the delete method and provide our Project ID. This also returns an operation but I'll leave it to you to poll the operation until it completes
```
operation3 = service.projects().delete(
    projectId=PROJECT_ID
).execute()
```
That's it!


