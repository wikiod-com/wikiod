---
title: "Getting started with MongoDB"
slug: "getting-started-with-mongodb"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Execution of a JavaScript file in MongoDB
    ./mongo localhost:27017/mydb myjsfile.js

**Explanation:**
This operation executes the `myjsfile.js` script in a `mongo` shell that connects to the `mydb` database on the `mongod` instance accessible via the `localhost` interface on port `27017`. `localhost:27017` is not mandatory as this is the default port `mongodb` uses. 

Also, you can run a `.js` file from within `mongo` console. 

    >load("myjsfile.js")



## Making the output of find readable in shell
We add three records to our collection test as:

    > db.test.insert({"key":"value1","key2":"Val2","key3":"val3"})
    WriteResult({ "nInserted" : 1 })
    > db.test.insert({"key":"value2","key2":"Val21","key3":"val31"})
    WriteResult({ "nInserted" : 1 })
    > db.test.insert({"key":"value3","key2":"Val22","key3":"val33"})
    WriteResult({ "nInserted" : 1 })

If we see them via find, they will look very ugly.

    > db.test.find()
    { "_id" : ObjectId("5790c5cecae25b3d38c3c7ae"), "key" : "value1", "key2" : "Val2
    ", "key3" : "val3" }
    { "_id" : ObjectId("5790c5d9cae25b3d38c3c7af"), "key" : "value2", "key2" : "Val2
    1", "key3" : "val31" }
    { "_id" : ObjectId("5790c5e9cae25b3d38c3c7b0"), "key" : "value3", "key2" : "Val2
    2", "key3" : "val33" }

To work around this and make them readable, use the **pretty**() function.

    > db.test.find().pretty()
    {
            "_id" : ObjectId("5790c5cecae25b3d38c3c7ae"),
            "key" : "value1",
            "key2" : "Val2",
            "key3" : "val3"
    }
    {
            "_id" : ObjectId("5790c5d9cae25b3d38c3c7af"),
            "key" : "value2",
            "key2" : "Val21",
            "key3" : "val31"
    }
    {
            "_id" : ObjectId("5790c5e9cae25b3d38c3c7b0"),
            "key" : "value3",
            "key2" : "Val22",
            "key3" : "val33"
    }
    >



## Complementary Terms
| SQL Terms  | MongoDB Terms |
|------      | ------        |
|Database    | Database      |
|Table       | Collection    |
|Entity / Row| Document      |
|Column      | Key / Field   |
|Table Join  |[Embedded Documents][1]
|Primary Key |[Primary Key][2] (Default key `_id` provided by mongodb itself)


  [1]: https://docs.mongodb.com/manual/tutorial/model-embedded-one-to-many-relationships-between-documents/
  [2]: https://docs.mongodb.com/manual/indexes/#default-id-index

## Installation
To install MongoDB, follow the steps below:
 - **For Mac OS:**
    - There are two options for Mac OS: manual install or [homebrew][1].
    - **Installing with [homebrew][1]:**
        - Type the following command into the terminal:
            <!-- language-all: lang-none -->
              $ brew install mongodb
    - **Installing manually:**
        - Download the latest release
   [here][2]. Make sure that you are
   downloading the appropriate file, specially check whether your
   operating system type is 32-bit or 64-bit. The downloaded file is in format `tgz`.
        - Go to the directory where this file is downloaded. Then type the following command:
          <!-- language-all: lang-none -->        
              $ tar xvf mongodb-osx-xyz.tgz
            Instead of `xyz`, there would be some version and system type information. The extracted folder would be same name as the `tgz` file. Inside the folder, their would be a subfolder named `bin` which would contain several binary file along with `mongod` and `mongo`. 
         - By default server keeps data in folder `/data/db`. So, we have to create that directory and then run the server having the following commands:

               $ sudo bash
               # mkdir -p /data/db
               # chmod 777 /data
               # chmod 777 /data/db
               # exit
         - To start the server, the following command should be given from the current location:

               $ ./mongod
            It would start the server on port 27017 by default.
        - To start the client, a new terminal should be opened having the same directory as before. Then the following command would start the client and connect to the server.

              $ ./mongo
            By default it connects to the `test` database. If you see the line like `connecting to: test`. Then you have successfully installed MongoDB. Congrats! Now, you can test [Hello World][3] to be more confident.

  - **For Windows:**
     -  Download the latest release
   [here][2]. Make sure that you are
   downloading the appropriate file, specially check whether your
   operating system type is 32-bit or 64-bit.

    - The downloaded binary file has extension `exe`. Run it. It will prompt an installation wizard.
    - Click **Next**.
    - **Accept** the licence agreement and click **Next**.
    - Select **Complete** Installation.
    - Click on **Install**. It might prompt a window for asking administrator's permission. Click **Yes**.
    - After installation click on **Finish**.
    - Now, the mongodb is installed on the path `C:/Program Files/MongoDB/Server/3.2/bin`. Instead of version 3.2, there could be some other version for your case. The path name would be changed accordingly.
    - `bin` directory contain several binary file along with `mongod` and `mongo`. To run it from other folder, you could add the path in system path. To do it:
        - Right click on **My Computer** and select **Properties**.
        - Click on **Advanced system setting** on the left pane.
        - Click on **Environment Variables...** under the **Advanced** tab.
        - Select **Path** from **System variables** section and click on **Edit...**.
        - Before Windows 10, append a semi-colon and paste the path given above. From Windows 10, there is a **New** button to add new path. 
        - Click **OK**s to save changes.
    - Now, create a folder named `data` having a sub-folder named `db` where you want to run the server.
    - Start command prompt from their. Either changing the path in cmd or clicking on **Open command window here** which would be visible after right clicking on the empty space of the folder GUI pressing the Shift and Ctrl key together.

    - Write the command to start the server:
        
          > mongod
        It would start the server on port 27017 by default.
    - Open another command prompt and type the following to start client:
          
          > mongo

    - By default it connects to the `test` database. If you see the line like `connecting to: test`. Then you have successfully installed MongoDB. Congrats! Now, you can test [Hello World][4] to be more confident.
    
- **For Linux:** Almost same as Mac OS except some equivalent command is needed.
    - For Debian-based distros (using `apt-get`):
        - Import MongoDB Repository key.

              $ sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv EA312927
              gpg: Total number processed: 1\
              gpg:               imported: 1  (RSA: 1)
        - Add repository to package list on **Ubuntu 16.04**.

              $ echo "deb http://repo.mongodb.org/apt/ubuntu xenial/mongodb-org/3.2 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-3.2.list
        - on **Ubuntu 14.04**.

              $ echo "deb http://repo.mongodb.org/apt/ubuntu trusty/mongodb-org/3.2 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-3.2.list
        - Update package list.

              $ sudo apt-get update
        - Install MongoDB.

              $ sudo apt-get install mongodb-org
    - For Red Hat based distros (using `yum`):
        - use a text editor which you prefer.

            $  vi /etc/yum.repos.d/mongodb-org-3.4.repo
        - Paste following text.

              [mongodb-org-3.4]
              name=MongoDB Repository
              baseurl=https://repo.mongodb.org/yum/redhat/$releasever/mongodb-org/3.4/x86_64/
              gpgcheck=1
              enabled=1
              gpgkey=https://www.mongodb.org/static/pgp/server-3.4.asc
        - Update package list.

              $ sudo yum update
        - Install MongoDB

              $ sudo yum install mongodb-org


  [1]: https://brew.sh/
  [2]: https://www.mongodb.com/download-center#community
  [3]: https://www.wikiod.com/mongodb/getting-started-with-mongodb#Hello World
  [4]: https://www.wikiod.com/mongodb/getting-started-with-mongodb#Hello World

## Basic commands on mongo shell
Show all available databases:

    show dbs;

Select a particular database to access, e.g. `mydb`. This will create `mydb` if it does not already exist:

    use mydb;

Show all collections in the database (be sure to select one first, see above):

    show collections; 

Show all functions that can be used with the database:

    db.mydb.help();

To check your currently selected database, use the command `db`

    > db
    mydb
`db.dropDatabase()` command is used to drop a existing database.

    db.dropDatabase()

## Hello World
After installation process, the following lines should be entered in mongo shell (client terminal).

    > db.world.insert({ "speech" : "Hello World!" });
    > cur = db.world.find();x=cur.next();print(x["speech"]);

> Hello World!

**Explanation:** 
- In the first line, we have inserted a `{ key : value }` paired document in the default database `test` and in the collection named `world`.
- In the second line we retrieve the data we have just inserted. The retrieved data is kept in a javascript variable named `cur`. Then by the `next()` function, we retrieved the first and only document and kept it in another js variable named `x`. Then printed the value of the document providing the key.

    

