---
title: "Getting started with progress-4gl"
slug: "getting-started-with-progress-4gl"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## FizzBuzz
Another example of "Hello World" style programs is [FizzBuzz][1]. 

    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cOut AS CHARACTER   NO-UNDO.

    DO i = 1 TO 100:
    
        /* Dividable by 3: fizz */
        IF i MODULO 3 = 0 THEN
            cOut = "Fizz".
        /* Dividable by 5: buzz */
        ELSE IF i MODULO 5 = 0 THEN
            cOut = "Buzz".
        /* Otherwise just the number */
        ELSE 
            cOut = STRING(i).
        
        /* Display the output */
        DISPLAY cOut WITH FRAME x1 20 DOWN.
        /* Move the display position in the frame down 1 */
        DOWN WITH FRAME x1.
    END. 


  [1]: https://en.wikipedia.org/wiki/Fizz_buzz

## Installation or Setup
**Installing Progress**

Download your distribution from Progress. If you want a demo license you need to contact them. Make sure you download a 64-bit and not a 32-bit tar file (unless you happen to run a 32-bit machine).

*Windows*

The download will be a zip archive. Unpack it and simply run setup.exe. The installation will be graphical but otherwise exactly like the one described below.

*Linux/Unix/HP-UX etc*

Put the tar file on your Progress system. Let's say you have it in your home directory:

    /home/user/PROGRESSFILENAME.tar

Extract it:

    cd /home/user
    tar xvf PROGRESSFILENAME.tar

It will create a directory named 

    proinst

Change directory to another destination and create a temporary directory there. For example:

    cd /tmp
    mkdir proinst116
    cd proinst116

Once the installation is complete this directory will contain information about the installation as well as files you can save and used for future automatic repetitions of the same installation.

Now run the installationscript (named "proinst" in the directory "proinst"):

    /home/user/proinst/proinst

This will start the installation:

     +-------------------------------------------------------------------+
     |                              Welcome                              |
     +-------------------------------------------------------------------+
     |                                                                   |
     |         WELCOME TO THE OPENEDGE INSTALLATION UTILITY              |
     |                                                                   |
     | Ensure that you have your completed "Preinstallation Checklist    |
     | for Unix" handy to consult. This information will facilitate your |
     | installation and ensure your choices are accurately recorded.     |
     |                                                                   |
     |     Copyright (c) 1984-2015 Progress Software Corporation         |
     |         and/or one of its subsidiaries or affiliates.             |
     |                     All Rights Reserved.                          |
     |                                                                   |
     |                                                                   |
     |                            [Enter=OK]                             |
     +-------------------------------------------------------------------+

Now you will need to insert license keys, company name etc. It's recommended to download an "addendum file" then you can simply press <kbd>Ctrl+A</kbd> and use it.

    +----------------------------------------------------------------------+
    |                     Product Configuration Data                       |
    +----------------------------------------------------------------------+
    |                                                [Enter=Additional]    |
    | Company Name: ______________________________   [Ctrl-E=Done]         |
    | Serial Number: _________                       [CTRL-T=Quit]         |
    | Control Number: _____  _____  _____            [CTRL-N=Release Notes]|
    |                                                [CTRL-V=View]         |
    |                                                [TAB=Next Field]      |
    |                                                [CTRL-P=Help]         |
    |                                                [CTRL-A=Addendum File]|
    |                                                                      |
    +----------------------------------------------------------------------+

Adding an addendum-file:

      +---------------------------------------------------------------------------+
      |                           License Addendum File                           |
      +---------------------------------------------------------------------------+
      |                                                                           |
      | Enter Path: /home/myuser/myfile.txt______________________________________ |
      |                                                                           |
      |                                                                           |
      |                                                                           |
      |      [Enter=OK]  [CTRL-N=Cancel]                                          |
      +---------------------------------------------------------------------------+
        |                                                [TAB=Next Field]      |
        |                                                [CTRL-P=Help]         |
        |                                                [CTRL-A=Addendum File]|
        |                                                                      |
        +----------------------------------------------------------------------+

After you've added licenses manually or loaded them via a file you can press <kbd>Ctrl</kbd>+<kbd>V</kbd> to view products to be installed:

          +------------------------+
          |Entered Product List    |
          +------------------------+
          | 4GL Development System |
          | OE Application Svr Ent |
    +-----| OE Enterprise RDBMS    |---------------------------------------+
    |     | OpenEdge Replication   |nfiguration Data                       |
    +-----+------------------------+---------------------------------------+
    |                                                [Enter=Additional]    |
    | Company Name: ______________________________   [Ctrl-E=Done]         |
    | Serial Number: _________                       [CTRL-T=Quit]         |
    | Control Number: _____  _____  _____            [CTRL-N=Release Notes]|
    |                                                [CTRL-V=View]         |
    |                                                [TAB=Next Field]      |
    |                                                [CTRL-P=Help]         |
    |                                                [CTRL-A=Addendum File]|
    |                                                                      |
    +----------------------------------------------------------------------+

Once you're satisfied, press <kbd>Ctrl+E</kbd> to continue the installation or <kbd>Ctrl</kbd>+<kbd>Q</kbd> to quit.

If you move on you will have to OK just one more thing:

       +-----------------------------------------------------------------------+
       |                 Done Configuration Data Confirmation                  |
       +-----------------------------------------------------------------------+
       |                                                                       |
       |Are you sure that you are done entering all the control numbers for the|
       |OpenEdge products that will be installed?                              |
       |                                                                       |
       |                            [Y=YES] [N=NO]                             |
       +-----------------------------------------------------------------------+
        |                                                [CTRL-P=Help]         |
        |                                                [CTRL-A=Addendum File]|
        |                                                                      |
        +----------------------------------------------------------------------+

Press <kbd>Y</kbd> to continue or <kbd>N</kbd> to go back.

Depending on what you're installing you might need to set up different products during the installation. 

Next step is to decide if you want to enable the "OpenEdge Explorer". <kbd>Y</kbd> or <kbd>N</kbd>. This can be changed later on.

                        +-------------------------------+
                        | Install Type and Destination  |
                        +-------------------------------+
                        | Select Type of Installation   |
                        | Select Destination Pathname   |
                        | Select Management Pathname    |
                        | Continue with Installation    |
                        | View Release Notes            |
                        | Cancel                        |
                        | Quit Installation             |
                        | Help                          |
                        +-------------------------------+




    +-----------------------------------------------------------------------------+
    |Type: Complete Install                                                       |
    |Destination pathname: /usr/dlc                                               |
    |Working Dir pathname: /usr/wrk                                               |
    |Management pathname: /usr/oemgmt                                             |
    |Management Working Dir pathname: /usr/wrk_oemgmt                             |
    |                                                                             |
    +-----------------------------------------------------------------------------+

Now you have to decide directories where you want to install Progress as well as primary working directory (basically where you want to store your code). Change these or move on with the defaults. Historically `/usr/dlc` has always been the default so you might want to change this to something thats unique for this specific version of Progress - that might help when upgrading. Choose a `Complete Install` (the default). 

Once done: choose `Continue with Installation` using arrow keys and press <kbd>enter</kbd> to continue.

        +---------------------------------------------------+
        |                 Select Server Engine              |
        +---------------------------------------------------+
        |*SQL   -Provides SQL access to OpenEdge data files |
        | Continue with Install                             |
        | Cancel                                            |
        | Help                                              |
        +---------------------------------------------------+

If you're not planning any SQL access you can press enter once and remove the `*` before SQL, otherwise just `Continue with Install`.


     +---------------------------------------------------------------------------+
     |                                 ATTENTION                                 |
     +---------------------------------------------------------------------------+
     |                                                                           |
     |The OpenEdge Adapter for Sonic ESB is a recommended component of this      |
     |installation and requires a Sonic ESB installation somewhere on your       |
     |network.                                                                   |
     |                                                                           |
     |Choose YES if you plan on using OpenEdge Adapter for Sonic ESB, else choose|
     |NO.                                                                        |
     |                                                                           |
     |                          [Y=YES] [N=NO] [H=Help]                          |
     +---------------------------------------------------------------------------+

Most likely you do not need the OpenEdge Adapter for Sonic ESB so press <kbd>N</kbd> - otherwise you know what to do.

      +-----------------------------------------------------------------+
      |                            ATTENTION                            |
      +-----------------------------------------------------------------+
      |                                                                 |
      |WebSpeed is a recommended component of this installation and     |
      |requires a Web Server installed somewhere on your network.       |
      |                                                                 |
      |Choose YES if you plan on using WebSpeed and you are installing  |
      |on the system where your Web Server is installed, else choose NO.|
      |                                                                 |
      |                     [Y=YES] [N=NO] [H=Help]                     |
      +-----------------------------------------------------------------+

If you plan on using WebSpeed for producing dynamic HTML press <kbd>Y</kbd>, otherwise <kbd>N</kbd>.

                      +------------------------------------+
                      | Web Server Type                    |
                      +------------------------------------+
                      | Select Web Server Type             |
                      | Select Web Server Script directory |
                      | Copy the static HTML to docroot    |
                      | Continue with Installation         |
                      | Cancel                             |
                      | Quit Installation                  |
                      | Help                               |
                      +------------------------------------+
Setting up WebSpeed: Choose `Select Web Server Type` and set it to `cgi` (most likely anyway). Web server script directory can be set to your servers `cgi-bin` directory or something like `/tmp`. Don't copy the static HTML - it's really outdated. Continue!

                          +----------------------------+
                          |Language Selection          |
                          +----------------------------+
                          | Chinese (Simplified)       |
                          | Czech                      |
                          | Dutch                      |
                          | English - American         |
                          | English - International    |
                          | French                     |
                          | German                     |
                          | Italian                    |
                          | Polish                     |
                          | Portuguese - Brazilian     |
                          | Spanish                    |
                          | Portuguese                 |
                          | Swedish                    |
                          | Spanish - Latin            |
                          | Make Default               |
                          | Continue with Installation |
                          | Cancel                     |
                          | Help                       |
                          +----------------------------+
Choose `English` unless you really need something else, you can actually select more than one - make one default in that case. Continue!

                          +------------------------------------+
                          | International Settings             |
                          +------------------------------------+
                          | Select CharacterSet,Collation,Case |
                          | Select a Date Format               |
                          | Select a Number Format             |
                          | Continue with Installation         |
                          | Cancel                             |
                          | Quit Installation                  |
                          | Help                               |
                          +------------------------------------+
                              | Polish                     |
                              | Portuguese - Brazilian     |
                              | Spanish                    |
                              | Portuguese                 |
                              | Swedish                    |
                              | Spanish - Latin            |
                              | Make Default               |
    +-----------------------------------------------------------------------------+
    |                                                                             |
    | CharacterSet,Collation,Case: ISO8859-1, Swedish, Basic                      |
    | Date Format: ymd                                                            |
    | Number Format: 1.234,56  (period, comma)                                    |
    +-----------------------------------------------------------------------------+

For the Itnernational Settings you should try and match any previous installations to help yourself in the future. Otherwise you can set it to something that fits your own needs. This can be changed in the future. Use `UTF-8` if you want.

      +---------------------------------------------------------------------------+
      |                         Web Services Adapter URL                          |
      +---------------------------------------------------------------------------+
      |   Please enter the URL of where you will configure the sample             |
      |   Web Services Adapter's Java Servlet.                                    |
      |                                                                           |
      | URL: http://fedora-1gb-ams3-01.localdomain:80/wsa/wsa1___________________ |
      |                                                                           |
      |      [Enter=OK]  [CTRL-N=Cancel]  [CTRL-P=Help]                           |
      +---------------------------------------------------------------------------+

Leave the defaults for the Web Services adapter URL unless you have a good reason.

      +-------------------------------------------------------------------------+
      |                           WSA Authentication                            |
      +-------------------------------------------------------------------------+
      |                                                                         |
      |Would you like to Disable the Web Services Adapter's administration user |
      |authentication?                                                          |
      |                                                                         |
      |                         [Y=YES] [N=NO] [H=Help]                         |
      +-------------------------------------------------------------------------+

Disable user authentication? Most likely <kbd>N</kbd> is what you want.

      +-------------------------------------------------------------------------+
      |                          Complete Installation                          |
      +-------------------------------------------------------------------------+
      |                                                                         |
      |The following products will be installed:                                |
      |'4GL Development System (x USERS)', 'OE Application Svr Ent (y USERS)', |
      |'OE Enterprise RDBMS (z USERS)', 'OpenEdge Replication (u USERS)'      |
      |                                                                         |
      |Disk Space Required for Products: 1,138,163,712 bytes                    |
      |Disk Space Required for Installation: 1,139,343,360 bytes                |
      |Disk Space Remaining After Installation: 26,534,129,664 bytes            |
      |                                                                         |
      |Selected Destination Path: /usr/dlc                                      |
      |                                                                         |
      |Do you want to install the above listed product(s)?                      |
      |                                                                         |
      |                         [Y=YES] [N=NO] [H=Help]                         |
      +-------------------------------------------------------------------------+

This is the final (but one) screen before installation begins.

                        +-----------------------------+
                        |        Copy Scripts?        |
                        +-----------------------------+
                        |                             |
                        |Copy the scripts to /usr/bin?|
                        |                             |
                        |   [Y=YES] [N=NO] [H=Help]   |
                        +-----------------------------+
If you choose to do this you might want to make sure there isn't a previous install being overwritten.


     +----------------------------------------------------------------------------+
     |                             Installing Files                               |
     +----------------------------------------------------------------------------+
     |                                                                            |
     |                 Installing subcomponent: Common Files (m)                  |
     |                        Installing file: libjvm.so                          |
     |                                    17%                                     |
     | +------------------------------------------------------------------------+ |
     | |                                                                        | |
     | +------------------------------------------------------------------------+ |
     |                                                                            |
     |                               [CTRL-T=Quit]                                |
     +----------------------------------------------------------------------------+

Installation in process. Takes a minute or two. 

      +-------------------------------------------------------------------------+
      |                          Configuring WebSpeed                           |
      +-------------------------------------------------------------------------+
      |                                                                         |
      | a. Set up and start your Web server                                     |
      |    - If you did not select to "Copy static HTML files to                |
      |      Document Root directory", then manually copy the files             |
      |      or set a link.                                                     |
      |    - For NSAPI Messenger, edit the "obj.conf" and "start" files         |
      |      on the Web server.                                                 |
      | b. Set up the Broker machine.                                           |
      |    - Set environment variables if necessary.                            |
      |    - Edit the properties file (ubroker.properties), then start Broker.  |
      | c. To validate your configuration through the Messenger                 |
      |    Administration Page, enter ?WSMAdmin after the Messenger name        |
      |    in a URL.                                                            |
      |    (For example, for CGI, http://hostname/cgi-bin/wspd_cgi.sh?WSMAdmin) |
      |    (For example, for NSAPI, http://hostname/wsnsa.dll?WSMADmin)         |
      |                                                                         |
      |See the "OpenEdge Application Server: Administration" guide for details. |
      |                                                                         |
      |                           [Enter=OK] [H=Help]                           |
      +-------------------------------------------------------------------------+

Some information about WebSpeed.

 

        +---------------------------------------------------------+
        |Installation of selected OpenEdge products is complete.  |
        |Refer to the installation notes for more information.    |
        +---------------------------------------------------------+
        | End the OpenEdge Installation                           |
        | View Release Notes                                      |
        | Help                                                    |
        +---------------------------------------------------------+

Final screen - End the Installation or View the Release Notes. 

*You are done!*

**Silent installation**

The installation has stored a file named `/usr/dlc/install/response.ini` (or your installation directory). This file can be used to repeat the exact same installation again in a "silent" install that can be scriptet and run without any interaction.

To run a silent install simply do:

    /path-to-proinst/proinst -b /path-to-response-file/response.ini -l /path-to-store-log/silent.log

 









## Hello, World!
Once you've started your Progress editor of choice (there are a couple of options) simply write:

    DISPLAY "Hello, World!".

And run by pressing the corresponding key or menu item:

On Windows in AppBuilder: <kbd>F1</kbd> (Compile -> Run)

On Linux/Unix in the 4GL editor: <kbd>F2</kbd> (or <kbd>ctrl</kbd>+<kbd>X</kbd>) (Compile -> Run)

On Windows in Developer Studio: <kbd>alt</kbd>+<kbd>shift</kbd>+<kbd>X</kbd>, followed by <kbd>G</kbd> (Run -> Run As Progress OpenEdge Application)

## Setting up the environment
**Linux/Unix**

Once you have Progress installed it's very easy to run.

You only need a couple of environment variables. The directory where Progress was installed (default `/usr/dlc` but can be something else) needs to be in the DLC-variable

    DLC=/usr/dlc
    
And you might also want the `"bin"` subdirectory of `DLC` in your `PATH`:

    PATH=$PATH:$DLC/bin
    
Now you're set!

Theres also a script installed called `proenv` that will do this (and a little bit more) for you. It's default location is `/usr/dlc/bin/proenv`.

**Some utilites:**

    showcfg

This will list all your installed products.

    pro

This will start the "Procedure Editor" where you can edit and run your programs.

    pro program.p

Will open program.p for editing if it exists. Otherwise an error will be displayed.

    pro -p program.p

This will run "program.p". If there's a compiled file (program.r) present it will be run, otherwise it will be temporarily compiled and after that executed. The compiled file will not be saved.





## Creating the "sports2000" demo database from the command line
This shows how to create the demo database used in big parts of Progress documentation: sports2000.

This assumes you have installed the Progress products with at least one type of database license.

Run `proenv` script/bat-file that will give you a prompt with all environment variables set.

**Create  a directory.**
 
This example is for Windows. Directory handling etc might be different in another OS.

    proenv> cd \
    proenv> mkdir db
    proenv> cd db
    proenv> mkdir sports2000
    proenv> cd sports2000

**Create a sports2000 database using "prodb"**

    proenv> prodb mySportsDb sports2000

>Syntax of prodb:
>
>prodb name-of-new-database name-and-path-of-source-database

This will create a database called "mySportsDb" in the current directory. That database is an exact copy of the sports2000 database that's shipped with the Progress install. Since the source sports2000 database is located in the Progress install directory you don't need to specify path.

If you look at the directory content you will see some files:

    proenv> dir
    2017-01-12  20:24         2 228 224 mySportsDb.b1
    2017-01-12  20:24         1 703 936 mySportsDb.d1
    2017-01-12  20:24            32 768 mySportsDb.db
    2017-01-12  20:24             2 951 mySportsDb.lg
    2017-01-12  20:07               368 mySportsDb.st
    2017-01-12  20:24           327 680 mySportsDb_10.d1
    2017-01-12  20:24            65 536 mySportsDb_10.d2
    2017-01-12  20:24         1 310 720 mySportsDb_11.d1
    2017-01-12  20:24         1 376 256 mySportsDb_11.d2
    2017-01-12  20:24           327 680 mySportsDb_12.d1
    2017-01-12  20:24            65 536 mySportsDb_12.d2
    2017-01-12  20:24           327 680 mySportsDb_7.d1
    2017-01-12  20:24            65 536 mySportsDb_7.d2
    2017-01-12  20:24           655 360 mySportsDb_8.d1
    2017-01-12  20:24           655 360 mySportsDb_8.d2
    2017-01-12  20:24           327 680 mySportsDb_9.d1
    2017-01-12  20:24            65 536 mySportsDb_9.d2

| File name | Contains | 
|-----------|----------|
|.db|The main database file. Contains the database schema|
|.lg|The database log file. Contains logging information in text format|
|.st|The database structure file. Describe the storage layout in a text format|
|.d?|The actual data. Different files store data of different formats. The .st file can tell what format|
|.b?|Before-Image files. Contains information about transactions in process.|


Now you can access the database directly by simply typing `pro mySportsDb`. This will start a Progress Editor that's connected to the database. This will be a single user connection so nobody else will be able to access the database at the same time.

In the editor you can simply type: 

    FOR EACH bill NO-LOCK: 
      DISPLAY bill. 
    END.

To access the database. Press <kbd>Ctrl</kbd>+<kbd>X</kbd> to execute. This will display all contents of the "bill" table. If you want to cancel you can press <kbd>Ctrl</kbd>+<kbd>C</kbd>.

## Commenting code
    /* 
    In all versions of 
    Progress ABL you can write 
    multi line comments 
    */

    /* They can also span a single line */

    //Starting with version 11.6 you can also write single line comments

    //Can you nest single line comments? //Yes you can
 
    string = "HELLO". //A single line comment can be written after some code

    string2 = "Goodbye". /* And the same thing
    goes for multi line comments. A difference is 
    that a multi line comment also can preceed some code */ i = 1.

    /* Is it possible to mix comments? 
    //Yes, but multi line comments always needs to be terminated! */

    /* You can nest multi line comments as well
    /* but then all nested comments must be terminated */ or the compiler 
    will generate an error */

Formally the single line comment starts with the double slash `//` and ends with a newline, carriage return or end-of-file.



## Program files
Progress ABL code is normally stored in files with different ending depending on what they contain. The endings are optional but rather a defacto standard:

| Filename extension| Contains                   |
| ----------------- | -------------------------- |
| .p                | A Progress program. Can contain several internal procedures, functions etc  |
| .i                | Include file to be included in other files
| .w                | A file containing a graphical representation of a Window or Dialog, WinForm-based.
| .r                | The compiled result of any file containing Progress 4GL. Called r-code.
| .cls              | A Progress Object Oriented Class | 
| .wrx              | A container for ActiveX data whenever needed (generated by compiling in "AppBuilder").

To run a program-file in Progress 4GL the `RUN`-statement is used:

    RUN program.p. //Will run program.p without parameters.
    RUN program.w (INPUT true). //Will run program.w with input parameter set to true.

    RUN program. //Will run program.r if present otherwise internal procedure "program".

To include another file in a Progress-program the `{}`-directive is used:

    {program.i} //Includes program.i in the current program


## Running sports2000 as a service
Once the sports2000 database has been installed it's time to run it as a standalone server (and not connect to it as a file).

Start proenv (`proenv` in the startmeny on Windows or `/usr/install-directory/bin/proenv` on Linux/Unix).

This example is from Windows. Linux is the same but you need to change paths etc to match your install.

    proenv> cd \db\sports2000
    proenv> proserve mySportsDb -H localhost -S 9999
    OpenEdge Release 11.6 as of Fri Oct 16 19:01:51 EDT 2015
    20:09:54 BROKER     This broker will terminate when session ends. (5405)
    20:09:54 BROKER     The startup of this database requires 17Mb of shared memory.  Maximum segment size is 128Mb.
    20:09:54 BROKER  0: Multi-user session begin. (333)
    20:09:55 BROKER  0: Begin Physical Redo Phase at 0 . (5326)
    20:17:36 BROKER  0: Before Image Log Initialization at block 1  offset 5300. (15321)
    20:09:55 BROKER  0: Login by xyz on CON:. (452)
    20:09:55 BROKER  0: Started for 9999 using TCP IPV4 address 127.0.0.1, pid 2892. (5644)
    proenv> 
(You might not get exactly this output).

This will start the mySportsDb on `localhost` and use port 9999 as primary port for database access. If you want to connect to this database from another client on the same network or elsewhere localhost wont work. Use your IP-address or hostname instead:

    proenv> proserve mySportsDb -H 192.168.1.10 -S 9999.

**Connecting and disconnecting**

Once your database is up and running you can connect to it in your Progress editor:

    CONNECT mySportsDb -H localhost -S 9999.
or 

    CONNECT "-db mySportsDb -H localhost -S 9999".

If you get an error message you have either gotten some information wrong in the command or the database isn't up and running. You could also have a software firewall or similar interfering.

You can check the database logfile (`mySportsDb.lg` in this example) for any clues.

Disconnecting is just as easy:

    DISCONNECT mySportDb.

or 

    DISCONNECT "mySportsDb".

**Shutting down the database (or disconnect users**

To shut the database down you can run the `proshut` command from proenv:

    proenv> proshut mySportsDb
    OpenEdge Release 11.6 as of Fri Oct 16 19:01:51 EDT 2015
    usr    pid    time of login           user id     Type  tty                  Limbo?
     24   7044 Wed Feb 01 20:22:57 2017   xyz         REMC  XYZ-PC               no
                1  Disconnect a User
                2  Unconditional Shutdown
                3  Emergency Shutdown (Kill All)
                x  Exit

1) Use `1` to disconnect specific users.
2) Use `2` to shut down the database. **Note:** no questions asked, shutdown starts directly!
3) Use `3` only if you can't take down the database any other way. This might corrupt your data.
4) Use `x` to exit the proshut utility.

You can also shutdown the database directly from the command line:

    proenv>proshut mySportsDb -by
Or disconnect a user from command line (assuming you know it's user number, usr in the list above):

    proenv>proshut mySportsDb -C disconnect 24
    OpenEdge Release 11.6 as of Fri Oct 16 19:01:51 EDT 2015
    User 24 disconnect initiated. (6796)



