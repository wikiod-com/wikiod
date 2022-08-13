---
title: "Dockerfiles"
slug: "dockerfiles"
draft: false
images: []
weight: 9869
type: docs
toc: true
---

Dockerfiles are files used to programatically build Docker images. They allow you to quickly and reproducibly create a Docker image, and so are useful for collaborating. 
Dockerfiles contain instructions for building a Docker image. Each instruction is written on one row, and is given in the form `<INSTRUCTION><argument(s)>`. Dockerfiles are used to build Docker images using the `docker build` command.

Dockerfiles are of the form:

    # This is a comment
    INSTRUCTION arguments

- Comments starts with a ``#``
- Instructions are upper case only
- The first instruction of a Dockerfile must be ``FROM`` to specify the base image

****

When building a Dockerfile, the Docker client will send a "build context" to the Docker daemon. The build context includes all files and folder in the same directory as the Dockerfile. ``COPY`` and ``ADD`` operations can only use files from this context.

****

Some Docker file may start with:

    # escape=`

This is used to instruct the Docker parser to use `` ` `` as an escape character instead of ``\``. This is mostly useful for Windows Docker files.

## Dockerfiles best pratices
**Group common operations**

Docker builds images as a collection of layers. Each layer can only add data, even if this data says that a file has been deleted. Every instruction creates a new layer. For example:

    RUN apt-get -qq update
    RUN apt-get -qq install some-package

Has a couple of downsides:
- It will create two layers, producing a larger image.
- Using ``apt-get update`` alone in a ``RUN`` statement causes caching issues and subsequently ``apt-get install`` instructions may **fail**. Suppose you later modify ``apt-get install`` by adding extra packages, then docker interprets the initial and modified instructions as identical and reuses the cache from previous steps. As a result the ``apt-get update`` command is **not** executed because its cached version is used during the build.

Instead, use:

    RUN apt-get -qq update && \
        apt-get -qq install some-package

as this only produce one layer.

**Mention the maintainer**

This is usually the second line of the Dockerfile. It tells who is in charge and will be able to help.

    LABEL maintainer John Doe <john.doe@example.com>

If you skip it, it will not break your image. But it will not help your users either.

**Be concise**

Keep your Dockerfile short. If a complex setup is necessary, consider using a dedicated script or setting up base images.

## EXPOSE Instruction
    EXPOSE <port> [<port>...]

The `EXPOSE` instruction informs Docker that the container listens on the specified network ports at runtime. `EXPOSE` does NOT make the ports of the container accessible to the host. To do that, you must use either the `-p` flag to publish a range of ports or the `-P` flag to publish all of the exposed ports. These flags are used in the `docker run [OPTIONS] IMAGE [COMMAND][ARG...]` to expose the port to the host. You can expose one port number and publish it externally under another number. 

    docker run -p 2500:80 <image name>
This command will create a container with the name \<image> and bind the container’s port 80 to the host machine’s port 2500.

To set up port redirection on the host system, see using the `-P` flag. The Docker network feature supports creating networks without the need to expose ports within the network, for detailed information see the overview of this feature).


## CMD Instruction
The `CMD` instruction has three forms:

    CMD ["executable","param1","param2"] (exec form, this is the preferred form)
    CMD ["param1","param2"] (as default parameters to ENTRYPOINT)
    CMD command param1 param2 (shell form)

There can only be one `CMD` instruction in a `Dockerfile`. If you list more than one `CMD` then only the last `CMD` will take effect.

The main purpose of a `CMD` is to provide defaults for an executing container. These defaults can include an executable, or they can omit the executable, in which case you must specify an `ENTRYPOINT` instruction as well.

Note: If `CMD` is used to provide default arguments for the `ENTRYPOINT` instruction, both the `CMD` and `ENTRYPOINT` instructions should be specified with the JSON array format.

Note: The exec form is parsed as a JSON array, which means that you must use double-quotes (“) around words not single-quotes (‘).

Note: Unlike the shell form, the exec form does not invoke a command shell. This means that normal shell processing does not happen. For example, `CMD [ "echo", "$HOME" ]` will not do variable substitution on `$HOME`. If you want shell processing then either use the shell form or execute a shell directly, for example: `CMD [ "sh", "-c", "echo $HOME" ]`.

When used in the shell or exec formats, the `CMD` instruction sets the command to be executed when running the image.

If you use the shell form of the `CMD`, then the command will execute in `/bin/sh -c`:

    FROM ubuntu
    CMD echo "This is a test." | wc -

If you want to run your command without a shell then you must express the command as a JSON array and give the full path to the executable. This array form is the preferred format of `CMD`. Any additional parameters must be individually expressed as strings in the array:

    FROM ubuntu
    CMD ["/usr/bin/wc","--help"]

If you would like your container to run the same executable every time, then you should consider using `ENTRYPOINT` in combination with `CMD`. See `ENTRYPOINT`.

If the user specifies arguments to docker run then they will override the default specified in `CMD`.

Note: don’t confuse `RUN` with `CMD`. `RUN` actually runs a command at image building time and commits the result; `CMD` does not execute anything at build time, but specifies the intended command for the image.

## The ENV and ARG Instruction
## ENV
    ENV <key> <value>
    ENV <key>=<value> ...

The `ENV` instruction sets the environment variable `<key>` to the value <value>. This value will be in the environment of all “descendant” Dockerfile commands and can be replaced inline in many as well.

The `ENV` instruction has two forms. The first form, `ENV <key> <value>`, will set a single variable to a value. The entire string after the first space will be treated as the `<value>` - including characters such as spaces and quotes.

The second form, `ENV <key>=<value> ...`, allows for multiple variables to be set at one time. Notice that the second form uses the equals sign (=) in the syntax, while the first form does not. Like command line parsing, quotes and backslashes can be used to include spaces within values.

For example:

    ENV myName="John Doe" myDog=Rex\ The\ Dog \
        myCat=fluffy
and

    ENV myName John Doe
    ENV myDog Rex The Dog
    ENV myCat fluffy

will yield the same net results in the final container, but the first form is preferred because it produces a single cache layer.

The environment variables set using `ENV` will persist when a container is run from the resulting image. You can view the values using docker inspect, and change them using `docker run --env <key>=<value>`.


## ARG

If you don't wish to persist the setting, use `ARG` instead. `ARG` will set environments only during the build. For example, setting 

    ENV DEBIAN_FRONTEND noninteractive

may confuse `apt-get` users on a Debian-based image when they enter the container in an interactive context via `docker exec -it the-container bash`.

Instead, use:

    ARG DEBIAN_FRONTEND noninteractive

You might alternativly also set a value for a single command only by using:

    RUN <key>=<value> <command>

## HelloWorld Dockerfile
**A minimal Dockerfile looks like this:**

    FROM alpine
    CMD ["echo", "Hello StackOverflow!"]

This will instruct Docker to build an image based on [Alpine][1] (``FROM``), a minimal distribution for containers, and to run a specific command (``CMD``) when executing the resulting image.

**Build and run it:**

    docker build -t hello .
    docker run --rm hello

**This will output:**

    Hello StackOverflow!


  [1]: https://hub.docker.com/_/alpine/

## USER Instruction
    USER daemon

The `USER` instruction sets the user name or UID to use when running the image and for any `RUN`, `CMD` and `ENTRYPOINT` instructions that follow it in the `Dockerfile`.

## WORKDIR Instruction
    WORKDIR /path/to/workdir

The `WORKDIR` instruction sets the working directory for any `RUN`, `CMD`, `ENTRYPOINT`, `COPY` and `ADD` instructions that follow it in the Dockerfile. If the `WORKDIR` doesn’t exist, it will be created even if it’s not used in any subsequent `Dockerfile` instruction.

It can be used multiple times in the one `Dockerfile`. If a relative path is provided, it will be relative to the path of the previous `WORKDIR` instruction. For example:

    WORKDIR /a
    WORKDIR b
    WORKDIR c
    RUN pwd

The output of the final `pwd` command in this `Dockerfile` would be `/a/b/c`.

The `WORKDIR` instruction can resolve environment variables previously set using `ENV`. You can only use environment variables explicitly set in the `Dockerfile`. For example:

    ENV DIRPATH /path
    WORKDIR $DIRPATH/$DIRNAME
    RUN pwd

The output of the final `pwd` command in this Dockerfile would be `/path/$DIRNAME`

## VOLUME Instruction
    VOLUME ["/data"]

The `VOLUME` instruction creates a mount point with the specified name and marks it as holding externally mounted volumes from native host or other containers. The value can be a JSON array, `VOLUME ["/var/log/"]`, or a plain string with multiple arguments, such as `VOLUME /var/log` or `VOLUME /var/log /var/db`. For more information/examples and mounting instructions via the Docker client, refer to Share Directories via Volumes documentation.

The `docker run` command initializes the newly created volume with any data that exists at the specified location within the base image. For example, consider the following Dockerfile snippet:

    FROM ubuntu
    RUN mkdir /myvol
    RUN echo "hello world" > /myvol/greeting
    VOLUME /myvol

This Dockerfile results in an image that causes docker run, to create a new mount point at /myvol and copy the greeting file into the newly created volume.

Note: If any build steps change the data within the volume after it has been declared, those changes will be discarded.

Note: The list is parsed as a JSON array, which means that you must use double-quotes (“) around words not single-quotes (‘).

## COPY Instruction
`COPY` has two forms:

    COPY <src>... <dest>
    COPY ["<src>",... "<dest>"] (this form is required for paths containing whitespace)

The `COPY` instruction copies new files or directories from `<src>` and adds them to the filesystem of the container at the path `<dest>`.

Multiple `<src>` resource may be specified but they must be relative to the source directory that is being built (the context of the build).

Each `<src>` may contain wildcards and matching will be done using Go’s `filepath.Match` rules. For example:

    COPY hom* /mydir/        # adds all files starting with "hom"
    COPY hom?.txt /mydir/    # ? is replaced with any single character, e.g., "home.txt"

The `<dest>` is an absolute path, or a path relative to [`WORKDIR`](https://www.wikiod.com/docker/dockerfiles#WORKDIR Instruction), into which the source will be copied inside the destination container.

    COPY test relativeDir/   # adds "test" to `WORKDIR`/relativeDir/
    COPY test /absoluteDir/  # adds "test" to /absoluteDir/

All new files and directories are created with a UID and GID of 0.

Note: If you build using stdin (`docker build - < somefile`), there is no build context, so `COPY` can’t be used.

`COPY` obeys the following rules:

- The `<src>` path must be inside the context of the build; you cannot `COPY` ../something /something, because the first step of a docker build is to send the context directory (and subdirectories) to the docker daemon.

- If `<src>` is a directory, the entire contents of the directory are copied, including filesystem metadata.
Note: The directory itself is not copied, just its contents.

- If `<src>` is any other kind of file, it is copied individually along with its metadata. In this case, if `<dest>` ends with a trailing slash /, it will be considered a directory and the contents of `<src>` will be written at `<dest>/base(<src>)`.

- If multiple `<src>` resources are specified, either directly or due to the use of a wildcard, then `<dest>` must be a directory, and it must end with a slash `/`.

- If `<dest>` does not end with a trailing slash, it will be considered a regular file and the contents of `<src>` will be written at `<dest>`.

- If `<dest>` doesn’t exist, it is created along with all missing directories in its path.

## LABEL  Instruction
    LABEL <key>=<value> <key>=<value> <key>=<value> ...

The `LABEL` instruction adds metadata to an image. A `LABEL` is a key-value pair. To include spaces within a `LABEL` value, use quotes and backslashes as you would in command-line parsing. A few usage examples:

    LABEL "com.example.vendor"="ACME Incorporated"
    LABEL com.example.label-with-value="foo"
    LABEL version="1.0"
    LABEL description="This text illustrates \
    that label-values can span multiple lines."

An image can have more than one label. To specify multiple labels, Docker recommends combining labels into a single `LABEL` instruction where possible. Each `LABEL` instruction produces a new layer which can result in an inefficient image if you use many labels. This example results in a single image layer.

    LABEL multi.label1="value1" multi.label2="value2" other="value3"

The above can also be written as:

    LABEL multi.label1="value1" \
          multi.label2="value2" \
          other="value3"

Labels are additive including `LABEL`s in `FROM` images. If Docker encounters a label/key that already exists, the new value overrides any previous labels with identical keys.

To view an image’s labels, use the docker inspect command.

    "Labels": {
        "com.example.vendor": "ACME Incorporated"
        "com.example.label-with-value": "foo",
        "version": "1.0",
        "description": "This text illustrates that label-values can span multiple lines.",
        "multi.label1": "value1",
        "multi.label2": "value2",
        "other": "value3"
    },

## MAINTAINER Instruction
    MAINTAINER <name>

The `MAINTAINER` instruction allows you to set the Author field of the generated images.

**DO NOT USE THE MAINTAINER DIRECTIVE**

According to [Official Docker Documentation](https://docs.docker.com/engine/reference/builder/#copy) the `MAINTAINER` instruction is deprecated. Instead, one should use the `LABEL` instruction to define the author of the generated images. The `LABEL` instruction is more flexible, enables setting metadata, and can be easily viewed with the command `docker inspect`.

    LABEL maintainer="someone@something.com"

## FROM  Instruction
    FROM <image>

Or

    FROM <image>:<tag>

Or

    FROM <image>@<digest>

The `FROM` instruction sets the Base Image for subsequent instructions. As such, a valid Dockerfile must have `FROM` as its first instruction. The image can be any valid image – it is especially easy to start by pulling an image from the Public Repositories.

`FROM` must be the first non-comment instruction in the Dockerfile.

`FROM` can appear multiple times within a single Dockerfile in order to create multiple images. Simply make a note of the last image ID output by the commit before each new `FROM` command.

The tag or digest values are optional. If you omit either of them, the builder assumes a latest by default. The builder returns an error if it cannot match the tag value.

## RUN Instruction
`RUN` has 2 forms:

    RUN <command> (shell form, the command is run in a shell, which by default is /bin/sh -c on Linux or cmd /S /C on Windows)
    RUN ["executable", "param1", "param2"] (exec form)

The `RUN` instruction will execute any commands in a new layer on top of the current image and commit the results. The resulting committed image will be used for the next step in the `Dockerfile`.

Layering `RUN` instructions and generating commits conforms to the core concepts of Docker where commits are cheap and containers can be created from any point in an image’s history, much like source control.

The exec form makes it possible to avoid shell string munging, and to `RUN` commands using a base image that does not contain the specified shell executable.

The default shell for the shell form can be changed using the `SHELL` command.

In the shell form you can use a `\` (backslash) to continue a single `RUN` instruction onto the next line. For example, consider these two lines:

    RUN /bin/bash -c 'source $HOME/.bashrc ;\
    echo $HOME'

Together they are equivalent to this single line:

    RUN /bin/bash -c 'source $HOME/.bashrc ; echo $HOME'

Note: To use a different shell, other than ‘/bin/sh’, use the exec form passing in the desired shell. For example, `RUN ["/bin/bash", "-c", "echo hello"]`

Note: The exec form is parsed as a JSON array, which means that you must use double-quotes (`“`) around words not single-quotes (`‘`).

Note: Unlike the shell form, the exec form does not invoke a command shell. This means that normal shell processing does not happen. For example, `RUN [ "echo", "$HOME" ]` will not do variable substitution on `$HOME`. If you want shell processing then either use the shell form or execute a shell directly, for example: `RUN [ "sh", "-c", "echo $HOME" ]`.

Note: In the JSON form, it is necessary to escape backslashes. This is particularly relevant on Windows where the backslash is the path separator. The following line would otherwise be treated as shell form due to not being valid JSON, and fail in an unexpected way: `RUN ["c:\windows\system32\tasklist.exe"]` 

The correct syntax for this example is: `RUN ["c:\\windows\\system32\\tasklist.exe"]`

The cache for `RUN` instructions isn’t invalidated automatically during the next build. The cache for an instruction like `RUN apt-get dist-upgrade -y` will be reused during the next build. The cache for `RUN` instructions can be invalidated by using the --no-cache flag, for example docker build --no-cache.

See the Dockerfile Best Practices guide for more information.

The cache for `RUN` instructions can be invalidated by `ADD` instructions. See below for details.

## ONBUILD  Instruction
    ONBUILD [INSTRUCTION]

The `ONBUILD` instruction adds to the image a trigger instruction to be executed at a later time, when the image is used as the base for another build. The trigger will be executed in the context of the downstream build, as if it had been inserted immediately after the `FROM` instruction in the downstream Dockerfile.

Any build instruction can be registered as a trigger.

This is useful if you are building an image which will be used as a base to build other images, for example an application build environment or a daemon which may be customized with user-specific configuration.

For example, if your image is a reusable Python application builder, it will require application source code to be added in a particular directory, and it might require a build script to be called after that. You can’t just call `ADD` and `RUN` now, because you don’t yet have access to the application source code, and it will be different for each application build. You could simply provide application developers with a boilerplate Dockerfile to copy-paste into their application, but that is inefficient, error-prone and difficult to update because it mixes with application-specific code.

The solution is to use `ONBUILD` to register advance instructions to run later, during the next build stage.

Here’s how it works:

When it encounters an `ONBUILD` instruction, the builder adds a trigger to the metadata of the image being built. The instruction does not otherwise affect the current build.

At the end of the build, a list of all triggers is stored in the image manifest, under the key OnBuild. They can be inspected with the `docker inspect` command.
Later the image may be used as a base for a new build, using the `FROM` instruction. As part of processing the `FROM` instruction, the downstream builder looks for `ONBUILD` triggers, and executes them in the same order they were registered. If any of the triggers fail, the `FROM` instruction is aborted which in turn causes the build to fail. If all triggers succeed, the `FROM` instruction completes and the build continues as usual.

Triggers are cleared from the final image after being executed. In other words they are not inherited by “grand-children” builds.

For example you might add something like this:

    [...]
    ONBUILD ADD . /app/src
    ONBUILD RUN /usr/local/bin/python-build --dir /app/src
    [...]

Warning: Chaining `ONBUILD` instructions using `ONBUILD` `ONBUILD` isn’t allowed.

Warning: The `ONBUILD` instruction may not trigger `FROM` or `MAINTAINER` instructions.

## STOPSIGNAL Instruction
    STOPSIGNAL signal

The `STOPSIGNAL` instruction sets the system call signal that will be sent to the container to exit. This signal can be a valid unsigned number that matches a position in the kernel’s syscall table, for instance 9, or a signal name in the format SIGNAME, for instance SIGKILL.

## HEALTHCHECK  Instruction
The `HEALTHCHECK` instruction has two forms:

    HEALTHCHECK [OPTIONS] CMD command (check container health by running a command inside the container)
    HEALTHCHECK NONE (disable any healthcheck inherited from the base image)


The `HEALTHCHECK` instruction tells Docker how to test a container to check that it is still working. This can detect cases such as a web server that is stuck in an infinite loop and unable to handle new connections, even though the server process is still running.

When a container has a healthcheck specified, it has a health status in addition to its normal status. This status is initially starting. Whenever a health check passes, it becomes healthy (whatever state it was previously in). After a certain number of consecutive failures, it becomes unhealthy.

The options that can appear before `CMD` are:

    --interval=DURATION (default: 30s)
    --timeout=DURATION (default: 30s)
    --retries=N (default: 3)

The health check will first run interval seconds after the container is started, and then again interval seconds after each previous check completes.

If a single run of the check takes longer than timeout seconds then the check is considered to have failed.

It takes retries consecutive failures of the health check for the container to be considered unhealthy.

There can only be one `HEALTHCHECK` instruction in a `Dockerfile`. If you list more than one then only the last `HEALTHCHECK` will take effect.

The command after the `CMD` keyword can be either a shell command (e.g. `HEALTHCHECK CMD /bin/check-running`) or an exec array (as with other Dockerfile commands; see e.g. `ENTRYPOINT` for details).

The command’s exit status indicates the health status of the container. The possible values are:

- `0: success` - the container is healthy and ready for use
- `1: unhealthy` - the container is not working correctly
- `2: starting` - the container is not ready for use yet, but is working correctly

If the probe returns 2 (“starting”) when the container has already moved out of the “starting” state then it is treated as “unhealthy” instead.

For example, to check every five minutes or so that a web-server is able to serve the site’s main page within three seconds:

    HEALTHCHECK --interval=5m --timeout=3s \
      CMD curl -f http://localhost/ || exit 1

To help debug failing probes, any output text (UTF-8 encoded) that the command writes on stdout or stderr will be stored in the health status and can be queried with `docker inspect`. Such output should be kept short (only the first 4096 bytes are stored currently).

When the health status of a container changes, a `health_status` event is generated with the new status.

The `HEALTHCHECK` feature was added in Docker 1.12.

## SHELL Instruction
    SHELL ["executable", "parameters"]

The `SHELL` instruction allows the default shell used for the shell form of commands to be overridden. The default shell on Linux is `["/bin/sh", "-c"]`, and on Windows is `["cmd", "/S", "/C"]`. The `SHELL` instruction must be written in JSON form in a Dockerfile.

The `SHELL` instruction is particularly useful on Windows where there are two commonly used and quite different native shells: cmd and powershell, as well as alternate shells available including sh.

The `SHELL` instruction can appear multiple times. Each `SHELL` instruction overrides all previous `SHELL` instructions, and affects all subsequent instructions. For example:

    FROM windowsservercore

    # Executed as cmd /S /C echo default
    RUN echo default

    # Executed as cmd /S /C powershell -command Write-Host default
    RUN powershell -command Write-Host default

    # Executed as powershell -command Write-Host hello
    SHELL ["powershell", "-command"]
    RUN Write-Host hello

    # Executed as cmd /S /C echo hello
    SHELL ["cmd", "/S"", "/C"]
    RUN echo hello

The following instructions can be affected by the `SHELL` instruction when the shell form of them is used in a Dockerfile: `RUN`, `CMD` and `ENTRYPOINT`.

The following example is a common pattern found on Windows which can be streamlined by using the `SHELL` instruction:

    ...
    RUN powershell -command Execute-MyCmdlet -param1 "c:\foo.txt"
    ...

The command invoked by docker will be:

    cmd /S /C powershell -command Execute-MyCmdlet -param1 "c:\foo.txt"

This is inefficient for two reasons. First, there is an un-necessary cmd.exe command processor (aka shell) being invoked. Second, each `RUN` instruction in the shell form requires an extra powershell -command prefixing the command.

To make this more efficient, one of two mechanisms can be employed. One is to use the JSON form of the `RUN` command such as:

    ...
    RUN ["powershell", "-command", "Execute-MyCmdlet", "-param1 \"c:\\foo.txt\""]
    ...

While the JSON form is unambiguous and does not use the un-necessary cmd.exe, it does require more verbosity through double-quoting and escaping. The alternate mechanism is to use the `SHELL` instruction and the shell form, making a more natural syntax for Windows users, especially when combined with the escape parser directive:

    # escape=`

    FROM windowsservercore
    SHELL ["powershell","-command"]
    RUN New-Item -ItemType Directory C:\Example
    ADD Execute-MyCmdlet.ps1 c:\example\
    RUN c:\example\Execute-MyCmdlet -sample 'hello world'

Resulting in:

    PS E:\docker\build\shell> docker build -t shell .
    Sending build context to Docker daemon 3.584 kB
    Step 1 : FROM windowsservercore
     ---> 5bc36a335344
    Step 2 : SHELL powershell -command
     ---> Running in 87d7a64c9751
     ---> 4327358436c1
    Removing intermediate container 87d7a64c9751
    Step 3 : RUN New-Item -ItemType Directory C:\Example
     ---> Running in 3e6ba16b8df9


    Directory: C:\


    Mode                LastWriteTime         Length Name
    ----                -------------         ------ ----
    d-----         6/2/2016   2:59 PM                Example


     ---> 1f1dfdcec085
    Removing intermediate container 3e6ba16b8df9
    Step 4 : ADD Execute-MyCmdlet.ps1 c:\example\
     ---> 6770b4c17f29
    Removing intermediate container b139e34291dc
    Step 5 : RUN c:\example\Execute-MyCmdlet -sample 'hello world'
     ---> Running in abdcf50dfd1f
    Hello from Execute-MyCmdlet.ps1 - passed hello world
     ---> ba0e25255fda
    Removing intermediate container abdcf50dfd1f
    Successfully built ba0e25255fda
    PS E:\docker\build\shell>
   
The `SHELL` instruction could also be used to modify the way in which a shell operates. For example, using `SHELL cmd /S /C /V:ON|OFF` on Windows, delayed environment variable expansion semantics could be modified.

The `SHELL` instruction can also be used on Linux should an alternate shell be required such zsh, csh, tcsh and others.

The `SHELL` feature was added in Docker 1.12.

## Copying files
To copy files from the build context in a Docker image, use the ``COPY`` instruction:

    COPY localfile.txt containerfile.txt

If the filename contains spaces, use the alternate syntax:

    COPY ["local file", "container file"]

The ``COPY`` command supports wildcards. It can be used for example to copy all images to the ``images/`` directory:

    COPY *.jpg images/

Note: in this example, ``images/`` may not exist. In this case, Docker will create it automatically.

## Exposing a port
To declare exposed ports from a Dockerfile use the ``EXPOSE`` instruction:

    EXPOSE 8080 8082

Exposed ports setting can be overridden from the Docker commandline but it is a good practice to explicitly set them in the Dockerfile as it helps understand what an application does.

## Installing Debian/Ubuntu packages
Run the install on a single run command to merge the update and install. If you add more packages later, this will run the update again and install all the packages needed. If the update is run separately, it will be cached and package installs may fail. Setting the frontend to noninteractive and passing the -y to install is needed for scripted installs. Cleaning and purging at the end of the install minimizes the size of the layer.

    FROM debian
    
    RUN apt-get update \
     && DEBIAN_FRONTEND=noninteractive apt-get install -y \
        git \
        openssh-client \
        sudo \
        vim \
        wget \
     && apt-get clean \
     && rm -rf /var/lib/apt/lists/*



