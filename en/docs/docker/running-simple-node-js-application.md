---
title: "Running Simple Node.js Application"
slug: "running-simple-nodejs-application"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Running a Basic Node.js application inside a Container
The example I'm going to discuss assumes you have a Docker installation that works in your system and a basic understanding of how to work with Node.js . If you are aware of how you must work with Docker , it should be evident that Node.js framework need not be installed on your system, rather we would be using the `latest` version of the `node` image available from Docker. Hence if needed you may download the image beforehand with the command `docker pull node`. (The command automatically `pulls` the latest version of the `node` image from docker.)

1) Proceed to make a directory where all your working application files would reside. Create a `package.json` file in this directory that describes your application as well as the dependencies. Your `package.json` file should look something like this:

        {
          "name": "docker_web_app",
          "version": "1.0.0",
          "description": "Node.js on Docker",
          "author": "First Last <first.last@example.com>",
          "main": "server.js",
          "scripts": {
            "start": "node server.js"
          },
          "dependencies": {
            "express": "^4.13.3"
          }
        }

2. If we need to work with Node.js we usually create a `server` file that defines a web application. In this case we use the `Express.js` framework (version `4.13.3` onwards). A basic `server.js` file would look something like this:

        var express = require('express');
        var PORT = 8080;
        var app = express();
        app.get('/', function (req, res) {
          res.send('Hello world\n');
        });
        
        app.listen(PORT);
        console.log('Running on http://localhost:' + PORT);
    
3. For those familiar with Docker, you would have come across a `Dockerfile`. A `Dockerfile` is a text file that contains all the commands required to build a custom image that is tailored for your application.

Create an empty text file named `Dockerfile` in the current directory. The method to create one is straightforward in Windows. In Linux, you may want to execute `touch Dockerfile` in the directory containing all the files required for your application.
Open the Dockerfile with any text editor and add the following lines:

    FROM node:latest
    RUN mkdir -p /usr/src/my_first_app
    WORKDIR /usr/src/my_first_app
    COPY package.json /usr/src/my_first_app/
    RUN npm install
    COPY . /usr/src/my_first_app
    EXPOSE 8080
 
 * `FROM node:latest` instructs the Docker daemon what image we want to build from. In this case we use the `latest` version of the official Docker image `node` available from the [Docker Hub][1]. 

* Inside this image we proceed to create a working directory that contains all the required files and we instruct the daemon to set this directory as the desired working directory for our application. For this we add

        RUN mkdir -p /usr/src/my_first_app
        WORKDIR /usr/src/my_first_app
* We then proceed to install application dependencies by first moving the `package.json` file (which specifies app info including dependencies) to the `/usr/src/my_first_app` working directory in the image. We do this by

        COPY package.json /usr/src/my_first_app/
        RUN npm install 
        
* We then type `COPY . /usr/src/my_first_app` to add all the application files and source code to the working directory in the image.

* We then use the `EXPOSE` directive to instruct the daemon to make port `8080` of the resulting container visible (via a container-to-host mapping) since the application binds to port `8080`.

* In the last step, we instruct the daemon to run the command `node server.js` inside the image by executing the basic `npm start` command. We use the `CMD` directive for this, which takes the commands as arguments.

        CMD [ "npm", "start" ] 

4. We then create a `.dockerignore` file in the same directory as the `Dockerfile`
to prevent our copy of `node_modules` and logs used by our Node.js system installation from being copied on to the Docker image. The `.dockerignore` file must have the following content:

        node_modules
        npm-debug.log

5. # Build your image

Navigate to the directory that contains the `Dockerfile` and run the following command to build the Docker image. The `-t` flag lets you tag your image so it's easier to find later using the docker images command: 

        $ docker build -t <your username>/node-web-app .

Your image will now be listed by Docker. View images using the below command:

    $ docker images
    
    REPOSITORY                      TAG        ID              CREATED
    node                            latest     539c0211cd76    10 minutes ago
    <your username>/node-web-app    latest     d64d3505b0d2    1 minute ago
    
6. # Running the image

We can now run the image we just created using the application contents, the `node` base image and the `Dockerfile`. We now proceed to run our newly created `<your username>/node-web-app` image. Providing `-d` switch to the `docker run` command runs the container in detached mode,so that the container runs in the background. The `-p` flag redirects a public port to a private port inside the container. Run the image you previously built using this command:

    $ docker run -p 49160:8080 -d <your username>/node-web-app
7. Print the output of your app by running `docker ps` on your terminal. The output should look something like this.

        CONTAINER ID        IMAGE                         COMMAND             CREATED             STATUS              PORTS                     NAMES
        7b701693b294      <your username>/node-web-app   "npm start"         20 minutes ago       Up 48 seconds       0.0.0.0:49160->8080/tcp   loving_goldstine

Get application output by entering `docker logs <CONTAINER ID>`. In this case it is  `docker logs 7b701693b294`.

Output: `Running on http://localhost:8080`

8. From the `docker ps` output, the port mapping obtained is `0.0.0.0:49160->8080/tcp`.  Hence Docker mapped the `8080` port inside of the container to the port 49160 on the host machine. In the browser we can now enter `localhost:49160`.

We can also call our app using `curl`:

    $ curl -i localhost:49160
    
    HTTP/1.1 200 OK
    X-Powered-By: Express
    Content-Type: text/html; charset=utf-8
    Content-Length: 12
    Date: Sun, 08 Jan 2017 14:00:12 GMT
    Connection: keep-alive
    
    Hello world      

 [1]: https://hub.docker.com/

