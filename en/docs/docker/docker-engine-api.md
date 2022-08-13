---
title: "Docker Engine API"
slug: "docker-engine-api"
draft: false
images: []
weight: 9436
type: docs
toc: true
---

An API that allows you to control every aspect of Docker from within your own applications, build tools to manage and monitor applications running on Docker, and even use it to build apps on Docker itself.

## Enable Remote access to Docker API on Linux running systemd
Linux running systemd, like Ubuntu 16.04, adding `-H tcp://0.0.0.0:2375` to `/etc/default/docker` does not have the effect it used to.

Instead, create a file called `/etc/systemd/system/docker-tcp.socket` to make docker available on a TCP socket on port 4243:

    [Unit]
    Description=Docker Socket for the API  
    [Socket]
    ListenStream=4243  
    Service=docker.service  
    [Install]
    WantedBy=sockets.target 

 
Then enable the new socket: 

    systemctl enable docker-tcp.socket
    systemctl enable docker.socket
    systemctl stop docker
    systemctl start docker-tcp.socket
    systemctl start docker

Now, verify if Remote API is working:

    curl -X GET http://localhost:4243/images/json

## Enable Remote Access with TLS on Systemd
Copy the package installer unit file to /etc where changes will not be overwritten on an upgrade:

    cp /lib/systemd/system/docker.service /etc/systemd/system/docker.service

Update /etc/systemd/system/docker.service with your options on ExecStart:

    ExecStart=/usr/bin/dockerd -H fd:// -H tcp://0.0.0.0:2376 \
      --tlsverify --tlscacert=/etc/docker/certs/ca.pem \
      --tlskey=/etc/docker/certs/key.pem \
      --tlscert=/etc/docker/certs/cert.pem

Note that `dockerd` is the 1.12 daemon name, prior it was `docker daemon`. Also note that 2376 is dockers standard TLS port, 2375 is the standard unencrypted port. See [this page](https://docs.docker.com/engine/security/https/) for steps to create your own TLS self signed CA, cert, and key.

After making changes to the systemd unit files, run the following to reload the systemd config:

    systemctl daemon-reload

And then run the following to restart docker:

    systemctl restart docker

It's a bad idea to skip TLS encryption when exposing the Docker port since anyone with network access to this port effectively has full root access on the host.


## Enable Remote access to Docker API on Linux
Edit `/etc/init/docker.conf` and update the `DOCKER_OPTS` variable to the following:

    DOCKER_OPTS='-H tcp://0.0.0.0:4243 -H unix:///var/run/docker.sock'

Restart Docker deamon

    service docker restart

Verify if Remote API is working

    curl -X GET http://localhost:4243/images/json

## Image pulling with progress bars, written in Go
Here is an example of image pulling using `Go` and `Docker Engine API` and the same progress bars as the ones shown when you run `docker pull your_image_name` in the `CLI`. For the purposes of the progress bars are used some [ANSI codes][1].
<!-- language: lang-go -->

    package yourpackage

    import (
        "context"
        "encoding/json"
        "fmt"
        "io"
        "strings"
    
        "github.com/docker/docker/api/types"
        "github.com/docker/docker/client"
    )

    // Struct representing events returned from image pulling
    type pullEvent struct {
        ID             string `json:"id"`
        Status         string `json:"status"`
        Error          string `json:"error,omitempty"`
        Progress       string `json:"progress,omitempty"`
        ProgressDetail struct {
            Current int `json:"current"`
            Total   int `json:"total"`
        } `json:"progressDetail"`
    }
    
    // Actual image pulling function
    func PullImage(dockerImageName string) bool {
        client, err := client.NewEnvClient()

        if err != nil {
            panic(err)
        }
    
        resp, err := client.ImagePull(context.Background(), dockerImageName, types.ImagePullOptions{})

        if err != nil {
            panic(err)
        }

        cursor := Cursor{}
        layers := make([]string, 0)
        oldIndex := len(layers)

        var event *pullEvent
        decoder := json.NewDecoder(resp)

        fmt.Printf("\n")
        cursor.hide()

        for {
            if err := decoder.Decode(&event); err != nil {
                if err == io.EOF {
                    break
                }
    
                panic(err)
            }

            imageID := event.ID

            // Check if the line is one of the final two ones
            if strings.HasPrefix(event.Status, "Digest:") || strings.HasPrefix(event.Status, "Status:") {
                fmt.Printf("%s\n", event.Status)
                continue
            }
    
            // Check if ID has already passed once
            index := 0
            for i, v := range layers {
                if v == imageID {
                    index = i + 1
                    break
                }
            }
    
            // Move the cursor
            if index > 0 {
                diff := index - oldIndex

                if diff > 1 {
                    down := diff - 1
                    cursor.moveDown(down)
                } else if diff < 1 {
                    up := diff*(-1) + 1
                    cursor.moveUp(up)
                }
    
                oldIndex = index
            } else {
                layers = append(layers, event.ID)
                diff := len(layers) - oldIndex
    
                if diff > 1 {
                    cursor.moveDown(diff) // Return to the last row
                }
    
                oldIndex = len(layers)
            }

            cursor.clearLine()

            if event.Status == "Pull complete" {
                fmt.Printf("%s: %s\n", event.ID, event.Status)
            } else {
                fmt.Printf("%s: %s %s\n", event.ID, event.Status, event.Progress)
            }
    
        }
    
        cursor.show()
    
        if strings.Contains(event.Status, fmt.Sprintf("Downloaded newer image for %s", dockerImageName)) {
            return true
        }
    
        return false
    }

For better readability, cursor actions with the ANSI codes are moved to a separate structure, which looks like this:

    package yourpackage

    import "fmt"
    
    // Cursor structure that implements some methods
    // for manipulating command line's cursor
    type Cursor struct{}
    
    func (cursor *Cursor) hide() {
        fmt.Printf("\033[?25l")
    }
    
    func (cursor *Cursor) show() {
        fmt.Printf("\033[?25h")
    }
    
    func (cursor *Cursor) moveUp(rows int) {
        fmt.Printf("\033[%dF", rows)
    }
    
    func (cursor *Cursor) moveDown(rows int) {
        fmt.Printf("\033[%dE", rows)
    }
    
    func (cursor *Cursor) clearLine() {
        fmt.Printf("\033[2K")
    }

After that in your main package you can call the `PullImage` function passing the image name you want to pull. Of course, before calling it, you have to be logged into the Docker registry, where the image is.


  [1]: https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_codes

## Making a cURL request with passing some complex structure
When using `cURL` for some queries to the `Docker API`, it might be a bit tricky to pass some complex structures. Let's say, [getting a list of images][1] allows using filters as a query parameter, which have to be a `JSON` representation of `map[string][]string` (about the maps in `Go` you can find more [here][2]).<br />
Here is how to achieve this:

    curl --unix-socket /var/run/docker.sock \
        -XGET "http:/v1.29/images/json" \
        -G \
        --data-urlencode 'filters={"reference":{"yourpreciousregistry.com/path/to/image": true}, "dangling":{"true": true}}'
Here the `-G` flag is used to specify that the data in the `--data-urlencode` parameter will be used in an `HTTP GET` request instead of the `POST` request that otherwise would be used. The data will be appended to the URL with a `?` separator.


  [1]: https://docs.docker.com/engine/api/v1.29/#operation/ImageList
  [2]: https://blog.golang.org/go-maps-in-action

