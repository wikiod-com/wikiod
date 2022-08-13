---
title: "Reverse tunnels"
slug: "reverse-tunnels"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## OpenSSH
Creating a reverse `ssh` tunnel takes just one switch `-R` to the original command.

# Command line
Let's assume you are connecting to the `example.com` as a user `guest` using a command `ssh guest@example.com`. Opening reverse tunnel can look like this:

    ssh -R 2222:localhost:22 guest@example.com

It will open a port `2222` on the remote server (loopback interface only) and every connection to this port will be forwarded to your local computer ssh server (port `22`).

This also assumes that you have allowed options `AllowTcpForwarding yes` and `PermitOpen any` in your `sshd_config` on your server. Otherwise it will fail with error

    open failed: administratively prohibited: open failed

If you want to allow the forwarded port to be accessible on other network addresses (than `localhost`), you need additionally to allow `GatewayPorts yes` and use a IP address or hostname or IP):

    ssh -R 2222:example.com:22 guest@example.com

# Configuration

Additionally, you can specify your remote port forwarding in your `~/.ssh/config` to avoid typing the same line every time you connect. Good practice might be to set up also alias to the host, which will have this forwarding, if you connect to your host frequently and don't want to initiate the port forwarding every time:

    Host example.com-R
      Hostname example.com
      User guest
      RemoteForward 2222 localhost:22

and then create remote port forwarding simply using `ssh example.com-R`

# Running in background

The port forwarding can be simply run in the background using switches `-N` (do not run the remote command, only the forwarding),  `-f` (go to background after authentication), `-T` (disable remote TTY allocation). Putting it all together:

    ssh -NTfR 2222:localhost:22 guest@example.com

