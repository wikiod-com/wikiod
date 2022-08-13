---
title: "Debugging ssh problems"
slug: "debugging-ssh-problems"
draft: false
images: []
weight: 9915
type: docs
toc: true
---

## Connection Refused
A "Connection Refused" error will occur if your client sends a connection request to a remote server host, and the remote host responds to say that it refuses to accept the request. The "Connection Refused" error essentially means that the computer is not accepting connections to the requested IP address and port.

"Connection refused" can be caused by a firewall which is blocking connection requests. A firewall which is configured to block connections to a particular endpoint can be set to drop connection requests--in which case the client will never get a response and will eventually timeout. Or the firewall can respond to connection connection requests with a refusal response.

Aside from firewalls, in the case of SSH, "connection refused" has a few possible causes:

 - You could be using the wrong port number to connect.  The standard port number for SSH is 22, but some people run the ssh service on a different port to deter unauthorized access attempts.

 - You could be trying to connect to the wrong computer. You may have mistyped the hostname or IP address. Or the computer may be using a dynamically-assigned address which has changed.

 - The ssh server process may not be running:
   - It may not have been started yet if the system is in the process of starting.
   - It may have been disabled; e.g. when the system is in single-user mode.
   - It may have been misconfigured, causing it to fail to start.
   - The computer may not have an SSH server set up. MS Windows systems typically don't include an SSH server. On some Linux systems, the SSH server may be an optional component. OS X includes an SSH server, but it's disabled by default.

 - The SSH server process may not be listening for connections on the specific IP interface which you're trying to connect to. Most computers have at least two IP interfaces, a "localhost" interface and one or more network interfaces. Each active interface will have an IP address associated with it. An SSH server is typically configured to accept connections on any IP interface. But it can be configured to accept connections only on particular interfaces. In that case, the computer will refuse connections to an IP address which the SSH server isn't listening to, even if the connection request has the correct port.

 - The server may have a backlog of connection requests to the same port. This is rare and unusual, but if the host is receiving connection requests faster than they can be handled, the host will eventually start rejecting new connection requests.

Note that, firewalls aside, "connection refused" means that you _are_ communicating with the remote computer--it's just not accepting your connection request.



## Private key is not accepted (OpenSSH clients debug)
By default, most of the information is hidden from the user. You can use `-v` switches to get a verbose log of the connection attempt, which will usually pinpoint the problem by showing why the behavior is different than you expect.

Let's assume you are connecting to the server `example.com` using `ssh` (or other OpenSSH client like `sftp` or `scp`) and your private key is not accepted by the server and the server asks for the password (or rejects the connection):

    $ ssh example.com
    user@example.com's password: 

Try to run the `ssh` with `-vvv` switches, which will write out all the debug messages. It will be a lot of information, but after some time, it is quite easy to understand that:

    $ ssh -vvv example.com

The most common problem is that the key is not in the expected location. You can expect similar lines to show up, complaining about missing files. Checking that your file was really read is a good start:

    debug1: identity file /home/username/.ssh/id_dsa type -1
    debug1: key_load_public: No such file or directory
    debug1: identity file /home/username/.ssh/id_dsa-cert type -1

We can continue to the authentication part. The key might be offered, but rejected by the server, because of problem with server configuration. The log might look somehow like this:

    debug3: preferred publickey,keyboard-interactive,password
    debug3: authmethod_lookup publickey
    debug3: remaining preferred: keyboard-interactive,password
    debug3: authmethod_is_enabled publickey
    debug1: Next authentication method: publickey
    debug1: Offering RSA public key: username@localhost
    debug3: send_pubkey_test
    debug3: send packet: type 50
    debug2: we sent a publickey packet, wait for reply
    debug3: receive packet: type 51
    debug1: Authentications that can continue: publickey,gssapi-keyex,gssapi-with-mic,password



## REMOTE HOST IDENTIFICATION HAS CHANGED!
The common error using `ssh` is to see the error like

    @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    @    WARNING: REMOTE HOST IDENTIFICATION HAS CHANGED!     @
    @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    IT IS POSSIBLE THAT SOMEONE IS DOING SOMETHING NASTY!
    Someone could be eavesdropping on you right now (man-in-the-middle attack)!
    It is also possible that a host key has just been changed.
    The fingerprint for the RSA key sent by the remote host is
    SHA256:L5ri/Xdgpuals893ej1z5F1wlg1n2YNeBf/tsABX+QQ.
    Please contact your system administrator.
    Add correct host key in /Users/username/.ssh/known_hosts to get rid of this message.
    Offending RSA key in /Users/username/.ssh/known_hosts:12
    RSA host key for *IP address* has changed and you have requested strict checking.
    Host key verification failed.

This means that you connected to the same server before and it was identified using different host keys. If you are aware you changed the server keys, reinstalled the server or the server administrator announced some changes, it is usually ok to remove the old key and let the `ssh` to store its new.

The old key can be transparently removed using `ssh-keygen`:

    ssh-keygen -R *IP address*

And next connection should ask you to verify the new fingerprint:

    ssh192.168.0.128
    The authenticity of host '192.168.0.128 (192.168.0.128)' can't be established.
    ECDSA key fingerprint is SHA256:L5ri/Xdgpuals893ej1z5F1wlg1n2YNeBf/tsABX+QQ.
    Are you sure you want to continue connecting (yes/no)? 

If you are not aware of any of the above, the best is to contact your server administrator to make sure that everything is ok. If not, the potential attacker would be able to get both your authentication information and all transferred data!

## ssh_exchange_identification: read: Connection reset by peer
This error message may be produced by the OpenSSH `ssh` client. It means that the TCP connection between the client and the server was abnormally closed by the server immediately after being accepted. Common reasons for this message include:

* The SSH server process is malfunctioning--for example, it crashed.
* A firewall, router, or other network device between the client and the server is interfering with the SSH connection.

The phrases in the error message indicate specifically what has happened:

**ssh_exchange_identification:** 1. After an SSH client makes a connection to an SSH server, the first step in starting the SSH protocol is for the server to send its software version to the client. An error containing "ssh_exchange_identification" indicates that the error occurred immediatly after making the TCP connection, while the client was waiting for the software version from the server.

**Connection reset:** A connection reset means the TCP connection was "abnormally closed". You can get this if the software process holding one of the TCP connection crashes. Network firewalls can be configured to use connection resets as a means to block TCP connections.

**by peer** means that the TCP connection was closed from the "other end" of the connection. In this case, the "other end" is the remote SSH server.

Note that this error doesn't indicate any kind of authentication failure. The server closed the TCP connection _immediately_ after accepting it. The client and server have not yet exchanged any data at all. The server has not yet sent a host key to the client, and the client has not yet tried to authenticate with the server.

## ssh_exchange_identification: Connection closed by remote host
This error message may be produced by the OpenSSH ssh client. It means that the TCP connection between the client and the server was closed by the server immediately after being accepted. This message generally indicates the SSH server has been configured not to accept connections from the client for some reason:

* The server may be configured to refuse connections from the client's IP address.
* The server may be configured with a limit on the number of active SSH connections.
* The client may have connected to something that's not an SSH server.

The phrases in the error message indicate specifically what has happened:

**ssh_exchange_identification:** 1. After an SSH client makes a connection to an SSH server, the first step in starting the SSH protocol is for the server to send its software version to the client. An error containing "ssh_exchange_identification" indicates that the error occurred immediately after making the TCP connection, while the client was waiting for the software version from the server.

**Connection closed:** This means the TCP connection was closed in a normal fashion. It indicates that the SSH server deliberately closed the connection. This is in contrast to "Connection reset", which could indicate that the SSH server crashed or malfunctioned.

**by remote host** means that the TCP connection was closed from the "other end" of the connection. In this case, the "other end" is the remote SSH server.

Note that this error doesn't indicate any kind of authentication failure. The server closed the TCP connection _immediately_ after accepting it. The client and server have not yet exchanged any data at all. The server has not yet sent a host key to the client, and the client has not yet tried to authenticate with the server.

## Connection timed out
A "Connection timed out" error occurs when the remote system does not respond to the client's attempt to open a TCP/IP connection.  The most common causes include:

 - A firewall is blocking the connection attempt on the port that you are using:
   - The firewall could be on the client-side, blocking outbound connections, on the server-side, blocking inbound connections, or somewhere else on the path.
   - The root problem *could* be that you are using the wrong port number.
 - The host you are attempting to connect to could be offline.
 - Some part of the network between the client and server could be down or disrupted.
 - There could be a network routing problem.

Diagnosing the root cause of connection timeouts is difficult.

