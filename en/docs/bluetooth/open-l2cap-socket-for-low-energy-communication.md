---
title: "Open L2CAP socket for Low Energy communication"
slug: "open-l2cap-socket-for-low-energy-communication"
draft: false
images: []
weight: 9888
type: docs
toc: true
---

## In C, with Bluez
    int get_l2cap_connection () {
First off, all the variables we need, explanation for will follow at the appropriate spot.

        int ssock = 0;
        int csock = 0;
        int reuse_addr = 1;
        struct sockaddr_l2 src_addr;
        struct bt_security bt_sec;
        int result = 0;

First, we need to create a socket, that we can accept a connection from. The socket family is `PF_BLUETOOTH`, socket type is `SOCK_SEQPACKET` (we want to have a TCP-like socket, not raw), and the protocol is the Bluetooth protocol L2CAP (`BTPROTO_L2CAP`).

        ssock = socket(PF_BLUETOOTH, SOCK_SEQPACKET, BTPROTO_L2CAP);

We want to make sure the it was succesful:

        if (ssock < 0) {
            perror("Opening L2CAP socket failed");
            return -1;
        }
    
We now have to fill the source address structure with a wildcard address, so any Bluetooth device with any address can connect to us. The wildcard address is defined as `BDADDR_ANY` in `bluetooth.h`. To copy it into the address structure, we can use the `bacpy` function. We also have to set the address family, address type and channel ID.

        memset(&src_addr, 0, sizeof(src_addr));
        bacpy(&src_addr.l2_bdaddr, BDADDR_ANY);
        src_addr.l2_family = AF_BLUETOOTH;
        src_addr.l2_bdaddr_type = BDADDR_LE_PUBLIC;
        src_addr.l2_cid = htobs(CID_ATT);
Setting the SO_REUSEADDR option will allow us to quickly call bind again if necessary (this can be left out):
    
        setsockopt(ssock, SOL_SOCKET, SO_REUSEADDR, &reuse_addr, sizeof(reuse_addr));

Next we have to bind the socket with the source address structure we just defined. Again, we check the return value to make sure it worked.
    
        result = bind(ssock, (struct sockaddr*) &src_addr, sizeof(src_addr));
        if (result < 0) {
            perror("Binding L2CAP socket failed");
            return -1;
        }
Next up is setting the security level. Note that this step is optional, but setting the security level to MEDIUM will allow automatic pairing with the device (the kernel handles the actual pairing).

    
        memset(&bt_sec, 0, sizeof(bt_sec));
        bt_sec.level = BT_SECURITY_MEDIUM;
        result = setsockopt(ssock, SOL_BLUETOOTH, BT_SECURITY, &bt_sec, sizeof(bt_sec));
        if (result != 0) {
            perrorno("Setting L2CAP security level failed");
            return -1;
        }
    
Now we can tell the kernel that our ssock is a passive socket, that will accept a connection. The second parameter is the backlog. If you want to know more, the manpage of listen contains all the information you need.

        result = listen(ssock, 10);
        if (result < 0) {
            perror("Listening on L2CAP socket failed");
            return -1;
        }
Now we can wait for an incoming connection. The peer_addr structure will contain the address of the connected device, once accept returns. csock will be the file descriptor of the socket we can read from/write to, to communicate with the connected device.

        memset(peer_addr, 0, sizeof(*peer_addr));
        socklen_t addrlen = sizeof(*peer_addr);
        csock = accept(ssock, (struct sockaddr*)peer_addr, &addrlen);
        if (csock < 0) {
            perror("Accepting connection on L2CAP socket failed");
            return -1;
        }
We can print the address of the connected device (optional, of course). We can use the batostr function to convert the Bluetooth address to a string.

        printf("Accepted connection from %s", batostr(&peer_addr->l2_bdaddr));

If we don't want any other devices to connect, we should close the server socket. Do the same thing with csock, after your communication with the device is finished.
    
        close(ssock);
        return csock;
    }

