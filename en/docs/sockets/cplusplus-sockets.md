---
title: "C++ Sockets"
slug: "c++-sockets"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

This topic will be about modern C++ style Berkeley Socket Programming (This is code for Linux, but easily portable to other platforms)

## Sample server code
    constexpr const size_t addressSize = sizeof(sockaddr_in);
    constexpr const uint16_t defaultPort = 80; // The port you want to use

    int serverSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    sockaddr_in serverAddress, clientAddress;

    memset(&serverAddress, 0, addressSize);
    serverAddress.sin_family = AF_INET;
    serverAddress.sin_addr.s_addr = htonl(INADDR_ANY);
    serverAddress.sin_port = htons(defaultPort);

    bind(serverSocket, (sockaddr*)&serverAddress, addressSize);
    listen(serverSocket, SOMAXCONN);

    while (true) { // Infinite running app
        std::thread{ // Create new thread for every client
            handleConnection, //Connection handler
            accept(serverSocket, (sockaddr*)&clientAddress, &addressSize) //Client socket
            // Any other parameters for the handler here
        }.detach(); // Detached thread to make resource management easier
    }
    return 0;

