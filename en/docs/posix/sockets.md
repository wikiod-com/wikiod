---
title: "Sockets"
slug: "sockets"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## TCP Concurrent Echo Server
In this example, we'll create a simple echo server that will listen on the specified port, and being able to handle new connections:

<!-- language: c -->
```
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <time.h>

/**
  Connection handler - this will be executed in
  the new process, after forking, and it will read
  all the data from the socket, while available and
  to echo it on the local terminal.

  Params:
    sd = socket to the client
*/
#define BUF_SIZE (1024)

int echo_client(int sd) 
{
    int result = 0; 

    char buf[BUF_SIZE + 1] = {0};

    ssize_t n_read;
    while (0 < (n_read = read(sd, buf, BUF_SIZE))) 
    {
        buf[n_read] = '\0';
        printf("%s\n", buf);
    }

    if (0 > n_read)
    {
      perror("read() failed");
      result = -1;
    }
    else
    {
      fprintf(stderr, "The other side orderly shut down the connection.\n");
    }

    close(sd);

    return result;
}


int main(void)
{
    // Create a listening socket
    int listening_socket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (listening_socket == -1)
    {
        perror("socket() failed");
        return EXIT_FAILURE;
    }

    // Bind it to port 15000.
    unsigned short listening_port = 15000;

    struct sockaddr_in addr = {0};
    addr.sin_family = AF_INET;
    addr.sin_port = htons(listening_port);

    socklen_t sock_len = sizeof(addr);

    if (0 > (bind(listening_socket, (const struct sockaddr*) &addr, sock_len)))
    {
        perror("bind() failed");
        return EXIT_FAILURE;
    }

    // Start listening
    if (0 > listen(listening_socket, 0))
    {
        perror("listen() failed");
        return EXIT_FAILURE;
    }

    // Accept new connections, fork the new process for handling
    // and handle the connection in the new process, while the parent
    // is waiting for another connection to arrive.
    int accepted_socket = 0;
    while (0 < (accepted_socket =
                  accept(listening_socket, (struct sockaddr*) &addr, &sock_len)))
    {
        pid_t pid_child = fork();

        if (0 > pid_child)
        {
            perror("fork() failed");
            return EXIT_FAILURE;
        }
        else if (0 == pid_child)
        {
            // inside the forked child here
            close(listening_socket); // The child does not need this any more. 

            echo_client(accepted_socket);

            return EXIT_SUCCESS;
        }

        // Inside parent process, since file descriptors are reference
        // counted, we need to close the client socket
        close(accepted_socket);
    }

    if (0 > accepted_socket)
    {
        perror("accept() failed");
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
```

## TCP Daytime Iterative Server
This is a TCP daytime iterative server kept as simple as possible.

<!-- language: c -->
```
#include <sys/types.h>   /* predefined types */ 
#include <unistd.h>      /* unix standard library */ 
#include <arpa/inet.h>   /* IP addresses conversion utilities */ 
#include <netinet/in.h>  /* sockaddr_in structure definition */
#include <sys/socket.h>  /* berkley socket library */ 
#include <stdio.h>       /* standard I/O library */ 
#include <string.h>      /* include to have memset */ 
#include <stdlib.h>      /* include to have exit */ 
#include <time.h>        /* time manipulation primitives */
 
#define MAXLINE 80 
#define BACKLOG 10 
 
int main(int argc, char *argv[]) 
{  
    int list_fd, conn_fd;  
    struct sockaddr_in serv_add; 
    char buffer[MAXLINE]; 
    time_t timeval; 
 
    /* socket creation third parameter should be IPPROTO_TCP but 0 is an
     * accepted value */  
    list_fd = socket(AF_INET, SOCK_STREAM, 0);
         
    /* address initialization */ 
    memset(&serv_add, 0, sizeof(serv_add));        /* init the server address */ 
    serv_add.sin_family = AF_INET;                 /* address type is IPV4 */ 
    serv_add.sin_port = htons(13);                 /* daytime port is 13 */ 
    serv_add.sin_addr.s_addr = htonl(INADDR_ANY);  /* connect from anywhere */ 
     
    /* bind socket */ 
    bind(list_fd, (struct sockaddr *)&serv_add, sizeof(serv_add));
        
    /* listen on socket */ 
    listen(list_fd, BACKLOG);

    while (1) 
    { 
        /* accept connection */ 
        conn_fd = accept(list_fd, (struct sockaddr *) NULL, NULL);
           
        timeval = time(NULL); 
        snprintf(buffer, sizeof(buffer), "%.24s\r\n", ctime(&timeval)); 
         
        write(conn_fd, buffer, strlen(buffer)); /* write daytime to client */ 
         
        close(conn_fd); 
    } 
     
    /* normal exit */ 
    close(list_fd); 
    exit(0); 
}
```

## TCP Daytime Client
This is a TCP daytime client kept as simple as possible.
     
<!-- language: c -->

    #include <unistd.h>      /* unix standard library */
    #include <arpa/inet.h>   /* IP addresses manipulation utilities */
    #include <netinet/in.h>  /* sockaddr_in structure definition */
    #include <sys/socket.h>  /* berkley socket library */
    #include <stdio.h>       /* standard I/O library */
    #include <string.h>      /* include to have memset*/
    #include <stdlib.h>      /* include to have exit*/

    #define MAXLINE 1024

    int main(int argc, char *argv[])
    {
        int sock_fd;
        int nread;
        struct sockaddr_in serv_add;
        char buffer[MAXLINE];

        /* socket creation third parameter should be IPPROTO_TCP but 0 is an
         * accepted value*/
        sock_fd = socket(AF_INET, SOCK_STREAM, 0);

        /* address initialization */
        memset(&serv_add, 0, sizeof(serv_add));     /* init the server address */
        serv_add.sin_family = AF_INET;              /* address type is IPV4 */
        serv_add.sin_port = htons(13);              /* daytime post is 13 */

        /* using inet_pton to build address */
        inet_pton(AF_INET, argv[1], &serv_add.sin_addr);

        /* connect to the server */
        connect(sock_fd, (struct sockaddr *)&serv_add, sizeof(serv_add));

        /* read daytime from server */
        while ((nread = read(sock_fd, buffer, MAXLINE)) > 0)
        {
            buffer[nread] = 0;
            if (fputs(buffer, stdout) == EOF)
            {
                perror("fputs error"); /* write daytime on stdout */
                return -1;
            }
        }

        close(sock_fd);
        exit(0);
    }

## Enabling TCP keepalive at server side
This is a client-server example. The process forks and runs client in the parent process and server in the child process:

* client connects to server and waits until server exits;
* server accepts connection from client, enables keepalive, and waits any signal.

Keepalive is configured using the following options described in [`socket(7)`][1] and [`tcp(7)`][2] man pages:

* `SO_KEEPALIVE` - enables sending of keep-alive messages
* `TCP_KEEPIDLE`- the time (in seconds) the connection needs to remain idle before TCP starts sending keepalive probes
* `TCP_KEEPINTVL` - the time (in seconds) between individual keepalive probes
* `TCP_KEEPCNT` - the maximum number of keepalive probes TCP should send before dropping the connection

Source code:

<!-- language: c -->
```
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/signal.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>

#define check(expr) if (!(expr)) { perror(#expr); kill(0, SIGTERM); }

void enable_keepalive(int sock) {
    int yes = 1;
    check(setsockopt(
        sock, SOL_SOCKET, SO_KEEPALIVE, &yes, sizeof(int)) != -1);

    int idle = 1;
    check(setsockopt(
        sock, IPPROTO_TCP, TCP_KEEPIDLE, &idle, sizeof(int)) != -1);

    int interval = 1;
    check(setsockopt(
        sock, IPPROTO_TCP, TCP_KEEPINTVL, &interval, sizeof(int)) != -1);

    int maxpkt = 10;
    check(setsockopt(
        sock, IPPROTO_TCP, TCP_KEEPCNT, &maxpkt, sizeof(int)) != -1);
}

int main(int argc, char** argv) {
    check(argc == 2);

    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(12345);
    check(inet_pton(AF_INET, argv[1], &addr.sin_addr) != -1);

    int server = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    check(server != -1);

    int yes = 1;
    check(setsockopt(server, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) != -1);

    check(bind(server, (struct sockaddr*)&addr, sizeof(addr)) != -1);
    check(listen(server, 1) != -1);

    if (fork() == 0) {
        int client = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        check(client != -1);
        check(connect(client, (struct sockaddr*)&addr, sizeof(addr)) != -1);
        printf("connected\n");
        pause();
    }
    else {
        int client = accept(server, NULL, NULL);
        check(client != -1);
        enable_keepalive(client);
        printf("accepted\n");
        wait(NULL);
    }

    return 0;
}
```

Keepalive packets may be monitored using `tcpdump`.

Example usage:

```
$ ./a.out 127.0.0.1 &
[1] 14010
connected
accepted

$ tcpdump -n -c4 -ilo port 12345
dropped privs to tcpdump
tcpdump: verbose output suppressed, use -v or -vv for full protocol decode
listening on lo, link-type EN10MB (Ethernet), capture size 262144 bytes
18:00:35.173892 IP 127.0.0.1.12345 > 127.0.0.1.60998: Flags [.], ack 510307430, win 342, options [nop,nop,TS val 389745775 ecr 389745675], length 0
18:00:35.173903 IP 127.0.0.1.60998 > 127.0.0.1.12345: Flags [.], ack 1, win 342, options [nop,nop,TS val 389745775 ecr 389745075], length 0
18:00:36.173886 IP 127.0.0.1.12345 > 127.0.0.1.60998: Flags [.], ack 1, win 342, options [nop,nop,TS val 389745875 ecr 389745775], length 0
18:00:36.173898 IP 127.0.0.1.60998 > 127.0.0.1.12345: Flags [.], ack 1, win 342, options [nop,nop,TS val 389745875 ecr 389745075], length 0
4 packets captured
8 packets received by filter
0 packets dropped by kernel
```

  [1]: http://man7.org/linux/man-pages/man7/socket.7.html
  [2]: http://man7.org/linux/man-pages/man7/tcp.7.html

## Socket basics
There are four types of sockets available in POSIX API: TCP, UDP, UNIX, and (optionally) RAW. Unix domain sockets may act like stream sockets or like datagram sockets.

Some of endpoint types:
1. `struct sockaddr` - universal endpoint type. Typically, other concrete endpoint types are converted to this type only in posix calls.
2. `struct sockaddr_in` - IPv4 endpoint
<!-- language: c -->
    struct sockaddr_in {
        sa_family_t sin_family;
        in_port_t sin_port;            /* Port number.  */
        struct in_addr sin_addr;       /* Internet address.  */
    };
    struct in_addr {
        in_addr_t s_addr;
    };

3. `struct sockaddr_in6` - IPv6 endpoint
<!-- language: c -->
    struct sockaddr_in6 {
        sa_family_t sin6_family;
        in_port_t sin6_port;    /* Transport layer port # */
        uint32_t sin6_flowinfo;    /* IPv6 flow information */
        struct in6_addr sin6_addr;    /* IPv6 address */
        uint32_t sin6_scope_id;    /* IPv6 scope-id */
    };
4. `struct sockaddr_un`.
<!-- language: c -->
    struct sockaddr_un {
        sa_family_t sun_family;               /* AF_UNIX */
        char        sun_path[108];            /* pathname */
    };

# Entire program

<!-- language: c -->
    #include <arpa/inet.h>
    #include <netinet/in.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <sys/socket.h>
    #include <unistd.h>
    
    #define DESIRED_ADDRESS "127.0.0.1"
    #define DESIRED_PORT 3500
    #define BUFSIZE 512
    
    int main()
    {
        // ADDRESS PART
        // MAIN PART
        close(sock);
        return EXIT_SUCCESS;
    }

# Creating IPv4 endpoint
<!-- language: c -->
    struct sockaddr_in addr = {0};
    addr.sin_family = AF_INET;
    addr.sin_port = htons(DESIRED_PORT); /*converts short to
                                           short with network byte order*/
    addr.sin_addr.s_addr = inet_addr(DESIRED_ADDRESS);

# TCP server snippet #
<!-- language: c -->
    int sock = socket (AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (sock == -1) {
        perror("Socket creation error");
        return EXIT_FAILURE;
    }

    if (bind(sock, (struct sockaddr*) &addr, sizeof(addr)) == -1) {
        perror("Bind error");
        close(sock);
        return EXIT_FAILURE;
    }
    
    if (listen(sock, 1/*length of connections queue*/) == -1) {
        perror("Listen error");
        close(sock);
        return EXIT_FAILURE;
    }
    
    socklen_t socklen = sizeof addr;
    int client_sock = accept(sock, &addr, &socklen); /* 2nd and 3rd argument may be NULL. */
    if (client_sock == -1) {
        perror("Accept error");
        close(sock);
        return EXIT_FAILURE;
    }

    printf("Client with IP %s connected\n", inet_ntoa(addr.sin_addr));
    
    char buf[BUFSIZE];
    if (send(sock, "hello", 5, 0) == -1) {
        perror("Send error");
        close(client_sock);
        close(sock);
        return EXIT_FAILURE;
    }

    ssize_t readden = recv(sock, buf, BUFSIZE, 0);
    if (readden < 0) {
        perror("Receive error");
        close(client_sock);
        close(sock);
        return EXIT_FAILURE;
    }
    else if (readden == 0) {
      fprintf(stderr, "Client orderly shut down the connection.\n");
    }
    else {readden > 0) {
        if (readden < BUFSIZE)
        {
          fprintf(stderr, "Received less bytes (%zd) then requested (%d).\n", 
            readden, BUFSIZE);
        }
    
        write (STDOUT_FILENO, buf, readden);
    }

# TCP client snippet #

<!-- language: c -->
    int sock = socket (AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (sock == -1) {
        perror("Socket creation error");
        return EXIT_FAILURE;
    }
    if (connect(sock, (struct sockaddr*) &addr, sizeof(addr)) == -1) {
        perror("Connection error");
        close(sock);
        return EXIT_FAILURE;
    }
    
    char buf[BUFSIZE];
    if (send(sock, "hello", 5, 0); /*write may be also used*/ == -1) {
        perror("Send error");
        close(client_sock);
        close(sock);
        return EXIT_FAILURE;
    }

    ssize_t readden = recv(sock, buf, BUFSIZE, 0); /*read may be also used*/
    if (readden < 0) {
        perror("Receive error");
        close(client_sock);
        close(sock);
        return EXIT_FAILURE;
    }
    else if (readden == 0)
    {
        fprintf(stderr, "Client orderly shut down the connection.\n");
    }
    else /* if (readden > 0) */ {
        if (readden < BUFSIZE)
        {
          fprintf(stderr, "Received less bytes (%zd) then requested (%d).\n", 
            readden, BUFSIZE);
        }
    
        write (STDOUT_FILENO, buf, readden);
    }  

# UDP server snippet #
<!-- language: c -->
    int sock = socket (AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if (sock == -1) {
        perror("Socket creation error");
        return EXIT_FAILURE;
    }
    if (bind(sock, (struct sockaddr*) &addr, sizeof(addr)) == -1) {
        perror("Bind error");
        close(sock);
        return EXIT_FAILURE;
    }
    
    char buf[BUFSIZE];
    ssize_t readden = recvfrom(sock, buf, BUFSIZE, 0, &addr, sizeof(addr));
    if (readden > 0) {
        printf("Client with IP %s sent datagram\n", inet_ntoa(addr.sin_addr));
        write (STDOUT_FILENO, buf, readden);
    }
    sendto(sock, "hello", 5, 0, &addr, sizeof(addr));

## Accepting connections on a blocking socket
A C program that wishes to accept network connections (act as a "server") should first create a socket bound to the address "INADDR_ANY" and call `listen` on it. Then, it can call `accept` on the server socket to block until a client connects.
    
<!-- language: lang-c -->
    //Create the server socket
    int servsock = socket(AF_INET, SOCK_STREAM, 0);
    if(servsock < 0) perror("Failed to create a socket");

    int enable = 1;
    setsockopt(servsock, SOL_SOCKET, SO_REUSEADDR, (char*)&enable, sizeof(int));

    //Bind to "any" address with a specific port to listen on that port
    int port = 12345;
    sockaddr_in serv_addr;
    memset(&serv_addr, 0, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr.s_addr = INADDR_ANY;
    serv_addr.sin_port = htons(port);

    if(bind(servsock, (sockaddr*)&serv_addr, sizeof(serv_addr)) < 0)
        perror("Error binding to socket");

    listen(servsock, 5);
    
    //Accept a client
    struct sockaddr_storage client_addr_info;
    socklen_t len = sizeof client_addr_info;

    int clientsock = accept(servsock, (struct sockaddr*)&client_addr_info, &len);
    
    //Now you can call read, write, etc. on the client socket


The `sockaddr_storage` struct that  gets passed to `accept` can be used to retrieve information about the client that connected. For example, here's how to determine the client's IP address:

<!-- language: lang-c -->
    char client_ip_str[INET6_ADDRSTRLEN + 1];
    if(client_addr_info.ss_family == AF_INET) {
        // Client has an IPv4 address
        struct sockaddr_in *s = (struct sockaddr_in *)&client_addr_info;
        inet_ntop(AF_INET, &s->sin_addr, client_ip_str, sizeof(client_ip_str));
    } else {  // AF_INET6
        // Client has an IPv6 address
        struct sockaddr_in6 *s = (struct sockaddr_in6 *)&client_addr_info;
        inet_ntop(AF_INET6, &s->sin6_addr, client_ip_str, sizeof(client_ip_str));
    }

## Connecting to a remote host
<!-- if version [lt POSIX.1-2008] -->

Given the name of a server as a string, `char* servername`, and a port number, `int port`, the following code creates and opens a socket connected to that server. The "name" of the server can either be a DNS name, such as "www.stackoverflow.com," or an IP address in standard notation, such as "192.30.253.113"; either input format is valid for `gethostbyname` (which [had been removed from POSIX.1-2008][1]).

<!-- language: lang-c -->
    char * server = "www.example.com";

    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if(sock < 0) 
      perror("Failed to create a socket");

    hostent *server = gethostbyname(servername);
    if (server == NULL) 
      perror("Host lookup failed");

    char server_ip_str[server->h_length];
    inet_ntop(AF_INET, server->h_addr, server_ip_str, server->h_length);

    sockaddr_in serv_addr;
    memset(&serv_addr, 0, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(port);
    memcpy(&serv_addr.sin_addr.s_addr, server->h_addr, server->h_length);

    if (connect(sock, (sockaddr*)&serv_addr, sizeof(serv_addr)) < 0)
        perror("Failed to connect");
    
    // Now you can call read, write, etc. on the socket.

    close(sock);

<!-- end version if -->


  [1]: http://pubs.opengroup.org/onlinepubs/9699919799/xrat/V4_xsh_chap03.html

## Reading and writing on a blocking socket
Even when sockets are in "blocking" mode, the `read` and `write` operations on them do not necessarily read and write all the data available to be read or written. In order to write an entire buffer into a socket, or read a known quantity of data from a socket, they must be called in a loop.

<!-- language: lang-c -->
    /*
     * Writes all bytes from buffer into sock. Returns true on success, false on failure.
     */
    bool write_to_socket(int sock, const char* buffer, size_t size) {
        size_t total_bytes = 0;
        while(total_bytes < size) {
            ssize_t bytes_written = write(sock, buffer + total_bytes, size - total_bytes);
            if(bytes_written >= 0) {
                total_bytes += bytes_written;
            } else if(bytes_written == -1 && errno != EINTR) {
                return false;
            }
        }
        return true;
    }

<!-- language: lang-c -->
    /*
     * Reads size bytes from sock into buffer. Returns true on success; false if
     * the socket returns EOF before size bytes can be read, or if there is an
     * error while reading.
     */
    bool read_from_socket(int sock, char* buffer, size_t size) {
        size_t total_bytes = 0;
        while(total_bytes < size) {
            ssize_t new_bytes = read(sock, buffer + total_bytes, size - total_bytes);
            if(new_bytes > 0) {
                total_bytes += new_bytes;
            } else if(new_bytes == 0 || (new_bytes == -1 && errno != EINTR)) {
                return false;
            }
        }
        return true;
    }

