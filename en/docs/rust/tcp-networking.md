---
title: "TCP Networking"
slug: "tcp-networking"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## A simple TCP client and server application: echo
The following code is based on the examples provided by the documentation on [std::net::TcpListener](https://doc.rust-lang.org/std/net/struct.TcpListener.html). This server application will listen to incoming requests and send back all incoming data, thus acting as an "echo" server. The client application will send a small message and expect a reply with the same contents.

**server:**

    use std::thread;
    use std::net::{TcpListener, TcpStream, Shutdown};
    use std::io::{Read, Write};
    
    fn handle_client(mut stream: TcpStream) {
        let mut data = [0 as u8; 50]; // using 50 byte buffer
        while match stream.read(&mut data) {
            Ok(size) => {
                // echo everything!
                stream.write(&data[0..size]).unwrap();
                true
            },
            Err(_) => {
                println!("An error occurred, terminating connection with {}", stream.peer_addr().unwrap());
                stream.shutdown(Shutdown::Both).unwrap();
                false
            }
        } {}
    }
    
    fn main() {
        let listener = TcpListener::bind("0.0.0.0:3333").unwrap();
        // accept connections and process them, spawning a new thread for each one
        println!("Server listening on port 3333");
        for stream in listener.incoming() {
            match stream {
                Ok(stream) => {
                    println!("New connection: {}", stream.peer_addr().unwrap());
                    thread::spawn(move|| {
                        // connection succeeded
                        handle_client(stream)
                    });
                }
                Err(e) => {
                    println!("Error: {}", e);
                    /* connection failed */
                }
            }
        }
        // close the socket server
        drop(listener);
    }

**client:**

    use std::net::{TcpStream};
    use std::io::{Read, Write};
    use std::str::from_utf8;
    
    fn main() {
        match TcpStream::connect("localhost:3333") {
            Ok(mut stream) => {
                println!("Successfully connected to server in port 3333");
    
                let msg = b"Hello!";
    
                stream.write(msg).unwrap();
                println!("Sent Hello, awaiting reply...");
    
                let mut data = [0 as u8; 6]; // using 6 byte buffer
                match stream.read_exact(&mut data) {
                    Ok(_) => {
                        if &data == msg {
                            println!("Reply is ok!");
                        } else {
                            let text = from_utf8(&data).unwrap();
                            println!("Unexpected reply: {}", text);
                        }
                    },
                    Err(e) => {
                        println!("Failed to receive data: {}", e);
                    }
                }
            },
            Err(e) => {
                println!("Failed to connect: {}", e);
            }
        }
        println!("Terminated.");
    }



