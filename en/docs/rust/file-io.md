---
title: "File IO"
slug: "file-io"
draft: false
images: []
weight: 9899
type: docs
toc: true
---

## Read a file as a whole as a String
    use std::fs::File;
    use std::io::Read;

    fn main() {
        let filename = "src/main.rs";
        // Open the file in read-only mode.
        match File::open(filename) {
            // The file is open (no error).
            Ok(mut file) => {
                let mut content = String::new();

                // Read all the file content into a variable (ignoring the result of the operation).
                file.read_to_string(&mut content).unwrap();

                println!("{}", content);

                // The file is automatically closed when is goes out of scope.
            },
            // Error handling.
            Err(error) => {
                println!("Error opening file {}: {}", filename, error);
            },
        }
    }

## Read a file line by line
    use std::fs::File;
    use std::io::{BufRead, BufReader};

    fn main() {
        let filename = "src/main.rs";
        // Open the file in read-only mode (ignoring errors).
        let file = File::open(filename).unwrap();
        let reader = BufReader::new(file);

        // Read the file line by line using the lines() iterator from std::io::BufRead.
        for (index, line) in reader.lines().enumerate() {
            let line = line.unwrap(); // Ignore errors.
            // Show the line and its number.
            println!("{}. {}", index + 1, line);
        }
    }

## Write in a file
    use std::env;
    use std::fs::File;
    use std::io::Write;

    fn main() {
        // Create a temporary file.
        let temp_directory = env::temp_dir();
        let temp_file = temp_directory.join("file");

        // Open a file in write-only (ignoring errors).
        // This creates the file if it does not exist (and empty the file if it exists).
        let mut file = File::create(temp_file).unwrap();

        // Write a &str in the file (ignoring the result).
        writeln!(&mut file, "Hello World!").unwrap();

        // Write a byte string.
        file.write(b"Bytes\n").unwrap();
    }

## Read a file as a Vec
    use std::fs::File;
    use std::io::Read;

    fn read_a_file() -> std::io::Result<Vec<u8>> {
        let mut file = try!(File::open("example.data"));

        let mut data = Vec::new();
        try!(file.read_to_end(&mut data));

        return Ok(data);
    }

`std::io::Result<T>` is an alias for `Result<T, std::io::Error>`.

The `try!()` macro returns from the function on error.

`read_to_end()` is a method of `std::io::Read` trait, which has to be explicitly `use`d.

`read_to_end()` does not return data it read. Instead it puts data into the container it's given.


