---
title: "Error handling"
slug: "error-handling"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

Rust uses `Result<T, E>` values to indicate recoverable errors during execution. Unrecoverable errors cause [Panics](https://www.wikiod.com/rust/panics-and-unwinds) which is a topic of its own.

Details of error handling is described in [*The Rust Programming Language* (a.k.a The Book)](https://doc.rust-lang.org/book/error-handling.html)

## Custom Error Types
    use std::error::Error;
    use std::fmt;
    use std::convert::From;
    use std::io::Error as IoError;
    use std::str::Utf8Error;

    #[derive(Debug)] // Allow the use of "{:?}" format specifier
    enum CustomError {
        Io(IoError),
        Utf8(Utf8Error),
        Other,
    }

    // Allow the use of "{}" format specifier
    impl fmt::Display for CustomError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match *self {
                CustomError::Io(ref cause) => write!(f, "I/O Error: {}", cause),
                CustomError::Utf8(ref cause) => write!(f, "UTF-8 Error: {}", cause),
                CustomError::Other => write!(f, "Unknown error!"),
            }
        }
    }

    // Allow this type to be treated like an error
    impl Error for CustomError {
        fn description(&self) -> &str {
            match *self {
                CustomError::Io(ref cause) => cause.description(),
                CustomError::Utf8(ref cause) => cause.description(),
                CustomError::Other => "Unknown error!",
            }
        }

        fn cause(&self) -> Option<&Error> {
            match *self {
                CustomError::Io(ref cause) => Some(cause),
                CustomError::Utf8(ref cause) => Some(cause),
                CustomError::Other => None,
            }
        }
    }

    // Support converting system errors into our custom error.
    // This trait is used in `try!`.
    impl From<IoError> for CustomError {
        fn from(cause: IoError) -> CustomError {
            CustomError::Io(cause)
        }
    }
    impl From<Utf8Error> for CustomError {
        fn from(cause: Utf8Error) -> CustomError {
            CustomError::Utf8(cause)
        }
    }

## Common Result methods
```
use std::io::{Read, Result as IoResult};
use std::fs::File;

struct Config(u8);

fn read_config() -> IoResult<String> {
    let mut s = String::new();
    let mut file = File::open(&get_local_config_path())
        // or_else closure is invoked if Result is Err.
        .or_else(|_| File::open(&get_global_config_path()))?;
    // Note: In `or_else`, the closure should return a Result with a matching
    //       Ok type, whereas in `and_then`, the returned Result should have a
    //       matching Err type.
    let _ = file.read_to_string(&mut s)?;
    Ok(s)
}

struct ParseError;

fn parse_config(conf_str: String) -> Result<Config, ParseError> {
    // Parse the config string...
    if conf_str.starts_with("bananas") {
        Err(ParseError)
    } else {
        Ok(Config(42))
    }
}

fn run() -> Result<(), String> {
    // Note: The error type of this function is String. We use map_err below to
    //       make the error values into String type
    let conf_str = read_config()
        .map_err(|e| format!("Failed to read config file: {}", e))?;
    // Note: Instead of using `?` above, we can use `and_then` to wrap the let
    //       expression below.
    let conf_val = parse_config(conf_str)
        .map(|Config(v)| v / 2) // map can be used to map just the Ok value
        .map_err(|_| "Failed to parse the config string!".to_string())?;

    // Run...

    Ok(())
}

fn main() {
    match run() {
        Ok(_) => println!("Bye!"),
        Err(e) => println!("Error: {}", e),
    }
}

fn get_local_config_path() -> String {
    let user_config_prefix = "/home/user/.config";
    // code to get the user config directory
    format!("{}/my_app.rc", user_config_prefix)
}

fn get_global_config_path() -> String {
    let global_config_prefix = "/etc";
    // code to get the global config directory
    format!("{}/my_app.rc", global_config_prefix)
}
```

If the config files don't exist, this outputs:

```plaintext
Error: Failed to read config file: No such file or directory (os error 2)
```

If parsing failed, this outputs:

```plaintext
Error: Failed to parse the config string!
```

_Note:_ As the project grows, it will get cumbersome to handle errors with these basic methods ([docs][1]) without losing information about the origin and propagation path of errors. Also, it is definitely a bad practice to convert errors into strings prematurely in order to handle multiple error types as shown above. A much better way is to use the crate [`error-chain`][2].


  [1]: https://doc.rust-lang.org/std/result/enum.Result.html
  [2]: https://crates.io/crates/error-chain

## Iterating through causes
It is often useful for debugging purposes to find the root cause of an error. In order to examine an error value that implements `std::error::Error`:

    use std::error::Error;

    let orig_error = call_returning_error();

    // Use an Option<&Error>. This is the return type of Error.cause().
    let mut err = Some(&orig_error as &Error);

    // Print each error's cause until the cause is None.
    while let Some(e) = err {
        println!("{}", e);
        err = e.cause();
    }

## Basic Error Reporting and Handling
`Result<T, E>` is an `enum` type which has two variants: `Ok(T)` indicating successful execution with meaningful result of type `T`, and `Err(E)` indicating occurrence of an unexpected error during execution, described by a value of type `E`.

    enum DateError {
        InvalidDay,
        InvalidMonth,
    }

    struct Date {
        day: u8,
        month: u8,
        year: i16,
    }

    fn validate(date: &Date) -> Result<(), DateError> {
        if date.month < 1 || date.month > 12 {
            Err(DateError::InvalidMonth)
        } else if date.day < 1 || date.day > 31 {
            Err(DateError::InvalidDay)
        } else {
            Ok(())
        }
    }

    fn add_days(date: Date, days: i32) -> Result<Date, DateError> {
        validate(&date)?; // notice `?` -- returns early on error
        // the date logic ...
        Ok(date)
    }

Also see [docs][1] for more details on `?` operator.

Standard library contains an [`Error` trait][2] which all error types are recommended to implement. An example implementation is given below.

    use std::error::Error;
    use std::fmt;
    
    #[derive(Debug)]
    enum DateError {
        InvalidDay(u8),
        InvalidMonth(u8),
    }
    
    impl fmt::Display for DateError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                &DateError::InvalidDay(day) => write!(f, "Day {} is outside range!", day),
                &DateError::InvalidMonth(month) => write!(f, "Month {} is outside range!", month),
            }
        }
    }
    
    impl Error for DateError {
        fn description(&self) -> &str {
            match self {
                &DateError::InvalidDay(_) => "Day is outside range!",
                &DateError::InvalidMonth(_) => "Month is outside range!",
            }
        }

        // cause method returns None by default
    }

_Note:_ Generally, `Option<T>` should not be used for reporting errors. `Option<T>` indicates an expected possibility of non-existence of a value and a single straightforward reason for it. In contrast, `Result<T, E>` is used to report unexpected errors during execution, and especially when there are multiple modes of failures to distinguish between them. Furthermore, `Result<T, E>` is only used as return values. ([An old discussion.][3])


  [1]: https://doc.rust-lang.org/std/macro.try.html
  [2]: https://doc.rust-lang.org/std/error/trait.Error.html
  [3]: https://www.reddit.com/r/rust/comments/2j0k21/rust_option_vs_result/cl77v1d/

