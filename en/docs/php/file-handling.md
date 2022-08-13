---
title: "File handling"
slug: "file-handling"
draft: false
images: []
weight: 9887
type: docs
toc: true
---

## Syntax
-    int readfile ( string $filename [, bool $use_include_path = false [, resource $context ]] )


## Parameters
| Parameter | Description|
|--------|------------|
| filename | The filename being read. |
| use_include_path | You can use the optional second parameter and set it to TRUE, if you want to search for the file in the include_path, too. |
| context | A context stream resource.  |

# Filename syntax
Most filenames passed to functions in this topic are:

1. Strings in nature.
   * File names can be passed directly. If values of other types are passed, they are cast to string. This is especially useful with `SplFileInfo`, which is the value in the iteration of `DirectoryIterator`.
1. Relative or absolute.
   * They may be absolute. On Unix-like systems, absolute paths start with `/`, e.g. `/home/user/file.txt`, while on Windows, absolute paths start with the drive, e.g. `C:/Users/user/file.txt`
   * They may also be relative, which is dependent on the value of [`getcwd`][getcwd] and subject to change by [`chdir`][chdir].
2. Accept protocols.
   * They may begin with `scheme://` to specify the protocol wrapper to manage with. For example, `file_get_contents("http://example.com")` retrieves content from http://example.com.
3. Slash-compatible.
   * While the `DIRECTORY_SEPARATOR` on Windows is a backslash, and the system returns backslashes for paths by default, the developer can still use `/` as the directory separator. Therefore, for compatibility, developers can use `/` as directory separators on all systems, but be aware that the values returned by the functions (e.g. `realpath`) may contain backslashes.

  [getcwd]: http://php.net/getcwd
  [chdir]: http://php.net/chdir

## Deleting files and directories
# Deleting files
The [`unlink`][unlink] function deletes a single file and returns whether the operation was successful.

    $filename = '/path/to/file.txt';
    
    if (file_exists($filename)) {
        $success = unlink($filename);
        
        if (!$success) {
             throw new Exception("Cannot delete $filename");
        }
    }

# Deleting directories, with recursive deletion

On the other hand, directories should be deleted with [`rmdir`][rmdir]. However, this function only deletes empty directories. To delete a directory with files, delete the files in the directories first. If the directory contains subdirectories, _recursion_ may be needed.

The following example scans files in a directory, deletes member files/directories recursively, and returns the number of files (not directories) deleted.

    function recurse_delete_dir(string $dir) : int {
        $count = 0;

        // ensure that $dir ends with a slash so that we can concatenate it with the filenames directly
        $dir = rtrim($dir, "/\\") . "/";

        // use dir() to list files
        $list = dir($dir);

        // store the next file name to $file. if $file is false, that's all -- end the loop.
        while(($file = $list->read()) !== false) {
            if($file === "." || $file === "..") continue;
            if(is_file($dir . $file)) {
                unlink($dir . $file);
                $count++;
            } elseif(is_dir($dir . $file)) {
                $count += recurse_delete_dir($dir . $file);
            }
        }

        // finally, safe to delete directory!
        rmdir($dir);

        return $count;
    }

  [unlink]: http://php.net/unlink
  [rmdir]: http://php.net/rmdir

## Convenience functions
# Raw direct IO

[`file_get_contents`][fgc] and [`file_put_contents`][fpc] provide the ability to read/write from/to a file to/from a PHP string in a single call.

[`file_put_contents`][fpc] can also be used with the `FILE_APPEND` bitmask flag to append to, instead of truncate and overwrite, the file. It can be used along with `LOCK_EX` bitmask to acquire an exclusive lock to the file while proceeding to writing. Bitmask flags can be joined with the `|` bitwise-OR operator.

    $path = "file.txt";
    // reads contents in file.txt to $contents
    $contents = file_get_contents($path);
    // let's change something... for example, convert the CRLF to LF!
    $contents = str_replace("\r\n", "\n", $contents);
    // now write it back to file.txt, replacing the original contents
    file_put_contents($path, $contents);

`FILE_APPEND` is handy for appending to log files while `LOCK_EX` helps prevent race condition of file writing from multiple processes. For example, to write to a log file about the current session:

    file_put_contents("logins.log", "{$_SESSION["username"]} logged in", FILE_APPEND | LOCK_EX);

# CSV IO
    fgetcsv($file, $length, $separator)

The [`fgetcsv`][fgcsv] parses line from open file checking for csv fields. It returns CSV fields in an array on success or `FALSE` on failure.

By default, it will read only one line of the CSV file.

    $file = fopen("contacts.csv","r");
    print_r(fgetcsv($file));    
    print_r(fgetcsv($file,5," "));
    fclose($file); 

**`contacts.csv`**
<!-- language: lang-none -->

    Kai Jim, Refsnes, Stavanger, Norway
    Hege, Refsnes, Stavanger, Norway

Output:

    Array
    (
        [0] => Kai Jim
        [1] => Refsnes
        [2] => Stavanger
        [3] => Norway
    )
    Array
    (
        [0] => Hege,
    )

# Reading a file to stdout directly
[`readfile`][readfile] copies a file to the output buffer. readfile() will not present any memory issues, even when sending large files, on its own.

    $file = 'monkey.gif';
    
    if (file_exists($file)) {
        header('Content-Description: File Transfer');
        header('Content-Type: application/octet-stream');
        header('Content-Disposition: attachment; filename="'.basename($file).'"');
        header('Expires: 0');
        header('Cache-Control: must-revalidate');
        header('Pragma: public');
        header('Content-Length: ' . filesize($file));
        readfile($file);
        exit;
    }

## Or from a file pointer
Alternatively, to seek a point in the file to start copying to stdout, use [`fpassthru`][fpassthru] instead. In the following example, the last 1024 bytes are copied to stdout:

    $fh = fopen("file.txt", "rb");
    fseek($fh, -1024, SEEK_END); 
    fpassthru($fh);

# Reading a file into an array
[`file`][file] returns the lines in the passed file in an array. Each element of the array corresponds to a line in the file, with the newline still attached.

    print_r(file("test.txt"));

**`test.txt`**
<!-- language: lang-none -->
    Welcome to File handling
    This is to test file handling

Output:

<!-- language: lang-none -->
    Array 
    ( 
        [0] => Welcome to File handling 
        [1] => This is to test file handling 
    )


  [fgc]: http://php.net/manual/en/function.file-get-contents.php
  [fpc]: http://php.net/manual/en/function.file-put-contents.php
  [fgcsv]: http://php.net/fgetcsv
  [readfile]: http://php.net/readfile
  [fpassthru]: http://php.net/fpassthru
  [file]: http://php.net/manual/en/function.file.php

## Getting file information
# Check if a path is a directory or a file
The [`is_dir`][is_dir] function returns whether the argument is a directory, while [`is_file`][is_file] returns whether the argument is a file. Use [`file_exists`][file_exists] to check if it is either.

    $dir  = "/this/is/a/directory";
    $file = "/this/is/a/file.txt";

    echo is_dir($dir) ? "$dir is a directory" : "$dir is not a directory", PHP_EOL,
        is_file($dir) ? "$dir is a file" : "$dir is not a file", PHP_EOL,
        file_exists($dir) ? "$dir exists" : "$dir doesn't exist", PHP_EOL,
        is_dir($file) ? "$file is a directory" : "$file is not a directory", PHP_EOL,
        is_file($file) ? "$file is a file" : "$file is not a file", PHP_EOL,
        file_exists($file) ? "$file exists" : "$file doesn't exist", PHP_EOL;

This gives:

    /this/is/a/directory is a directory
    /this/is/a/directory is not a file
    /this/is/a/directory exists
    /this/is/a/file.txt is not a directory
    /this/is/a/file.txt is a file
    /this/is/a/file.txt exists

# Checking file type
Use [`filetype`][filetype] to check the type of a file, which may be:
* `fifo`
* `char`
* `dir`
* `block`
* `link`
* `file`
* `socket`
* `unknown`

Passing the filename to the [`filetype`][filetype] directly:

    echo filetype("~"); // dir

Note that `filetype` returns false and triggers an `E_WARNING` if the file doesn't exist.

# Checking readability and writability
Passing the filename to the [`is_writable`][is_writable] and [`is_readable`][is_readable] functions check whether the file is writable or readable respectively.

The functions return `false` gracefully if the file does not exist.

# Checking file access/modify time
Using [`filemtime`][filemtime] and [`fileatime`][fileatime] returns the timestamp of the last modification or access of the file. The return value is a Unix timestamp -- see https://www.wikiod.com/php/working-with-dates-and-time for details.

    echo "File was last modified on " . date("Y-m-d", filemtime("file.txt"));
    echo "File was last accessed on " . date("Y-m-d", fileatime("file.txt"));

# Get path parts with fileinfo
    $fileToAnalyze = ('/var/www/image.png');
    
    $filePathParts = pathinfo($fileToAnalyze);

    echo '<pre>';
       print_r($filePathParts);
    echo '</pre>';

This example will output:

    Array
    (
        [dirname] => /var/www
        [basename] => image.png
        [extension] => png
        [filename] => image
    )

Which can be used as:

    $filePathParts['dirname']
    $filePathParts['basename']
    $filePathParts['extension']
    $filePathParts['filename']


| Parameter | Details |
| ------ | ------ |
| $path   | The full path of the file to be parsed   |
| $option   | One of four available options [PATHINFO_DIRNAME, PATHINFO_BASENAME, PATHINFO_EXTENSION or PATHINFO_FILENAME]   |

 - If an option (the second parameter) is not passed, an associative array is returned otherwise a string is returned.
 - Does not validate that the file exists.
 - Simply parses the string into parts.  No validation is done on the file (no mime-type checking, etc.)
 - The extension is simply the last extension of `$path`  The path for the file `image.jpg.png` would be `.png` even if it technically a `.jpg` file.  A file without an extension will not return an extension element in the array.

  [filetype]: http://php.net/filetype
  [is_dir]: http://php.net/is-dir
  [is_file]: http://php.net/is-file
  [file_exists]: http://php.net/file-exists
  [is_readable]: http://php.net/is-readable
  [is_writable]: http://php.net/is-writable
  [fileatime]: http://php.net/fileatime
  [filemtime]: http://php.net/filemtime

## Minimize memory usage when dealing with large files
If we need to parse a large file, e.g. a CSV more than 10 Mbytes containing millions of rows, some use `file` or `file_get_contents` functions and end up with hitting `memory_limit` setting with 

> Allowed memory size of XXXXX bytes exhausted

error. Consider the following source (top-1m.csv has exactly 1 million rows and is about 22 Mbytes of size) 


    var_dump(memory_get_usage(true));
    $arr = file('top-1m.csv');
    var_dump(memory_get_usage(true));

This outputs:

    int(262144)
    int(210501632) 

because the interpreter needed to hold all the rows in `$arr` array, so it consumed ~200 Mbytes of RAM. Note that we haven't even done anything with the contents of the array. 

Now consider the following code:

    var_dump(memory_get_usage(true));
    $index = 1;
    if (($handle = fopen("top-1m.csv", "r")) !== FALSE) {
        while (($row = fgetcsv($handle, 1000, ",")) !== FALSE) {
            file_put_contents('top-1m-reversed.csv',$index . ',' . strrev($row[1]) . PHP_EOL, FILE_APPEND);
            $index++;
        }
        fclose($handle);
    }
    var_dump(memory_get_usage(true));

which outputs

    int(262144)
    int(262144)

so we don't use a single extra byte of memory, but parse the whole CSV and save it to another file reversing the value of the 2nd column. That's because `fgetcsv` reads only one row and `$row` is overwritten in every loop. 

## Stream-based file IO
# Opening a stream
[`fopen`][fopen] opens a file stream handle, which can be used with various functions for reading, writing, seeking and other functions on top of it. This value is of `resource` type, and cannot be passed to other threads persisting its functionality.

    $f = fopen("errors.log", "a"); // Will try to open errors.log for writing

The second parameter is the mode of the file stream:


| Mode | Description |
| ---: | :---------- |
| `r` | Open in read only mode, starting at the beginning of the file |
| `r+` | Open for reading and writing, starting at the beginning of the file |
| `w` | open for writing only, starting at the beginning of the file.  If the file exists it will empty the file.  If it doesn't exist it will attempt to create it. |
| `w+` | open for reading and writing, starting at the beginning of the file.  If the file exists it will empty the file.  If it doesn't exist it will attempt to create it. | 
| `a` | open a file for writing only, starting at the end of the file.  If the file does not exist, it will try to create it |
| `a+` | open a file for reading and writing, starting at the end of the file.  If the file does not exist, it will try to create it |
| `x` | create and open a file for writing only.  If the file exists the `fopen` call will fail |
| `x+` | create and open a file for reading and writing.  If the file exists the `fopen` call will fail |
| `c` | open the file for writing only.  If the file does not exist it will try to create it.  It will start writing at the beginning of the file, but will not empty the file ahead of writing |
| `c+` | open the file for reading and writing.  If the file does not exist it will try to create it.  It will start writing at the beginning of the file, but will not empty the file ahead of writing |

Adding a `t` behind the mode (e.g. `a+b`, `wt`, etc.) in Windows will translate `"\n"` line endings to `"\r\n"` when working with the file. Add `b` behind the mode if this is not intended, especially if it is a binary file.

The PHP application should close streams using [`fclose`][fclose] when they are no longer used to prevent the `Too many open files` error. This is particularly important in CLI programs, since the streams are only closed when the runtime shuts down -- this means that in web servers, it _may not be *necessary*_ (but still _should_, as a practice to prevent resource leak) to close the streams if you do not expect the process to run for a long time, and will not open many streams.

# Reading
Using [`fread`][fread] will read the given number of bytes from the file pointer, or until an EOF is met.

## Reading lines
Using [`fgets`][fgets] will read the file until an EOL is reached, or the given length is read.

Both [`fread`][fread] and [`fgets`][fgets] will move the file pointer while reading.

## Reading everything remaining
Using [`stream_get_contents`][sgc] will all remaining bytes in the stream into a string and return it.

# Adjusting file pointer position
Initially after opening the stream, the file pointer is at the beginning of the file (or the end, if the mode `a` is used). Using the [`fseek`][fseek] function will move the file pointer to a new position, relative to one of three values:

* `SEEK_SET`: This is the default value; the file position offset will be relative to the beginning of the file.
* `SEEK_CUR`: The file position offset will be relative to the current position.
* `SEEK_END`: The file position offset will be relative to the end of the file. Passing a negative offset is the most common use for this value; it will move the file position to the specified number of bytes before the end of file.

[`rewind`][rewind] is a convenience shortcut of `fseek($fh, 0, SEEK_SET)`.

Using [`ftell`][ftell] will show the absolute position of the file pointer.

For example, the following script reads skips the first 10 bytes, reads the next 10 bytes, skips 10 bytes, reads the next 10 bytes, and then the last 10 bytes in file.txt:

    $fh = fopen("file.txt", "rb");
    fseek($fh, 10); // start at offset 10
    echo fread($fh, 10); // reads 10 bytes
    fseek($fh, 10, SEEK_CUR); // skip 10 bytes
    echo fread($fh, 10); // read 10 bytes
    fseek($fh, -10, SEEK_END); // skip to 10 bytes before EOF
    echo fread($fh, 10); // read 10 bytes
    fclose($fh);

# Writing
Using [`fwrite`][fwrite] writes the provided string to the file starting at the current file pointer.

    fwrite($fh, "Some text here\n");

  [fopen]: http://php.net/fopen
  [fclose]: http://php.net/fclose
  [fwrite]: http://php.net/fwrite
  [fread]: http://php.net/fread
  [fseek]: http://php.net/fseek
  [rewind]: http://php.net/rewind
  [fgets]: http://php.net/fgets
  [ftell]: http://php.net/ftell
  [sgc]: http://php.net/stream-get-contents

## Moving and Copying files and directories
# Copying files
[`copy`][copy] copies the source file in the first argument to the destination in the second argument. The resolved destination needs to be in a directory that is already created.

    if (copy('test.txt', 'dest.txt')) {
        echo 'File has been copied successfully';
    } else {
        echo 'Failed to copy file to destination given.'
    }

# Copying directories, with recursion
Copying directories is pretty much similar to deleting directories, except that for files [`copy`][copy] instead of [`unlink`][unlink] is used, while for directories, [`mkdir`][mkdir] instead of [`rmdir`][rmdir] is used, at the beginning instead of being at the end of the function.

    function recurse_delete_dir(string $src, string $dest) : int {
        $count = 0;

        // ensure that $src and $dest end with a slash so that we can concatenate it with the filenames directly
        $src = rtrim($dest, "/\\") . "/";
        $dest = rtrim($dest, "/\\") . "/";

        // use dir() to list files
        $list = dir($src);

        // create $dest if it does not already exist
        @mkdir($dest);

        // store the next file name to $file. if $file is false, that's all -- end the loop.
        while(($file = $list->read()) !== false) {
            if($file === "." || $file === "..") continue;
            if(is_file($src . $file)) {
                copy($src . $file, $dest . $file);
                $count++;
            } elseif(is_dir($src . $file)) {
                $count += recurse_copy_dir($src . $file, $dest . $file);
            }
        }

        return $count;
    }

# Renaming/Moving
Renaming/Moving files and directories is much simpler. Whole directories can be moved or renamed in a single call, using the [`rename`][rename] function.

- `rename("~/file.txt", "~/file.html");`
- `rename("~/dir", "~/old_dir");`
- `rename("~/dir/file.txt", "~/dir2/file.txt");`

  [copy]: http://php.net/copy
  [unlink]: http://php.net/unlink
  [mkdir]: http://php.net/mkdir
  [rmdir]: http://php.net/rmdir
  [rename]: http://php.net/rename

