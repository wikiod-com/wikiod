---
title: "Basic text file IO"
slug: "basic-text-file-io"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Read and write from Documents folder

**Swift 3**

    import UIKit
    
    // Save String to file
    let fileName = "TextFile"
    let documentDirectory = try FileManager.default.urlForDirectory(.documentDirectory, in: .userDomainMask, appropriateFor: nil, create: true)
    
    var fileURL = try documentDirectory.appendingPathComponent(fileName).appendingPathExtension("txt")
    
    print("FilePath: \(fileURL.path)")
    
    var toFileString = "Text to write"
    do {
      // Write to file
      try toFileString.writeToURL(fileURL, atomically: true, encoding: NSUTF8StringEncoding)
    } catch let error as NSError {
      print("Failed writing to URL: \(fileURL), Error:\(error.localizedDescription)")
    }
    
    // Reading
    var fromFileString = ""
    do {
      fromFileString = try String(contentsOfURL: fileURL)
    } catch let error as NSError {
      print("Failed reading from URL: \(fileURL), Error: " + error.localizedDescription)
    }
    print("Text input from file: \(fromFileString)")

**Swift 2**

    import UIKit
    
    // Save String to file
    let fileName = "TextFile"
    let DocumentDirectoryURL = try! NSFileManager.defaultManager().URLForDirectory(.DocumentDirectory, inDomain: .UserDomainMask, appropriateForURL: nil, create: true)
    
    let fileURL = DocumentDirectoryURL.URLByAppendingPathComponent(fileName).URLByAppendingPathExtension("txt")
    print("FilePath: \(fileURL.path)")
    
    var toFileString = "Text to write"
    do {
      // Write to file
      try toFileString.writeToURL(fileURL, atomically: true, encoding: NSUTF8StringEncoding)
    } catch let error as NSError {
      print("Failed writing to URL: \(fileURL), Error:\(error.localizedDescription)")
    }
    
    // Reading
    var fromFileString = ""
    do {
      fromFileString = try String(contentsOfURL: fileURL)
    } catch let error as NSError {
      print("Failed reading from URL: \(fileURL), Error: " + error.localizedDescription)
    }
    print("Text input from file: \(fromFileString)")

