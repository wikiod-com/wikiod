---
title: "Caching on disk space"
slug: "caching-on-disk-space"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

Caching videos, images and audios using `URLSession` and `FileManager`

## Reading
    let url = "https://path-to-media"
    guard let documentsUrl = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first,
          let searchQuery = url.absoluteString.components(separatedBy: "/").last else {
      return nil
    }

    do {
      let directoryContents = try FileManager.default.contentsOfDirectory(at: documentsUrl, includingPropertiesForKeys: nil, options: [])
      let cachedFiles = directoryContents.filter { $0.absoluteString.contains(searchQuery) }
      
      // do something with the files found by the url
    } catch {
      // Could not find any files
    }

## Saving
    let url = "https://path-to-media"
    let request = URLRequest(url: url)
    let downloadTask = URLSession.shared.downloadTask(with: request) { (location, response, error) in
      guard let location = location,
            let response = response,
            let documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true).first else {
        return
      }
      let documentsDirectoryUrl = URL(fileURLWithPath: documentsPath)
      let documentUrl = documentsDirectoryUrl.appendingPathComponent(response.suggestedFilename)
      let _ = try? FileManager.default.moveItem(at: location, to: documentUrl)

      // documentUrl is the local URL which we just downloaded and saved to the FileManager
    }.resume()

