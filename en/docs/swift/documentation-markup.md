---
title: "Documentation markup"
slug: "documentation-markup"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Class documentation
Here is a basic class documentation example:

    /// Class description
    class Student {

        // Member description
        var name: String
        
        /// Method description
        ///
        /// - parameter content:   parameter description
        ///
        /// - returns: return value description
        func say(content: String) -> Bool {
            print("\(self.name) say \(content)")
            return true
        }
    }

Note that with *Xcode 8*, you can generate the documentation snippet with <kbd>command</kbd>+<kbd>option</kbd>+<kbd>/</kbd>.

This will return:
[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/kPt3z.png

## Documentation styles
    /**
     Adds user to the list of poople which are assigned the tasks.
     
     - Parameter name: The name to add
     - Returns: A boolean value (true/false) to tell if user is added successfully to the people list.
    */
    func addMeToList(name: String) -> Bool {
        
        // Do something....
        
        
        return true
    }
[![enter image description here][1]][1]


    /// This is a single line comment
    func singleLineComment() {
        
    }
[![enter image description here][2]][2]


    /**
     Repeats a string `times` times.
     
     - Parameter str:   The string to repeat.
     - Parameter times: The number of times to repeat `str`.
     
     - Throws: `MyError.InvalidTimes` if the `times` parameter
     is less than zero.
     
     - Returns: A new string with `str` repeated `times` times.
    */
    func repeatString(str: String, times: Int) throws -> String {
        guard times >= 0 else { throw MyError.invalidTimes }
        return "Hello, world"
    }
[![enter image description here][3]][3]


    /**
     # Lists
     
     You can apply *italic*, **bold**, or `code` inline styles.
     
     ## Unordered Lists
     - Lists are great,
     - but perhaps don't nest
     - Sub-list formatting
     - isn't the best.
     
     ## Ordered Lists
     1. Ordered lists, too
     2. for things that are sorted;
     3. Arabic numerals
     4. are the only kind supported.
    */
    func complexDocumentation() {
        
    }
    
[![enter image description here][4]][4]


    /**
     Frame and construction style.
     
     - Road: For streets or trails.
     - Touring: For long journeys.
     - Cruiser: For casual trips around town.
     - Hybrid: For general-purpose transportation.
    */
    enum Style {
        case Road, Touring, Cruiser, Hybrid
    }
[![enter image description here][5]][5]


  [1]: https://i.stack.imgur.com/tsfA7.png
  [2]: https://i.stack.imgur.com/SzpCE.png
  [3]: https://i.stack.imgur.com/GLrTl.png
  [4]: https://i.stack.imgur.com/8yhkl.png
  [5]: https://i.stack.imgur.com/cQzyn.png

