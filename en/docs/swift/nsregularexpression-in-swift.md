---
title: "NSRegularExpression in Swift"
slug: "nsregularexpression-in-swift"
draft: false
images: []
weight: 9935
type: docs
toc: true
---

**Special Characters**

       *?+[(){}^$|\./


## Extending String to do simple pattern matching
    extension String {
        func matchesPattern(pattern: String) -> Bool {
            do {
                let regex = try NSRegularExpression(pattern: pattern,
                                                    options: NSRegularExpressionOptions(rawValue: 0))
                let range: NSRange = NSMakeRange(0, self.characters.count)
                let matches = regex.matchesInString(self, options: NSMatchingOptions(), range: range)
                return matches.count > 0
            } catch _ {
                return false
            }
        }
    }
    
    // very basic examples - check for specific strings
    dump("Pinkman".matchesPattern("(White|Pinkman|Goodman|Schrader|Fring)"))
    
    // using character groups to check for similar-sounding impressionist painters
    dump("Monet".matchesPattern("(M[oa]net)"))
    dump("Manet".matchesPattern("(M[oa]net)"))
    dump("Money".matchesPattern("(M[oa]net)"))     // false
    
    // check surname is in list
    dump("Skyler White".matchesPattern("\\w+ (White|Pinkman|Goodman|Schrader|Fring)"))
    
    // check if string looks like a UK stock ticker
    dump("VOD.L".matchesPattern("[A-Z]{2,3}\\.L"))
    dump("BP.L".matchesPattern("[A-Z]{2,3}\\.L"))
    
    // check entire string is printable ASCII characters
    dump("tab\tformatted text".matchesPattern("^[\u{0020}-\u{007e}]*$"))
    
    // Unicode example: check if string contains a playing card suit
    dump("♠︎".matchesPattern("[\u{2660}-\u{2667}]"))
    dump("♡".matchesPattern("[\u{2660}-\u{2667}]"))
    dump("😂".matchesPattern("[\u{2660}-\u{2667}]"))    // false
    
    // NOTE: regex needs Unicode-escaped characters
    dump("♣︎".matchesPattern("♣︎"))           // does NOT work

Below is another example which builds on the above to do something useful, which can't easily be done by any other method and lends itself well to a regex solution.

    // Pattern validation for a UK postcode.
    // This simply checks that the format looks like a valid UK postcode and should not fail on false positives.
    private func isPostcodeValid(postcode: String) -> Bool {
        return postcode.matchesPattern("^[A-Z]{1,2}([0-9][A-Z]|[0-9]{1,2})\\s[0-9][A-Z]{2}")
    }
    
    // valid patterns (from https://en.wikipedia.org/wiki/Postcodes_in_the_United_Kingdom#Validation)
    // will return true
    dump(isPostcodeValid("EC1A 1BB"))
    dump(isPostcodeValid("W1A 0AX"))
    dump(isPostcodeValid("M1 1AE"))
    dump(isPostcodeValid("B33 8TH"))
    dump(isPostcodeValid("CR2 6XH"))
    dump(isPostcodeValid("DN55 1PT"))
    
    // some invalid patterns
    // will return false
    dump(isPostcodeValid("EC12A 1BB"))
    dump(isPostcodeValid("CRB1 6XH"))
    dump(isPostcodeValid("CR 6XH"))







## Basic Usage


## Replacing Substrings


## Special Characters


## Validation


## NSRegularExpression for mail validation
    func isValidEmail(email: String) -> Bool {

        let emailRegEx = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}"
        
        let emailTest = NSPredicate(format:"SELF MATCHES %@", emailRegEx)
        return emailTest.evaluate(with: email)
    }

or you could use String extension like this: 
   
    extension String
    {
        func isValidEmail() -> Bool {
    
            let emailRegEx = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}"
            
            let emailTest = NSPredicate(format:"SELF MATCHES %@", emailRegEx)
            return emailTest.evaluate(with: self)
        }
    }

