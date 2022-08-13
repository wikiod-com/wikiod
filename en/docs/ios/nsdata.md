---
title: "NSData"
slug: "nsdata"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

# Useful Resources

[Apple Documentation (NSData)][1]

[NSData.dataWithContentsOfFile()][2]

[NSData.bytes][3]


  [1]: https://developer.apple.com/library/mac/documentation/Cocoa/Reference/Foundation/Classes/NSData_Class/
  [2]: https://developer.apple.com/library/mac/documentation/Cocoa/Reference/Foundation/Classes/NSData_Class/#//apple_ref/occ/clm/NSData/dataWithContentsOfFile:
  [3]: https://developer.apple.com/library/mac/documentation/Cocoa/Reference/Foundation/Classes/NSData_Class/#//apple_ref/occ/instp/NSData/bytes

## Converting NSData to HEX string
`NSData` can be represented as hexadecimal string, similar to what it outputs in its `description` method.

## Swift

    extension NSData {

        func hexString() -> String {
            return UnsafeBufferPointer<UInt8>(start: UnsafePointer<UInt8>(bytes), count: length)
                .reduce("") { $0 + String(format: "%02x", $1) }
        }

    }

## Objective-C

    @implementation NSData (HexRepresentation)

    - (NSString *)hexString {
        const unsigned char *bytes = (const unsigned char *)self.bytes;
        NSMutableString *hex = [NSMutableString new];
        for (NSInteger i = 0; i < self.length; i++) {
            [hex appendFormat:@"%02x", bytes[i]];
        }
        return [hex copy];
    }

    @end

## Creating NSData objects
# Using a file

## Swift

    let data = NSData(contentsOfFile: filePath) //assuming filePath is a valid path

## Objective-C

    NSData *data = [NSData dataWithContentsOfFile:filePath]; //assuming filePath is a valid path

# Using a String object

## Swift

    let data = (string as NSString).dataUsingEncoding(NSUTF8StringEncoding) //assuming string is a String object

## Objective-C

    NSData *data = [string dataUsingEncoding:NSUTF8StringEncoding]; //assuming string is a String object



## Converting NSData to other types
# To String

## Swift

    let string = String(NSString(data: data, encoding: NSUTF8StringEncoding)) //assuming data is a valid NSData object

## Objective-C

    NSString *string = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding]; //assuming data is a valid NSData object
    [string release];

# To Array

## Swift

    let array = data.bytes as! NSMutableArray //assuming data is a valid NSData object

## Objective-C

    NSMutableArray *array = (NSMutableArray *)[data bytes]; //assuming data is a valid NSData object

# To Bytes Array

## Swift

    let bytesArray = data.bytes as! UInt8 //assuming data is a valid NSData object

## Objective-C

    UInt8 *bytesArray = (UInt8 *)data.bytes; //assuming data is a valid NSData object

