---
title: "Useful Regex Showcase"
slug: "useful-regex-showcase"
draft: false
images: []
weight: 9941
type: docs
toc: true
---

## Match an email address
> Matching an email address within a string is a hard task, because the
> specification defining it, [the RFC2822](https://www.ietf.org/rfc/rfc2822.txt),
> is complex making it hard to implement as a regex. For more details why it is
> not a good idea to match an email with a regex, please refer to the
> antipattern example [when not to use a regex: for matching
> emails](https://www.wikiod.com/regex/when-you-should-not-use-regular-expressions).
> The best advice to note from that page is to use a peer reviewed and widely
> library in your favorite language to implement this.

## Validate an email address format

When you need to rapidly validate an entry to make sure it _looks like_ an
email, the best option is to keep it simple:

    ^\S{1,}@\S{2,}\.\S{2,}$

That regex will check that the mail address is a non-space separated sequence
of characters of length greater than one, followed by an `@`, followed by two
sequences of non-spaces characters of length two or more separated by a `.`.
It's not perfect, and might validate invalid addresses (according to the
format), but most importantly, it's not invalidating valid addresses.

## Check the address exists

The only reliable way to check that an email is valid is to check for its existence.
There used to be the `VRFY` SMTP command that has been designed for that purpose, but
sadly, after [being abused by spammers it's now not available anymore](http://stackoverflow.com/a/566121/1290438).

So the only way you're left with to check that the mail is valid and exists is to
actually send an e-mail to that address.

## Huge Regex alternatives

Though, it's not impossible to validate an address email using a regex. The only issues
is that the closer to the specification those regexes will be, the bigger they will be and
as a consequency they are impossibly hard to read and maintain. Below, you'll find example
of such more accurate regex that are being used in some libraries.

> **⚠️** The following regex are given for documentation and learning purposes, copy pasting
> them in your code is a bad idea. Instead, use that library directly, so you can rely on
> upstream code and peer developers to keep your email parsing code up to date and maintained.

### Perl Address matching module

The best examples of such regex are in some languages standard libraries. For example,
there's one from the [`RFC::RFC822::Address` module](https://metacpan.org/pod/Mail::RFC822::Address)
in the Perl library that tries to be as accurate as possible according to the RFC. For your
curiosity you can find a version of that regex at [this URL](http://www.ex-parrot.com/%7Epdw/Mail-RFC822-Address.html),
that has been generated from the grammar, and if you're tempted to copy paste it, 
here's quote from the regex' author:

> "_I do not maintain the regular expression [linked]. There may be bugs in it that have already been fixed in the Perl module._"

### .Net Address matching module

Another, shorter variant is the one used by the .Net standard library in the
[`EmailAddressAttribute` module](http://referencesource.microsoft.com/#System.ComponentModel.DataAnnotations/DataAnnotations/EmailAddressAttribute.cs,54):

    ^((([a-z]|\d|[!#\$%&'\*\+\-\/=\?\^_`{\|}~]|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])+(\.([a-z]|\d|[!#\$%&'\*\+\-\/=\?\^_`{\|}~]|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])+)*)|((\x22)((((\x20|\x09)*(\x0d\x0a))?(\x20|\x09)+)?(([\x01-\x08\x0b\x0c\x0e-\x1f\x7f]|\x21|[\x23-\x5b]|[\x5d-\x7e]|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])|(\\([\x01-\x09\x0b\x0c\x0d-\x7f]|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF]))))*(((\x20|\x09)*(\x0d\x0a))?(\x20|\x09)+)?(\x22)))@((([a-z]|\d|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])|(([a-z]|\d|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])([a-z]|\d|-|\.|_|~|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])*([a-z]|\d|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])))\.)+(([a-z]|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])|(([a-z]|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])([a-z]|\d|-|\.|_|~|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])*([a-z]|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])))\.?$

But even if it's _shorter_ it's still too big to be readable and easily maintainable.

### Ruby Address matching module

In ruby a composition of regex are being used in the [rfc822 module](https://github.com/mspanc/rfc822/blob/master/lib/rfc822.rb)
to match an address. This is a neat idea, as in case bugs are found, it will be easier to pinpoint the regex part to change and
fix it.

### Python Address matching module

As a counter example, the python [email parsing module](https://github.com/python/cpython/blob/2d264235f6e066611b412f7c2e1603866e0f7f1b/Lib/email/_parseaddr.py#L260-L317)
is not using a regex, but instead implements it using a parser.


## Match a date
You should remember that regex was designed for matching a date (or not). Saying that a date is *valid* is a much more complicated struggle, since it will require a lot of exception handling (see [leap year conditions](https://en.wikipedia.org/wiki/Leap_year#Algorithm)).

Let's start by matching the month (1 - 12) with an optional leading 0:

    0?[1-9]|1[0-2]

To match the day, also with an optional leading 0:

    0?[1-9]|[12][0-9]|3[01]

And to match the year (let's just assume the range 1900 - 2999):

    (?:19|20)[0-9]{2}

The separator can be a space, a dash, a slash, empty, etc. Feel free to add anything you feel may be used as a separator:

    [-\\/ ]?

Now you concatenate the whole thing and get:

    (0?[1-9]|1[0-2])[-\\/ ]?(0?[1-9]|[12][0-9]|3[01])[-/ ]?(?:19|20)[0-9]{2} // MMDDYYYY
    (0?[1-9]|[12][0-9]|3[01])[-\\/ ]?(0?[1-9]|1[0-2])[-/ ]?(?:19|20)[0-9]{2} // DDMMYYYY
    (?:19|20)[0-9]{2}[-\\/ ]?(0?[1-9]|1[0-2])[-/ ]?(0?[1-9]|[12][0-9]|3[01]) // YYYYMMDD

If you want to be a bit more pedantic, you can use a back reference to be sure that the two separators will be the same:

    (0?[1-9]|1[0-2])([-\\/ ]?)(0?[1-9]|[12][0-9]|3[01])\2(?:19|20)[0-9]{2} // MMDDYYYY
                                                     ^ refer to [-/ ]
    (0?[1-9]|[12][0-9]|3[01])([-\\/ ]?)(0?[1-9]|1[0-2])\2(?:19|20)[0-9]{2} // DDMMYYYY
    (?:19|20)[0-9]{2}([-\\/ ]?)(0?[1-9]|1[0-2])\2(0?[1-9]|[12][0-9]|3[01]) // YYYYMMDD

## Match a phone number
Here's how to match a prefix code (a `+` or (00), then a number from 1 to 1939, with an optional space):  
<sup>This doesn't look for a *valid* prefix but something that might be a prefix. See the [full list](https://en.wikipedia.org/wiki/List_of_country_calling_codes#Tree_list) of prefixes</sup>

    (?:00|\+)?[0-9]{4}

Then, as the entire phone number length is, at most, 15, we can look for up to 14 digits:  
<sup>At least 1 digit is spent for the prefix</sup> 

    [0-9]{1,14}

The numbers may contains spaces, dots, or dashes and may be grouped by 2 or 3.

    (?:[ .-][0-9]{3}){1,5}

---

With the optional prefix:

    (?:(?:00|\+)?[0-9]{4})?(?:[ .-][0-9]{3}){1,5}

---

If you want to match a specific country format, you can use this [search query](http://stackoverflow.com/search?q=phone+number+%5Bregex%5D) and add the country, the question has certainly already been asked.

## Match an IP Address
**IPv4**

To match IPv4 address format, you need to check for numbers `[0-9]{1,3}` three times `{3}` separated by periods `\.` and ending with another number.

    ^(?:[0-9]{1,3}\.){3}[0-9]{1,3}$

This regular expression is too simple - if you want to it to be accurate, you need to check that the numbers are between `0` and `255`, with the regex above accepting `444` in any position. You want to check for 250-255 with `25[0-5]`, or any other 200 value `2[0-4][0-9]`, or any 100 value or less with `[01]?[0-9][0-9]`. You want to check that it is followed by a period `\.` three times `{3}` and then once without a period.

    ^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$

**IPv6**

IPv6 addresses take the form of 8 16-bit hex words delimited with the colon (`:`) character. In this case, we check for 7 words followed by colons, followed by one that is not. If a word has leading zeroes, they _may_ be truncated, meaning each word may contain between 1 and 4 hex digits.

    ^(?:[0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$

This, however, is insufficient. As IPv6 addresses can become quite "wordy", the standard specifies that zero-only words may be replaced by `::`. This may only be done once in an address (for anywhere between 1 and 7 consecutive words), as it would otherwise be indeterminate. This produces a number of (rather nasty) variations:

    ^::(?:[0-9a-fA-F]{1,4}:){0,6}[0-9a-fA-F]{1,4}$
    ^[0-9a-fA-F]{1,4}::(?:[0-9a-fA-F]{1,4}:){0,5}[0-9a-fA-F]{1,4}$
    ^[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}::(?:[0-9a-fA-F]{1,4}:){0,4}[0-9a-fA-F]{1,4}$
    ^(?:[0-9a-fA-F]{1,4}:){0,2}[0-9a-fA-F]{1,4}::(?:[0-9a-fA-F]{1,4}:){0,3}[0-9a-fA-F]{1,4}$
    ^(?:[0-9a-fA-F]{1,4}:){0,3}[0-9a-fA-F]{1,4}::(?:[0-9a-fA-F]{1,4}:){0,2}[0-9a-fA-F]{1,4}$
    ^(?:[0-9a-fA-F]{1,4}:){0,4}[0-9a-fA-F]{1,4}::(?:[0-9a-fA-F]{1,4}:)?[0-9a-fA-F]{1,4}$
    ^(?:[0-9a-fA-F]{1,4}:){0,5}[0-9a-fA-F]{1,4}::[0-9a-fA-F]{1,4}$
    ^(?:[0-9a-fA-F]{1,4}:){0,6}[0-9a-fA-F]{1,4}::$

Now, putting it all together (using alternation) yields:

    ^(?:[0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$|
    ^::(?:[0-9a-fA-F]{1,4}:){0,6}[0-9a-fA-F]{1,4}$|
    ^[0-9a-fA-F]{1,4}::(?:[0-9a-fA-F]{1,4}:){0,5}[0-9a-fA-F]{1,4}$|
    ^[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}::(?:[0-9a-fA-F]{1,4}:){0,4}[0-9a-fA-F]{1,4}$|
    ^(?:[0-9a-fA-F]{1,4}:){0,2}[0-9a-fA-F]{1,4}::(?:[0-9a-fA-F]{1,4}:){0,3}[0-9a-fA-F]{1,4}$|
    ^(?:[0-9a-fA-F]{1,4}:){0,3}[0-9a-fA-F]{1,4}::(?:[0-9a-fA-F]{1,4}:){0,2}[0-9a-fA-F]{1,4}$|
    ^(?:[0-9a-fA-F]{1,4}:){0,4}[0-9a-fA-F]{1,4}::(?:[0-9a-fA-F]{1,4}:)?[0-9a-fA-F]{1,4}$|
    ^(?:[0-9a-fA-F]{1,4}:){0,5}[0-9a-fA-F]{1,4}::[0-9a-fA-F]{1,4}$|
    ^(?:[0-9a-fA-F]{1,4}:){0,6}[0-9a-fA-F]{1,4}::$

Be sure to write it out in multiline mode and with a pile of comments so whoever is inevitably tasked with figuring out what this means doesn't come after you with a blunt object.

## Validate a 12hr and 24hr time string
For a 12hour time format one can use:

    ^(?:0?[0-9]|1[0-2])[-:][0-5][0-9]\s*[ap]m$

Where 

 - `(?:0?[0-9]|1[0-2])` is the hour
 - `[-:]` is the separator, which can be adjusted to fit your need
 - `[0-5][0-9]` is the minute
 - `\s*[ap]m` followed any number of whitespace characters, and `am` or `pm`

If you need the seconds:

    ^(?:0?[0-9]|1[0-2])[-:][0-5][0-9][-:][0-5][0-9]\s*[ap]m$

---

For a 24hr time format:

    ^(?:[01][0-9]|2[0-3])[-:h][0-5][0-9]$

Where:

 - `(?:[01][0-9]|2[0-3])` is the hour
 - `[-:h]` the separator, which can be adjusted to fit your need
 - `[0-5][0-9]` is the minute

With the seconds:

    ^(?:[01][0-9]|2[0-3])[-:h][0-5][0-9][-:m][0-5][0-9]$

Where `[-:m]` is a second separator, replacing the `h` for hours with an `m` for minutes, and `[0-5][0-9]` is the second.

## Match UK postcode
Regex to match [postcodes in UK][1]


The format is as follows, where A signifies a letter and 9 a digit:

| Format    | Coverage    | Example |
| ------ | ------ |------ |
| Cell   | Cell   | |
|AA9A 9AA  |  WC postcode area; EC1–EC4, NW1W, SE1P, SW1 |   EC1A 1BB
|A9A 9AA  |  E1W, N1C, N1P  |  W1A 0AX
|A9 9AA, A99 9AA   | B, E, G, L, M, N, S, W  |  M1 1AE, B33 8TH
|AA9 9AA, AA99 9AA |  All other postcodes   | CR2 6XH, DN55 1PT




    (GIR 0AA)|((([A-Z-[QVX]][0-9][0-9]?)|(([A-Z-[QVX]][A-Z-[IJZ]][0-9][0-9]?)|(([A-Z-[QVX]][0-9][A-HJKPSTUW])|([A-Z-[QVX]][A-Z-[IJZ]][0-9][ABEHMNPRVWXY])))) [0-9][A-Z-[CIKMOV]]{2})

Where first part:
 
     (GIR 0AA)|((([A-Z-[QVX]][0-9][0-9]?)|(([A-Z-[QVX]][A-Z-[IJZ]][0-9][0-9]?)|(([A-Z-[QVX]][0-9][A-HJKPSTUW])|([A-Z-[QVX]][A-Z-[IJZ]][0-9][ABEHMNPRVWXY]))))

Second:    

    [0-9][A-Z-[CIKMOV]]{2})




  [1]: https://en.wikipedia.org/wiki/Postcodes_in_the_United_Kingdom

