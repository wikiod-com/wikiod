---
title: "XMPP Addresses aka. JIDs (Jabber Identifiers)"
slug: "xmpp-addresses-aka-jids-jabber-identifiers"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Syntax
- [ localpart "@" ] domainpart [ "/" resourcepart ]


## Parameters
 Part | Common Usage
 -------------|------------
 Localpart | Identifies an XMPP entity (optional)
 Domainpart | Identifies the XMPP service
 Resourcepart | Identifies a session of an XMPP entity (optional)

XMPP addresses, more commonly known as JIDs (Jabber Identifiers) are defined in [RFC 7622](https://tools.ietf.org/html/rfc7622) and act as addresses on the XMPP network. They look like an email address, but sometimes have an optional "resourcepart" at the end that identifies a particular client logged in as the account represented by the rest of the address (since XMPP may have multiple clients connected per account). An example of an XMPP address with the resourcepart (a client) `xyz` is:

    romeo@example.net/xyz

## Splitting a JID (generic)
To split a JID into its component parts (the localpart, domainpart, and resourcepart), the following algorithm should be used (where the localpart is represented by `lp`, the resourcepart by `rp`, and the domainpart by `dp` and `∈` is used to check if the given character is included in the string):

[![Split JID flowchart][1]][1]

Note that the localpart and resourcepart are optional and may result in empty strings (you may have a jid that is *just* a domainpart).

  [1]: http://i.stack.imgur.com/tdvJY.png

## JID Types
A JID consists of three parts: localpart@domainpart/resourcepart.

**Full JIDs** (always have a resource part)

    romeo@example.org/orchard
    example.org/da863ab

**Bare JIDs** (always without resource part)

     romeo@example.org
     example.org

## Validating a JID (generic)
Unlike emails, JIDs were defined with Internationalization (i18n) in mind using the
Preparation, Enforcement, and Comparison of Internationalized Strings (PRECIS) framework. PRECIS (defined in [RFC 7564][rfc7564]), is a framework for comparing strings safely in a variety of contexts. For instance, imagine you have registered the nickname “Richard IV” (Latin capital letters I, Vee) in a group chat: Using PRECIS the chat application could ensure that no one else comes along and registers the nickname “Richard Ⅳ” (Unicode Roman Numeral 4) and uses it to impersonate you.

The algorithm for validating a JID that has already been split into its localpart, domainpart, and resourcepart (See: https://www.wikiod.com/xmpp/xmpp-addresses-aka-jids-jabber-identifiers#Splitting a JID (generic) for information on extracing the parts of a JID from a string) is as follows:

[![JID validation flowchart][1]][1]

The `Validations` step should perform the following:

- Check that the localpart is less than 1024 bytes (bytes, not glyphs)
- Check that the localpart doesnot contain any of `"&'/:<>@`
- Check that the resourcepart is less than 1024 bytes
- Check that the domainpart is greater than zero bytes and less than 1024 bytes (and possibly validate that the individual parts of the domain fit into DNS requirements)
- If the domain is a valid IPv6 address, ensrue that it uses bracketed notation (eg. `[::1]` instead of `::1`)

[rfc7564]: https://tools.ietf.org/html/rfc7564


  [1]: http://i.stack.imgur.com/Ph9yK.png

## Splitting a JID (Go)
The [`mellium.im/xmpp/jid`][1] package implements operations on JIDs. To split a JID string into its component parts the `SplitString` function may be used:

```
lp, dp, rp, err := SplitString("romeo@example.net")
``` 

No validation is performed by the function and the parts are not guaranteed to be valid.

To manually split a string without depending on the `jid` package, the underlying code looks like this:

```
// SplitString splits out the localpart, domainpart, and resourcepart from a
// string representation of a JID. The parts are not guaranteed to be valid, and
// each part must be 1023 bytes or less.
func SplitString(s string) (localpart, domainpart, resourcepart string, err error) {

    // RFC 7622 §3.1.  Fundamentals:
    //
    //    Implementation Note: When dividing a JID into its component parts,
    //    an implementation needs to match the separator characters '@' and
    //    '/' before applying any transformation algorithms, which might
    //    decompose certain Unicode code points to the separator characters.
    //
    // so let's do that now. First we'll parse the domainpart using the rules
    // defined in §3.2:
    //
    //    The domainpart of a JID is the portion that remains once the
    //    following parsing steps are taken:
    //
    //    1.  Remove any portion from the first '/' character to the end of the
    //        string (if there is a '/' character present).
    sep := strings.Index(s, "/")

    if sep == -1 {
        sep = len(s)
        resourcepart = ""
    } else {
        // If the resource part exists, make sure it isn't empty.
        if sep == len(s)-1 {
            err = errors.New("The resourcepart must be larger than 0 bytes")
            return
        }
        resourcepart = s[sep+1:]
        s = s[:sep]
    }

    //    2.  Remove any portion from the beginning of the string to the first
    //        '@' character (if there is an '@' character present).

    sep = strings.Index(s, "@")

    switch sep {
    case -1:
        // There is no @ sign, and therefore no localpart.
        localpart = ""
        domainpart = s
    case 0:
        // The JID starts with an @ sign (invalid empty localpart)
        err = errors.New("The localpart must be larger than 0 bytes")
        return
    default:
        domainpart = s[sep+1:]
        localpart = s[:sep]
    }

    // We'll throw out any trailing dots on domainparts, since they're ignored:
    //
    //    If the domainpart includes a final character considered to be a label
    //    separator (dot) by [RFC1034], this character MUST be stripped from
    //    the domainpart before the JID of which it is a part is used for the
    //    purpose of routing an XML stanza, comparing against another JID, or
    //    constructing an XMPP URI or IRI [RFC5122].  In particular, such a
    //    character MUST be stripped before any other canonicalization steps
    //    are taken.

    domainpart = strings.TrimSuffix(domainpart, ".")

    return
}
```


  [1]: https://godoc.org/mellium.im/xmpp/jid

## Splitting a JID (Rust)
In Rust the [`xmpp-addr`] ([docs]) crate can be used to manipulate JIDs.
To split a JID into its component parts (without validating that those parts are valid), the [`Jid::split`] function may be used:


<!-- language: rust -->
    let (lp, dp, rp) = Jid::split("feste@example.net")?;
    assert_eq!(lp, Some("feste"));
    assert_eq!(dp, "example.net");
    assert_eq!(rp, None);

[`xmpp-addr`]: https://crates.io/crates/xmpp-addr
[docs]: https://docs.rs/xmpp-addr
[`Jid::split`]: https://docs.rs/xmpp-addr/0.11.1/xmpp_addr/struct.Jid.html#method.split

