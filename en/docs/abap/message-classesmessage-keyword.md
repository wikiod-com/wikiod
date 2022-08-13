---
title: "Message ClassesMESSAGE keyword"
slug: "message-classesmessage-keyword"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

The `MESSAGE` statement may be used to interrupt program flow to display short messages to the user. Messages content may be defined in the program's code, in the program's text symbols, or in an independent message class defined in `SE91`.

The maximum length of a message, including parameters passed to it using `&`, is 72 characters.

## Defining a Message Class
`PROGRAM zprogram MESSAGE-ID sabapdemos.`

System-defined message may be stored in a message class. The `MESSAGE-ID` token defines the message class `sabapdemos` for the entire program. If this is not used, the message class must be specified on each `MESSAGE` call.

## MESSAGE with Predefined Text Symbol
    PROGRAM zprogram MESSAGE-ID za.
    ...
    MESSAGE i000 WITH TEXT-i00.

A message will display the text stored in the text symbol `i00` to the user. Since the message type is i (as seen in `i000`), after the user exits the dialog box, program flow will continue from the point of the `MESSAGE` call.

Although the text did not come from the message class `za`, a `MESSAGE-ID` must be specified.

## Message without Predefined Message Class
    PROGRAM zprogram.
    ...
    MESSAGE i050(sabapdemos).

It may be inconvenient to define a message class for the entire program, so it is possible to define the message class that the message comes from in the `MESSAGE` statement itself. This example will display message `050` from the message class `sabapdemos`.

## Dynamic Messaging
    DATA: msgid TYPE sy-msgid VALUE 'SABAPDEMOS', 
          msgty TYPE sy-msgty VALUE 'I', 
          msgno TYPE sy-msgno VALUE '050'. 

    MESSAGE ID mid TYPE mtype NUMBER num. 

The `MESSAGE` call above is synonymous to the call `MESSAGE i050(sapdemos).`.

## Passing Parameters to Messages
The `&` symbol may be used in a message to allow parameters to be passed to it.

---

**Ordered Parameters**

Message `777` of class `sabapdemos`:

    Message with type &1 &2 in event &3


Calling this message with three parameters will return a message using the parameters:

    MESSAGE i050(sabapdemos) WITH 'E' '010' 'START-OF-SELECTION`.

This message will be  displayed as `Message with type E 010 in event START-OF-SELECTION`. The number next to the `&` symbol designates the order in which the parameters are displayed.

---

**Unordered Parameters**

Message `888` of class `sabapdemos`:

    & & & &

The calling of this message is similar:

    MESSAGE i050(sabapdemos) WITH 'param1' 'param2' 'param3' 'param4'.

This will output `param1 param2 param3 param4`.

