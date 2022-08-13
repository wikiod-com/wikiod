---
title: "How to find the right person for help."
slug: "how-to-find-the-right-person-for-help"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

This should mirror some of the official Linux kernel docs, and post links to the latest versions of said documents in [`tovalds/linux`](https://github.com/torvalds/linux) on GitHub.com. The idea is to encourage individuals to make use of the `MAINTAINERS` files, `linux-kernel` mailing list, `git log`, and [`scripts/get-maintainer`](https://github.com/torvalds/linux/blob/master/scripts/get_maintainer.pl), so that they are familiar with the commonly-used ways of identifying a key point of contact.

## Find the "likely" maintainers for the FTDI USB serial converter
First, determine the source file for this particular driver.
Found it at `drivers/usb/serial/ftdi_sio.c`.

    ./scripts/get_maintainer.pl drivers/usb/serial/ftdi_sio.c 

And the results:

    Johan Hovold <johan@kernel.org> (maintainer:USB SERIAL SUBSYSTEM)
    Greg Kroah-Hartman <gregkh@linuxfoundation.org> (supporter:USB SUBSYSTEM)
    linux-usb@vger.kernel.org (open list:USB SERIAL SUBSYSTEM)
    linux-kernel@vger.kernel.org (open list)

Now we know who to ping for help with this particular driver, and which e-mail addresses should be CC'ed when submitting a patch against this driver.



