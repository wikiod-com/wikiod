---
title: "GUI Applications in Perl"
slug: "gui-applications-in-perl"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Tk is one of the most commonly used GUI toolkits for Perl. Other common toolkits are GTK+2 & 3, WxWidgets, and Win32 widgets. Less commonly used are Qt4, XUL, Prima, and FLTK.

Tk, GTK+3, Wx, Win32, Prima, FLTK, and XUL are actively updated. Qt4 and GTK+2 are no longer developed actively, but may have maintenance releases.

## GTK Application
    use strict;
    use warnings;
    
    use Gtk2 -init;
    
    my $window = Gtk2::Window->new();
    $window->show();
    
    Gtk2->main();
    
    0;

