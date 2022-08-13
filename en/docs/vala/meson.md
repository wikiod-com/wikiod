---
title: "Meson"
slug: "meson"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Meson is a next-generation build system designed with simplicity and explicitness in mind.

## Basic project
<!-- language: lang-meson -->

    project('Vala Project')
    
    glib_dep = dependency('glib-2.0')
    gobject_dep = dependency('gobject-2.0')
    
    executable('foo', 'foo.vala', dependencies: [glib_dep, gobject_dep])

Note: both `glib-2.0` and `gobject-2.0` dependencies are required unless `--nostdpkg` is explicitly given.

## Posix-based project (no GLib or GObject)
<!-- language: lang-vala -->

    project('Posix-based Project', 'vala')
    
    add_project_arguments(['--nostdpkg'], language: 'vala')
    
    posix_dep = meson.get_compiler('vala').find_library('posix')
    
    executable('foo', 'foo.vala', dependencies: [posix_dep])

## Mixed sources
<!-- language: lang-vala -->

    project('Mixed sources Project', 'vala')
    
    glib_dep = dependency('glib-2.0')
    gobject_dep = dependency('gobject-2.0')
    
    executable('foo', 'foo.vala', 'bar.c', dependencies: [glib_dep, gobject_dep])

In `foo.vala`:

<!-- language: lang-vala -->

    namespace Foo {
        public extern int bar ();
    
        public int main (string[] args) {
            return bar ();
        }
    }

In `bar.c`:

<!-- language: lang-c -->

    int 
    bar () 
    {
        return 0;
    }

