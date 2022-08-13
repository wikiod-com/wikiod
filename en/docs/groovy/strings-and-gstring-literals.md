---
title: "Strings and GString literals"
slug: "strings-and-gstring-literals"
draft: false
images: []
weight: 9961
type: docs
toc: true
---

## Syntax
- 'Single quoted string'
- "Double quoted string"
- '''Multiline string'''
- """Triple double quoted string"""
- /Slashy string/
- $/Dollar slash string/$

Groovy has two string types the java `java.lang.String` and `groovy.lang.GString`, as well as multiple forms of string literals (see syntax and examples).

The main difference between the two types of strings is that GString supports string interpolation.

## Dollar slash string
    def param = 'string'
    def str = $/
    multiline $param
    no need to escape slash
    \n
    $
    $$
    /$
    assert str instanceof GString
    assert str.readLines().size() == 6
    assert str.contains('\\n')
    assert str.contains('$')

## Single quoted string
    def str = 'Single quoted string'
    assert str instanceof String

## Double quoted string (without interpolation placeholder)
    def str = "Double quoted string"
    assert str instanceof String

## Double quoted string (interpolation)
    def param = 'string'
    def str = "Double quoted ${param}"
    assert str instanceof GString
    assert str == 'Double quoted string'

The parameter is by default resolved eagerly, this means this applies:

    def param = 'string'
    def str = "Double quoted ${param}"
    param = 'another string'
    assert str == 'Double quoted string'

In order to load the parameter lazily every time the string is used, this can be done:

    def param = 'string'
    def str = "Double quoted ${ -> param}"
    assert str == 'Double quoted string'
    param = 'lazy load'
    assert str == 'Double quoted lazy load'

## Multiline string
    def str = '''multiline 
    string'''
    assert str instanceof String

## Multiline string (extra trailing newline)
    def str = '''
    multiline 
    string'''
    assert str.readLines().size() == 3


## Multiline string (without extra trailing newline)
    def str = '''\
    multiline 
    string'''
    assert str.readLines().size() == 2


## Triple double quoted string
    def param = 'string'
    def str = """
    multiline
    $param
    """
    assert str instanceof GString
    assert str.readLines().size() == 3
    assert str == '''
    multiline
    string
    '''


## Slashy string (no interpolation placeholder)
    def str = /
    multiline string
    no need to escape slash
    \n
    /
    assert str instanceof String
    assert str.readLines().size() == 4
    assert str.contains('\\n')

## Slashy string (interpolation)
    def param = 'string'
    def str = /
    multiline $param
    no need to escape slash
    \n
    /
    assert str instanceof GString
    assert str.readLines().size() == 4
    assert str.contains('\\n')
    assert str.contains('string')

