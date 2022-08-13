---
title: "Magics"
slug: "magics"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

IPython extends Python with a set of commands called _magics_. These are special functions whose names start with a `%` that are recognized by the IPython shell. Magics whose name begins with just one `%` take as argument the rest of the line and are called _line magics_. Magics that begin with a double percent sign `%%` take a multi-line argument and are called _cell magics_. 

## The %timeit and `%time` magics
The `%timeit` magic runs the given code many times, then returns the speed of the fastest result.

    In [1]: %timeit sum(range(100000))
    100 loops, best of 3: 2.91 ms per loop

The `%%timeit` cell magic can be used to time blocks of code.

    In [2]: %%timeit
       ...: a = 0
       ...: for i in range(100000):
       ...:     a += i
       ...:
    100 loops, best of 3: 9.67 ms per loop

The `%time` magic times a single run of a function, similar to the Unix `time` command. Unlike `%timeit`, `%time` also shows the result.

    In [3]: %time sum(range(100000))
    CPU times: user 2.68 ms, sys: 3 µs, total: 2.68 ms
    Wall time: 2.69 ms
    Out[3]: 4999950000

## Built-in line and cell magics
Magics whose name begins with just one `%` take as argument the rest of the line and are called _line magics_. Magics that begin with a double percent sign `%%` take a multi-line argument and are called _cell magics_. 

A commonly used magic is [`%timeit`][1], a wrapper around the Python's `timeit.timeit` function, for measuring the execution time of a piece of code.


    In [35]: ra = [random.randint(0,1000) for r in range(1000)]
    In [35]: %timeit sorted(ra)
    1000 loops, best of 3: 507 µs per loop

An example of cell magic is `%%bash` (equivalent to `%%script bash`) for running the input as bash code

    In [49]: %%bash
        ...: i=3
        ...: while [ $i -ge 0 ]
        ...: do
        ...: echo $i
        ...: i=$(($i-1))
        ...: done
        ...:
    3
    2
    1
    0

Note that the end of a cell is marked by an empty line.

To view all built-in magics use `%lsmagic`
 
    In [51]: %lsmagic
    Out[51]:
    Available line magics:
    %alias  %alias_magic  %autocall  %autoindent  %automagic  %bookmark  %cd  %cls
    %colors  %config  %copy  %cpaste  %ddir  %debug  %dhist  %dirs  %doctest_mode  
    %echo  %ed  %edit  %env  %gui  %hist  %history  %killbgscripts  %ldir  %load  
    %load_ext  %loadpy  %logoff  %logon  %logstart  %logstate  %logstop  %ls  %lsmagic
    %macro  %magic  %matplotlib  %mkdir  %notebook  %page  %paste  %pastebin  %pdb
    %pdef  %pdoc  %pfile  %pinfo  %pinfo2  %popd  %pprint  %precision  %profile  
    %prun  %psearch  %psource  %pushd  %pwd  %pycat  %pylab  %quickref  %recall  
    %rehashx  %reload_ext  %ren  %rep  %rerun  %reset  %reset_selective  %rmdir  %run  
    %save  %sc  %set_env  %store  %sx  %system  %tb  %time  %timeit  %unalias  
    %unload_ext  %who  %who_ls  %whos  %xdel  %xmode

    Available cell magics:
    %%!  %%HTML  %%SVG  %%bash  %%capture  %%cmd  %%debug  %%file  %%html  
    %%javascript  %%js  %%latex  %%perl  %%prun  %%pypy  %%python  %%python2  %%python3  
    %%ruby  %%script  %%sh  %%svg  %%sx  %%system  %%time  %%timeit  %%writefile

    Automagic is ON, % prefix IS NOT needed for line magics.

Note that by default 'automagic' is on, and therefore line magics can be called without `%` prefix (but cell functions still need a `%%` prefix).


  [1]: https://www.wikiod.com/ipython/magics#The %timeit and `%time` magics

