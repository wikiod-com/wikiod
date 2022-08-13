---
title: "Random In Batch Files"
slug: "random-in-batch-files"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Random Numbers
Using the dynamic variable `%Random%`, we can get a random integer from 0 to 32767. For example:


    echo %random%

This obviously, returns an integer from 0 to 32767. But sometimes we want it to be in a specific range, say from 1 to 100.

---

## Generating Random Numbers Within Specific Range

The basic method to do so is listed below.

<pre><code>set /a result=(%RANDOM%*<b>max</b>/32768)+<b>min</b></code></pre>

where `max` is the top number that can be generated, and `min` is the smallest number that can be generated. Note that you will not get any decimal numbers because `set /a` rounds down automatically. To generate a decimal random number, try this:

<pre><code>set /a whole=(%RANDOM%*<b>max</b>/32768)+<b>min</b>
set /a decimal=(%RANDOM%*<b>max</b>/32768)+<b>min</b>
echo %whole%.%decimal%</code></pre>

---

## Generating Random Numbers larger than 32767

If you try 
 
    set /a whole=(%RANDOM%*65536/32768)+1

you will most likely get random numbers that are odd.

To generate numbers larger than 32767, here is a better method.

    set /a result=%random:~-1%%random:~-1%%random:~-1%%random:~-1%%random:~-1%%random:~-1%

The previous code extracts the 1 character from each `%random%`. But this is done on purpose.

Since the `random` number could be one digit number, extracting the last 2 digit won't work. That's why we extract only the last character. In this case, we have 6 `%random:~-1%`, generating the maximum of `999999`, and the minimum at `000000`, you may need to adjust this to suit your needs.

## Pseudorandom

`cmd.exe` generate the seed based on the time the `cmd` section started, so if you start mutliple section at the nearly same time, the result may not be 'random' enough.

## Random Alphabets
Unfortunately, batch does not have a built-in method to generate alphabets, but using `%random%` and `for` loop, we can 'generate' alphabets.

---

This is a simple idea of how this works.

    set /a result=(%random%*26/32768)+1
    for /f "tokens=%result%" %%I in ("A B C D E F G H I J K L M N O P Q R S T U V W X Y Z") do (
        echo %%I
    )

- The first `set /a` statement generate a random number `N` between 1 to 26
- The `for /f` statement picks the `N`th item from a list of A to Z.
  - Return the result

One can put a total `31` items in 1 `for` loop, and practically unlimited items using [this method].(https://stackoverflow.com/questions/43139660/batch-for-loop-parameter-order)

## Pseudorandom And Uniform Random In Batch
# Pseudorandom Distribution

Accorinding to [this Stack Overflow answer](https://stackoverflow.com/questions/5777400/how-to-use-random-in-batch-script/5777608#5777608), user CherryDT pointed out this code:

   
    set /a num=%random% %% 100

does not give a uniform distribution.

   The internal dynamic variable `%random%` **does** gives a **uniform distribution**, but the above code will not be a uniformed random. This code generates a random number between 0 ~ 99, but the result will not be uniform. 0 ~ 67 will occur more than 68 ~ 99 since `32767 MOD 100` = `67`. 

To generate a uniform distributed random using the above code, then `100` must be changed. Here is a method to get a number that creates a uniform distribution.

    32767 mod (32767 / n)

where `n` is an integer, between 0  ~ 32767, the result may be decimal and may not work in batch.

---

# Uniform Distribution

    set /a result=(%RANDOM%*100/32768)+1

This method will generate a uniform distribution. It avoids using `%`, which is more like "remainder" then "modulus" in a batch script. Without using `%`, the result will be uniform.

Alternatively, here is an inefficient, but uniform method.

    set /a test=%random%

    if %test% geq [yourMinNumber] (
        if %test% leq [yourMaxNumber] (

            rem do something with your random number that is in the range.

       )
    )

Change `[yourMinNumber]` and `[yourMaxNumber]` accordingly to your own values.


