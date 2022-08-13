---
title: "Readline"
slug: "readline"
draft: false
images: []
weight: 9889
type: docs
toc: true
---

## Syntax
 - const readline = require('readline')
 - readline.close()
 - readline.pause()
 - readline.prompt([preserveCursor])
 - readline.question(query, callback)
 - readline.resume()
 - readline.setPrompt(prompt)
 - readline.write(data[, key])
 - readline.clearLine(stream, dir)
 - readline.clearScreenDown(stream)
 - readline.createInterface(options)
 - readline.cursorTo(stream, x, y)
 - readline.emitKeypressEvents(stream[, interface])
 - readline.moveCursor(stream, dx, dy)

## Line-by-line file reading
    const fs = require('fs');
    const readline = require('readline');
    
    const rl = readline.createInterface({
        input: fs.createReadStream('text.txt')
    });
    
    // Each new line emits an event - every time the stream receives \r, \n, or \r\n
    rl.on('line', (line) => {
        console.log(line);
    });
    
    rl.on('close', () => {
        console.log('Done reading file');
    });

## Prompting user input via CLI
    const readline = require('readline');
    
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });
    
    rl.question('What is your name?', (name) => {
        console.log(`Hello ${name}!`);

        rl.close();
    });



