---
title: "Parsing command line arguments"
slug: "parsing-command-line-arguments"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Passing action (verb) and values
<!-- language: lang-js -->
    const options = require("commander");
    
    options
      .option("-v, --verbose", "Be verbose");
    
    options
      .command("convert")
      .alias("c")
      .description("Converts input file to output file")
      .option("-i, --in-file <file_name>", "Input file")
      .option("-o, --out-file <file_name>", "Output file")
      .action(doConvert);
    
    options.parse(process.argv);
    
    if (!options.args.length) options.help();
    
    function doConvert(options){
        //do something with options.inFile and options.outFile
    };


## Passing boolean switches
    const options = require("commander");
    
    options
      .option("-v, --verbose")
      .parse(process.argv);
    
    if (options.verbose){
      console.log("Let's make some noise!");
    }



