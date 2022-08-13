---
title: "ANTLR TargetsLanguage Runtimes"
slug: "antlr-targetslanguage-runtimes"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Language Support
ANTLR is capable of generating parsers for a number of programming languages:

 1. C# Target
 2. Python Target
 3. JavaScript Target
 4. Java Target

By default ANTLR will generate a parser from commandline in the Java programming language :

    Java -jar antlr-4.5.3-complete.jar yourGrammar.g4 //Will output a
        java parser
To change the target language you can run the following command from the OS terminal/commandline:

        antlr4 -Dlanguage=Python3 yourGrammar.g4 
    //with alias
        java -jar antlr-4.5.3-complete.jar -Dlanguage=Python3 yourGrammar.g4 
    //without alias

Rather than use the '-Dlanguage' parameter on the commandline/terminal each time to build your desired parser for a specific language you can select the target from within your .g4 grammar file by including the target within the global section:

    options {
        language  = "CSharp";
    }
    //or
    options {
        language="Python";
    }

To use the generated parser output make sure you have the ANTLR runtime for the specified language :

 1. [CSharp runtime][1]
 2. [Python 2 runtime][2]
 3. [python 3 runtime][3]


[Full instructions and information on ANTLR run-times libraries][4]


  [1]: http://www.antlr.org/download/antlr-csharp-runtime-4.5.3.zip
  [2]: https://pypi.python.org/pypi/antlr4-python2-runtime
  [3]: https://pypi.python.org/pypi/antlr4-python3-runtime
  [4]: http://www.antlr.org/download.html

## Python parser setup
After running your grammar .g4 file with ANTLR.jar you should have a number of files generated such as :

    1.yourGrammarNameListener.py
    2.yourGrammarNameParser.py
    3.yourGrammarName.tokens
    ...

To use these in a python project include the Python runtime in your workspace so any application you are developing can access the ANTLR library. This can be done by extracting the runtime into your current project folder or importing it within your IDE into your project dependencies.

    #main.py
    import yourGrammarNameParser
    import sys
    
    #main method and entry point of application
    
    def main(argv):
        """Main method calling a single debugger for an input script"""
        parser = yourGrammarNameParser
        parser.parse(argv)
    
    if __name__ == '__main__':
        main(sys.argv) 

This setup includes your parser and accepts input from commandline to allow processing of a file passed as a parameter.

    #yourGrammarNameParser.py
    from yourGrammarNameLexer import yourGrammarNameLexer
    from yourGrammarNameListener import yourGrammarNameListener
    from yourGrammarNameParser import yourGrammarNameParser
    from antlr4 import *
    import sys
    
    class yourGrammarNameParser(object):
        """
        Debugger class - accepts a single input script and processes
        all subsequent requirements
        """
    def __init__(self): # this method creates the class object.
        pass
            
            
    #function used to parse an input file
    def parse(argv):
        if len(sys.argv) > 1:
            input = FileStream(argv[1]) #read the first argument as a filestream
            lexer = yourGrammarNameLexer(input) #call your lexer
            stream = CommonTokenStream(lexer)
            parser = yourGrammarNameParser(stream)
            tree = parser.program() #start from the parser rule, however should be changed to your entry rule for your specific grammar.
            printer = yourGrammarNameListener(tree,input)
            walker = ParseTreeWalker()
            walker.walk(printer, tree)
        else:
            print('Error : Expected a valid file')

These files coupled with the ANTLR runtime and your files generated from your grammar file will accept a single filename as an argument and read and parse your grammar rules.

To extend the basic functionality you should also expand on the default listener to handle relevant events for tokens that are encountered during runtime.

