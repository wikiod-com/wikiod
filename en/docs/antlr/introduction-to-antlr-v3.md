---
title: "Introduction to ANTLR v3"
slug: "introduction-to-antlr-v3"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Installation and Setup
How To Install ANTLR in Eclipse
-------------------------------

(Last tested on Indigo and ANTLR IDE 2.1.2)

 1. Install Eclipse.
 2. Download [ANTLR complete binaries jar that includes ANTLR v2.][1] Extract to a temp directory. Copy the antlr-n.n
    folder to an appropriate permanent location, for example the same
    folder that Eclipse is installed in.
 3. Add ANTLR IDE update site to
    Eclipse.
    - In Eclipse, click on Help and select Install New Software.
    - Click Add… button.
    - In the Add Repository window, for Location type http://antlrv3ide.sourceforge.net/updates and type something like ANTLR IDE for the Name and click OK to get back to the Available Software window.
    - Check the box for ANTLR IDE vn.n.n and click on through until it is installed. Eclipse will probably restart.
 4. Configure the ANTLR IDE.
    - In the Eclipse main window, click Window then Preferences.
    - In the left pane, expand ANTLR and select Builder.
    - In the right pane, click the Add… button.
    - In the Add ANTLR Package window, click Directory… and navigate to the location of the antlr-n.n folder and click OK.
    - Click OK to close the Add ANTLR Package window.
    - Select Code Generator in the left pane and click on Project relative folder in the right pane. Type a folder name. Examples: antlr-java or antlr-generated.
    - Select any other configuration parameters but DO NOT check –nfa or –dfa in the under General in the Building window. If checked, these will cause ANTLR errors preventing java files from being generated in the output folder. 
    - Click OK to close the Preferences window.
 5. Create a new Java project and enable ANTLR support.
    - From the Eclipse main window, go to File, New, Java Project. Click Next, type a project name and click Finish.
    - To enable ANTLR support for the project, in the Package Explorer window (left pane) right-click the project just created and select Configure, Convert to ANTLR project.
    - Add the ANTLR complete jar file to the project: right-click the project and select Properties, Java Build Path, click Add External JARs…, browse to the ANTLR jar file, select it, and click OK. Click OK to close the project Properties window.
 6. Create an ANTLR grammar.
    - Create a new ANTLR grammar: right-click the src folder of the project, then File, New, Other, expand ANTLR and select Combined Grammar. Click Next, type grammar name, select a Language option, and click Finish. 
    - A “.g” file is created with the options selected and a blank rule. Add the options language=Java, @header, @lexer::header, and @members statements at the top (see example). Auto completion is the easiest way to add these (press CTRL-space to bring up auto-completion list).
 7. Save the grammar.
    - When saved, a folder containing generated Java code for the grammar should appear in the Project Explorer. If it does not, make sure the –nfa or –dfa options are not checked in ANTLR Preferences under General in the Building window (Step 4g). [Confirm if these needed: check CLASSPATH environment variable points to the Java7 that matches your Eclipse install (32 or 64 bits) and Windows Path environment variable had Java7 SDK.]
    - To avoid “cannot be resolved to a type” Java errors, right-click the folder containing generated Java code, then Build Path, Use as a Source Folder.

SAMPLE COMBINED GRAMMAR

    grammar test; //must match filename.g
    
    options {
        language = Java;
    }

    @header { //parser
        package pkgName; //optional
        import java.<whatever you need>.*;
    }
    
    @members { //parser
        // java code here
    }
    
    @lexer::header { //lexer
        package pkgName; //optional
        import java.<whatever you need>.*;
    }
    
    @lexer::members {
        // java code here
    }
    /*------------------------------------------------------------------
     * PARSER RULES (convention is all lowercase)
     *------------------------------------------------------------------*/
    parserule: LEXRULE;
    
    /*------------------------------------------------------------------
     * LEXER RULES (convention is all uppercase)
     *------------------------------------------------------------------*/
    LEXRULE: 'a'..'z';


  [1]: http://www.antlr.org/download/antlr-3.4-complete.jar

