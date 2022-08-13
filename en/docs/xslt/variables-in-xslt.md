---
title: "Variables in xslt"
slug: "variables-in-xslt"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Xslt Variables
**Global Variable:** This variable is available everywhere in the xsl stylesheet. This variable should only be the child of &lt;xsl:stylesheet&gt; element.

**Local variable:** This variable is only available where it is declared.

See Below Code:

    <?xml version="1.0" encoding="UTF-8"?> 
    <xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                     xmlns:xs="http://www.w3.org/2001/XMLSchema"
                     exclude-result-prefixes="xs"
                     version="2.0">
      <xsl:output omit-xml-declaration="yes"/>

      <xsl:variable name="a" select="5"/>     <!-- Global Variable -->

      <xsl:template match="/">
         <xsl:variable name="b" select="2"/> <!--Local Variable -->
         <xsl:value-of select="$a+$b"/>      <!--Addition of 'a' and 'b' -->
      </xsl:template> 
    </xsl:stylesheet>

The Output of the above code would be: **7**

There are two ways to define a value to the variable like:

By xpath expression in the **@select attribute of <xsl:variable> element** like:

    <xsl:variable name="apple" select="'RED'"/>

OR

By the **content of the <xsl:variable> element** like:

    <xsl:variable name="apple">RED</xsl:variable>

To call a declared variable use **$** Sign with variable name, like in above code '$a'

