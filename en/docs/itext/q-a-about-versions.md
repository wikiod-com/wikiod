---
title: "Q & A about versions"
slug: "q--a-about-versions"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

Some frequently asked questions about the version numbers in iText.


**Why do the version numers jump from 2 to 5, and from 5 to 7?** There are several reasons for skipping version numbers. In 2009, the version number of iText (Java) and iTextSharp (C#) were not in sync. The Java version was at version 2.1.7; the C# version was at version 4.1.6. A decision was taken to move to Java 5 for the Java version and to harmonize the version numbers of iText and iTextSharp.

**What's the deal with iText 4?** Third parties have released iText 4.2.z versions, but those aren't official versions. They aren't supported by anyone and shouldn't be used because no one really knows what's inside.

**Is iText 7 backward compatible?** iText 7 is a totally new version of iText, written from scratch by the iText team. Backward compatibility is broken in favor of a more intuitive interface. The Java version of iText has moved from Java 5 to Java 7, which was one of the reasons to jump from iText 5 to iText 7.

## itext 2 vs iText 5 vs iText 7
# iText 2 and earlier:
import com.lowagie.text.*;

# iText 5:
import com.itextpdf.text.*;

# iText 7:
import com.itextpdf.kernel.*;

import com.itextpdf.layout.*;

...

