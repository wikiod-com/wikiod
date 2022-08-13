---
title: "Font-extensions"
slug: "font-extensions"
draft: false
images: []
weight: 9708
type: docs
toc: true
---

## Creating and using font extensions
Create a font extension using the IDE. See the [iReport][1] or [Jaspersoft Studio][2] documentation for details. The font extension can also be [created manually][3].

# What are font extensions?

Using a `textElement` you can specify a font (if not specified default font `SansSerif` is used)

    <textElement>
        <font fontName="DejaVu Sans"/>
    </textElement>

To calculate font-metric (for line breaks, alignment etc) and render the font correctly, the **font** needs to be **mapped in the JVM** (Java virtual macchine). You could install the font file directly to the JVM but this is not encourage 

From the JasperReport Ultimate Guide:
> We strongly encourage people to use only fonts derived from font
> extensions, because this is the only way to make sure that the fonts
> will be available to the application when the reports are executed at
> runtime. Using system fonts always brings the risk for the reports not
> to work properly when deployed on a new machine that might not have
> those fonts installed


# Default font extension

JasperReports provide a default font-extension (see maven distribution jasperreports-fonts.jar). Adding this to classpath you can use the following fontName's without creating your own font-extension

> DejaVu Sans<br/> DejaVu Serif<br/> DejaVu Sans Mono

# Common Issues

Issues to consider when using font's in pdf (itext):

* When exporting to PDF, if the text is not rendered correctly (missing parts, characters not showed, not wrapping or sized correctly), the **font-extensions** are likely missing.

* Is the actual `.tff` **supported** ([OpenType][2]) and can the font actually **render** the character? Not all fonts render all characters in `UTF-8`.

* Is the **correct encoding** passed to iText? In doubts (or in general) use the **encoding `Identity-H`** this is recommend for newer PDF standards and gives you the ability to mix different encoding.

* Is the font **embedded** so that a PDF shared across computers can display the content even if the font is not installed? If the font is not one of the [14 Standard Type 1 fonts][4] always embed it.

Note the version of iText used by jasper report will not render all fonts ([ligaturizer problem][5]), You can test the `ttf` font and encoding directly see http://stackoverflow.com/questions/35127956/how-can-i-test-if-my-font-is-rendered-correctly-in-pdf 


  [1]: http://community.jaspersoft.com/wiki/ireport-fonts
  [2]: http://community.jaspersoft.com/wiki/custom-font-font-extension
  [3]: http://jasperreports.sourceforge.net/sample.reference/fonts/
  [4]: http://stackoverflow.com/questions/27344944/what-s-difference-between-font-is-embedd-fonts-and-nonembedd-font/27345103#27345103
  [5]: http://stackoverflow.com/questions/36655923/why-is-the-gujarati-indian-text-not-rendered-correctly-using-arial-unicode-ms

