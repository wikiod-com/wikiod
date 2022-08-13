---
title: "Getting system font names and using the fonts"
slug: "getting-system-font-names-and-using-the-fonts"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

The following examples show how to retrieve the default names of the system fonts that are store in the _/system/fonts/_ directory and how to use a system font to set the typeface of a `TextView` element.

## Getting system font names
    ArrayList<String> fontNames = new ArrayList<String>();
    File temp = new File("/system/fonts/");
    String fontSuffix = ".ttf";

    for(File font : temp.listFiles()) {
        String fontName = font.getName();
        if(fontName.endsWith(fontSuffix)) {
            fontNames.add(fontName.subSequence(0,fontName.lastIndexOf(fontSuffix)).toString());
        }
    }



## Applying a system font to a TextView
In the following code you need to replace `fontsname` by the name of the font you would like to use:

    TextView lblexample = (TextView) findViewById(R.id.lblexample);
    lblexample.setTypeface(Typeface.createFromFile("/system/fonts/" + "fontsname" + ".ttf"));

