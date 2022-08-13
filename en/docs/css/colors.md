---
title: "Colors"
slug: "colors"
draft: false
images: []
weight: 9764
type: docs
toc: true
---

## Syntax
- color: #rgb
- color: #rrggbb
- color: rgb[a](<red\>, <green\>, <blue\>[, <alpha\>])
- color: hsl[a](<hue\>, <saturation%\>, <lightness%>[, <alpha\>])
- color: [colorkeyword](https://www.wikiod.com/css/colors#Color Keywords) /* green, blue, yellow, orange, red, ..etc */

## currentColor
`currentColor` returns the computed color value of the current element.

## Use in same element ##

Here currentColor evaluates to red since the ``color`` property is set to ``red``:

<!-- language-all: lang-css -->

    div {
       color: red;     
       border: 5px solid currentColor;
       box-shadow: 0 0 5px currentColor;
    }

In this case, specifying currentColor for the border is most likely redundant because omitting it should produce identical results. Only use currentColor inside the border property within the same element if it would be overwritten otherwise due to a [more specific](https://www.wikiod.com/css/cascading-and-specificity#Calculating Selector Specificity) selector.

Since it's the computed color, the border will be green in the following example due to the second rule overriding the first:

```
div {
   color: blue;
   border: 3px solid currentColor; 
   color: green;
}
```

## Inherited from parent element
The parent's color is inherited, here currentColor evaluates to 'blue', making the child element's border-color blue.

```
.parent-class {
    color: blue;
}

.parent-class .child-class {
    border-color: currentColor;
}
```

currentColor can also be used by other rules which normally would not inherit from the color property, such as background-color. The example below shows the children using the color set in the parent as its background:

```
.parent-class {
    color: blue;
}

.parent-class .child-class {
    background-color: currentColor;
}
```

**Possible Result:**

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/rkkXo.gif

## Color Keywords
Most browsers support using color keywords to specify a color. For example, to set the `color` of an element to blue, use the `blue` keyword:

```
.some-class {
    color: blue;
}
```

CSS keywords are not case sensitive—`blue`, `Blue` and `BLUE` will all result in `#0000FF`. 

# Color Keywords

|   Color name           | Hex value |    RGB values    |                 Color             |
|------------------------|-----------|------------------|-----------------------------------|
|   AliceBlue            |   #F0F8FF | rgb(240,248,255) | [![AliceBlue][1]][1]              |
|   AntiqueWhite         |   #FAEBD7 | rgb(250,235,215) | [![AntiqueWhite][2]][2]           |
|   Aqua                 |   #00FFFF | rgb(0,255,255)   | [![Aqua][3]][3]                   |
|   Aquamarine           |   #7FFFD4 | rgb(127,255,212) | [![Aquamarine][4]][4]             |
|   Azure                |   #F0FFFF | rgb(240,255,255) | [![Azure][5]][5]                  |
|   Beige                |   #F5F5DC | rgb(245,245,220) | [![Beige][6]][6]                  |
|   Bisque               |   #FFE4C4 | rgb(255,228,196) | [![Bisque][7]][7]                 |
|   Black                |   #000000 | rgb(0,0,0)       | [![Black][8]][8]                  |
|   BlanchedAlmond       |   #FFEBCD | rgb(255,235,205) | [![BlanchedAlmond][9]][9]         |
|   Blue                 |   #0000FF | rgb(0,0,255)     | [![Blue][10]][10]                 |
|   BlueViolet           |   #8A2BE2 | rgb(138,43,226)  | [![BlueViolet][11]][11]           |
|   Brown                |   #A52A2A | rgb(165,42,42)   | [![Brown][12]][12]                |
|   BurlyWood            |   #DEB887 | rgb(222,184,135) | [![BurlyWood][13]][13]            |
|   CadetBlue            |   #5F9EA0 | rgb(95,158,160)  | [![CadetBlue][14]][14]            |
|   Chartreuse           |   #7FFF00 | rgb(127,255,0)   | [![Chartreuse][15]][15]           |
|   Chocolate            |   #D2691E | rgb(210,105,30)  | [![Chocolate][16]][16]            |
|   Coral                |   #FF7F50 | rgb(255,127,80)  | [![Coral][17]][17]                |
|   CornflowerBlue       |   #6495ED | rgb(100,149,237) | [![CornflowerBlue][18]][18]       |
|   Cornsilk             |   #FFF8DC | rgb(255,248,220) | [![Cornsilk][19]][19]             |
|   Crimson              |   #DC143C | rgb(220,20,60)   | [![Crimson][20]][20]              |
|   Cyan                 |   #00FFFF | rgb(0,255,255)   | [![Cyan][21]][21]                 |
|   DarkBlue             |   #00008B | rgb(0,0,139)     | [![DarkBlue][22]][22]             |
|   DarkCyan             |   #008B8B | rgb(0,139,139)   | [![DarkCyan][23]][23]             |
|   DarkGoldenRod        |   #B8860B | rgb(184,134,11)  | [![DarkGoldenRod][24]][24]        |
|   DarkGray             |   #A9A9A9 | rgb(169,169,169) | [![DarkGray][25]][25]             |
|   DarkGrey             |   #A9A9A9 | rgb(169,169,169) | [![DarkGrey][26]][26]             |
|   DarkGreen            |   #006400 | rgb(0,100,0)     | [![DarkGreen][27]][27]            |
|   DarkKhaki            |   #BDB76B | rgb(189,183,107) | [![DarkKhaki][28]][28]            |
|   DarkMagenta          |   #8B008B | rgb(139,0,139)   | [![DarkMagenta][29]][29]          |
|   DarkOliveGreen       |   #556B2F | rgb(85,107,47)   | [![DarkOliveGreen][30]][30]       |
|   DarkOrange           |   #FF8C00 | rgb(255,140,0)   | [![DarkOrange][31]][31]           |
|   DarkOrchid           |   #9932CC | rgb(153,50,204)  | [![DarkOrchid][32]][32]           |
|   DarkRed              |   #8B0000 | rgb(139,0,0)     | [![DarkRed][33]][33]              |
|   DarkSalmon           |   #E9967A | rgb(233,150,122) | [![DarkSalmon][34]][34]           |
|   DarkSeaGreen         |   #8FBC8F | rgb(143,188,143) | [![DarkSeaGreen][35]][35]         |
|   DarkSlateBlue        |   #483D8B | rgb(72,61,139)   | [![DarkSlateBlue][36]][36]        |
|   DarkSlateGray        |   #2F4F4F | rgb(47,79,79)    | [![DarkSlateGray][37]][37]        |
|   DarkSlateGrey        |   #2F4F4F | rgb(47,79,79)    | [![DarkSlateGrey][38]][38]        |
|   DarkTurquoise        |   #00CED1 | rgb(0,206,209)   | [![DarkTurquoise][39]][39]        |
|   DarkViolet           |   #9400D3 | rgb(148,0,211)   | [![DarkViolet][40]][40]           |
|   DeepPink             |   #FF1493 | rgb(255,20,147)  | [![DeepPink][41]][41]             |
|   DeepSkyBlue          |   #00BFFF | rgb(0,191,255)   | [![DeepSkyBlue][42]][42]          |
|   DimGray              |   #696969 | rgb(105,105,105) | [![DimGray][43]][43]              |
|   DimGrey              |   #696969 | rgb(105,105,105) | [![DimGrey][44]][44]              |
|   DodgerBlue           |   #1E90FF | rgb(30,144,255)  | [![DodgerBlue][45]][45]           |
|   FireBrick            |   #B22222 | rgb(178,34,34)   | [![FireBrick][46]][46]            |
|   FloralWhite          |   #FFFAF0 | rgb(255,250,240) | [![FloralWhite][47]][47]          |
|   ForestGreen          |   #228B22 | rgb(34,139,34)   | [![ForestGreen][48]][48]          |
|   Fuchsia              |   #FF00FF | rgb(255,0,255)   | [![Fuchsia][49]][49]              |
|   Gainsboro            |   #DCDCDC | rgb(220,220,220) | [![Gainsboro][50]][50]            |
|   GhostWhite           |   #F8F8FF | rgb(248,248,255) | [![GhostWhite][51]][51]           |
|   Gold                 |   #FFD700 | rgb(255,215,0)   | [![Gold][52]][52]                 |
|   GoldenRod            |   #DAA520 | rgb(218,165,32)  | [![GoldenRod][53]][53]            |
|   Gray                 |   #808080 | rgb(128,128,128) | [![Gray][54]][54]                 |
|   Grey                 |   #808080 | rgb(128,128,128) | [![Grey][55]][55]                 |
|   Green                |   #008000 | rgb(0,128,0)     | [![Green][56]][56]                |
|   GreenYellow          |   #ADFF2F | rgb(173,255,47)  | [![GreenYellow][57]][57]          |
|   HoneyDew             |   #F0FFF0 | rgb(240,255,240) | [![HoneyDew][58]][58]             |
|   HotPink              |   #FF69B4 | rgb(255,105,180) | [![HotPink][59]][59]              |
|   IndianRed            |   #CD5C5C | rgb(205,92,92)   | [![IndianRed][60]][60]            |
|   Indigo               |   #4B0082 | rgb(75,0,130)    | [![Indigo][61]][61]               |
|   Ivory                |   #FFFFF0 | rgb(255,255,240) | [![Ivory][62]][62]                |
|   Khaki                |   #F0E68C | rgb(240,230,140) | [![Khaki][63]][63]                |
|   Lavender             |   #E6E6FA | rgb(230,230,250) | [![Lavender][64]][64]             |
|   LavenderBlush        |   #FFF0F5 | rgb(255,240,245) | [![LavenderBlush][65]][65]        |
|   LawnGreen            |   #7CFC00 | rgb(124,252,0)   | [![LawnGreen][66]][66]            |
|   LemonChiffon         |   #FFFACD | rgb(255,250,205) | [![LemonChiffon][67]][67]         |
|   LightBlue            |   #ADD8E6 | rgb(173,216,230) | [![LightBlue][68]][68]            |
|   LightCoral           |   #F08080 | rgb(240,128,128) | [![LightCoral][69]][69]           |
|   LightCyan            |   #E0FFFF | rgb(224,255,255) | [![LightCyan][70]][70]            |
|   LightGoldenRodYellow |   #FAFAD2 | rgb(250,250,210) | [![LightGoldenRodYellow][71]][71] |
|   LightGray            |   #D3D3D3 | rgb(211,211,211) | [![LightGray][72]][72]            |
|   LightGrey            |   #D3D3D3 | rgb(211,211,211) | [![LightGrey][73]][73]            |
|   LightGreen           |   #90EE90 | rgb(144,238,144) | [![LightGreen][74]][74]           |
|   LightPink            |   #FFB6C1 | rgb(255,182,193) | [![LightPink][75]][75]            |
|   LightSalmon          |   #FFA07A | rgb(255,160,122) | [![LightSalmon][76]][76]          |
|   LightSeaGreen        |   #20B2AA | rgb(32,178,170)  | [![LightSeaGreen][77]][77]        |
|   LightSkyBlue         |   #87CEFA | rgb(135,206,250) | [![LightSkyBlue][78]][78]         |
|   LightSlateGray       |   #778899 | rgb(119,136,153) | [![LightSlateGray][79]][79]       |
|   LightSlateGrey       |   #778899 | rgb(119,136,153) | [![LightSlateGrey][80]][80]       |
|   LightSteelBlue       |   #B0C4DE | rgb(176,196,222) | [![LightSteelBlue][81]][81]       |
|   LightYellow          |   #FFFFE0 | rgb(255,255,224) | [![LightYellow][82]][82]          |
|   Lime                 |   #00FF00 | rgb(0,255,0)     | [![Lime][83]][83]                 |
|   LimeGreen            |   #32CD32 | rgb(50,205,50)   | [![LimeGreen][84]][84]            |
|   Linen                |   #FAF0E6 | rgb(250,240,230) | [![Linen][85]][85]                |
|   Magenta              |   #FF00FF | rgb(255,0,255)   | [![Magenta][86]][86]              |
|   Maroon               |   #800000 | rgb(128,0,0)     | [![Maroon][87]][87]               |
|   MediumAquaMarine     |   #66CDAA | rgb(102,205,170) | [![MediumAquaMarine][88]][88]     |
|   MediumBlue           |   #0000CD | rgb(0,0,205)     | [![MediumBlue][89]][89]           |
|   MediumOrchid         |   #BA55D3 | rgb(186,85,211)  | [![MediumOrchid][90]][90]         |
|   MediumPurple         |   #9370DB | rgb(147,112,219) | [![MediumPurple][91]][91]         |
|   MediumSeaGreen       |   #3CB371 | rgb(60,179,113)  | [![MediumSeaGreen][92]][92]       |
|   MediumSlateBlue      |   #7B68EE | rgb(123,104,238) | [![MediumSlateBlue][93]][93]      |
|   MediumSpringGreen    |   #00FA9A | rgb(0,250,154)   | [![MediumSpringGreen][94]][94]    |
|   MediumTurquoise      |   #48D1CC | rgb(72,209,204)  | [![MediumTurquoise][95]][95]      |
|   MediumVioletRed      |   #C71585 | rgb(199,21,133)  | [![MediumTurquoise][96]][96]      |
|   MidnightBlue         |   #191970 | rgb(25,25,112)   | [![MidnightBlue][97]][97]         |
|   MintCream            |   #F5FFFA | rgb(245,255,250) | [![MintCream][98]][98]            |
|   MistyRose            |   #FFE4E1 | rgb(255,228,225) | [![MistyRose][99]][99]            |
|   Moccasin             |   #FFE4B5 | rgb(255,228,181) | [![Moccasin][100]][100]           |
|   NavajoWhite          |   #FFDEAD | rgb(255,222,173) | [![NavajoWhite][101]][101]        |
|   Navy                 |   #000080 | rgb(0,0,128)     | [![Navy][102]][102]               |
|   OldLace              |   #FDF5E6 | rgb(253,245,230) | [![OldLace][103]][103]            |
|   Olive                |   #808000 | rgb(128,128,0)   | [![Olive][104]][104]              |
|   OliveDrab            |   #6B8E23 | rgb(107,142,35)  | [![OliveDrab][105]][105]          |
|   Orange               |   #FFA500 | rgb(255,165,0)   | [![Orange][106]][106]             |
|   OrangeRed            |   #FF4500 | rgb(255,69,0)    | [![OrangeRed][107]][107]          |
|   Orchid               |   #DA70D6 | rgb(218,112,214) | [![Orchid][108]][108]             |
|   PaleGoldenRod        |   #EEE8AA | rgb(238,232,170) | [![PaleGoldenRod][109]][109]      |
|   PaleGreen            |   #98FB98 | rgb(152,251,152) | [![PaleGreen][110]][110]          |
|   PaleTurquoise        |   #AFEEEE | rgb(175,238,238) | [![PaleTurquoise][111]][111]      |
|   PaleVioletRed        |   #DB7093 | rgb(219,112,147) | [![PaleVioletRed][112]][112]      |
|   PapayaWhip           |   #FFEFD5 | rgb(255,239,213) | [![PapayaWhip][113]][113]         |
|   PeachPuff            |   #FFDAB9 | rgb(255,218,185) | [![PeachPuff][114]][114]          |
|   Peru                 |   #CD853F | rgb(205,133,63)  | [![Peru][115]][115]               |
|   Pink                 |   #FFC0CB | rgb(255,192,203) | [![Pink][116]][116]               |
|   Plum                 |   #DDA0DD | rgb(221,160,221) | [![Plum][117]][117]               |
|   PowderBlue           |   #B0E0E6 | rgb(176,224,230) | [![PowderBlue][118]][118]         |
|   Purple               |   #800080 | rgb(128,0,128)   | [![Purple][119]][119]             |
|   RebeccaPurple        |   #663399 | rgb(102,51,153)  | [![RebeccaPurple][120]][120]      |
|   Red                  |   #FF0000 | rgb(255,0,0)     | [![Red][121]][121]                |
|   RosyBrown            |   #BC8F8F | rgb(188,143,143) | [![RosyBrown][122]][122]          |
|   RoyalBlue            |   #4169E1 | rgb(65,105,225)  | [![RoyalBlue][123]][123]          |
|   SaddleBrown          |   #8B4513 | rgb(139,69,19)   | [![SaddleBrown][124]][124]        |
|   Salmon               |   #FA8072 | rgb(250,128,114) | [![Salmon][125]][125]             |
|   SandyBrown           |   #F4A460 | rgb(244,164,96)  | [![SandyBrown][126]][126]         |
|   SeaGreen             |   #2E8B57 | rgb(46,139,87)   | [![SeaGreen][127]][127]           |
|   SeaShell             |   #FFF5EE | rgb(255,245,238) | [![SeaShell][128]][128]           |
|   Sienna               |   #A0522D | rgb(160,82,45)   | [![Sienna][129]][129]             |
|   Silver               |   #C0C0C0 | rgb(192,192,192) | [![Silver][130]][130]             |
|   SkyBlue              |   #87CEEB | rgb(135,206,235) | [![SkyBlue][131]][131]            |
|   SlateBlue            |   #6A5ACD | rgb(106,90,205)  | [![SlateBlue][132]][132]          |
|   SlateGray            |   #708090 | rgb(112,128,144) | [![SlateGray][133]][133]          |
|   SlateGrey            |   #708090 | rgb(112,128,144) | [![SlateGrey][134]][134]          |
|   Snow                 |   #FFFAFA | rgb(255,250,250) | [![Snow][135]][135]               |
|   SpringGreen          |   #00FF7F | rgb(0,255,127)   | [![SpringGreen][136]][136]        |
|   SteelBlue            |   #4682B4 | rgb(70,130,180)  | [![SteelBlue][137]][137]          |
|   Tan                  |   #D2B48C | rgb(210,180,140) | [![tan][138]][138]                |
|   Teal                 |   #008080 | rgb(0,128,128)   | [![Teal][139]][139]               |
|   Thistle              |   #D8BFD8 | rgb(216,191,216) | [![Thistle][140]][140]            |
|   Tomato               |   #FF6347 | rgb(255,99,71)   | [![Tomato][141]][141]             |
|   Turquoise            |   #40E0D0 | rgb(64,224,208)  | [![Turquoise][142]][142]          |
|   Violet               |   #EE82EE | rgb(238,130,238) | [![Violet][143]][143]             |
|   Wheat                |   #F5DEB3 | rgb(245,222,179) | [![Wheat][144]][144]              |
|   White                |   #FFFFFF | rgb(255,255,255) | [![White][145]][145]              |
|   WhiteSmoke           |   #F5F5F5 | rgb(245,245,245) | [![WhiteSmoke][146]][146]         |
|   Yellow               |   #FFFF00 | rgb(255,255,0)   | [![Yellow][147]][147]             |
|   YellowGreen          |   #9ACD32 | rgb(154,205,50)  | [![YellowGreen][148]][148]        |
        
---

In addition to the named colors, there is also the keyword `transparent`, which represents a fully-transparent black: `rgba(0,0,0,0)`

---

  [1]: http://i.stack.imgur.com/dVsBW.png
  [2]: http://i.stack.imgur.com/jvOLr.png
  [3]: http://i.stack.imgur.com/vvCsT.png
  [4]: http://i.stack.imgur.com/3grSN.png
  [5]: http://i.stack.imgur.com/bH1ms.png
  [6]: http://i.stack.imgur.com/fUpj6.png
  [7]: http://i.stack.imgur.com/brQkJ.png
  [8]: http://i.stack.imgur.com/cr64Z.png
  [9]: http://i.stack.imgur.com/WwRQg.png
  [10]: http://i.stack.imgur.com/Pn17o.png
  [11]: http://i.stack.imgur.com/X7cq0.png
  [12]: http://i.stack.imgur.com/pgKFN.png
  [13]: http://i.stack.imgur.com/GGAHU.png
  [14]: http://i.stack.imgur.com/Fx9ga.png
  [15]: http://i.stack.imgur.com/dVCUd.png
  [16]: http://i.stack.imgur.com/U0eMw.png
  [17]: http://i.stack.imgur.com/QEETt.png
  [18]: http://i.stack.imgur.com/gI2Wv.png
  [19]: http://i.stack.imgur.com/9U0uV.png
  [20]: http://i.stack.imgur.com/ub3mh.png
  [21]: http://i.stack.imgur.com/MM0cY.png
  [22]: http://i.stack.imgur.com/YtKOx.png
  [23]: http://i.stack.imgur.com/qzL44.png
  [24]: http://i.stack.imgur.com/Cuf10.png
  [25]: http://i.stack.imgur.com/W83Ip.png
  [26]: http://i.stack.imgur.com/wfBJS.png
  [27]: http://i.stack.imgur.com/jfMqO.png
  [28]: http://i.stack.imgur.com/ZdZMD.png
  [29]: http://i.stack.imgur.com/oxUBA.png
  [30]: http://i.stack.imgur.com/zuMtq.png
  [31]: http://i.stack.imgur.com/HL4wv.png
  [32]: http://i.stack.imgur.com/DEL6o.png
  [33]: http://i.stack.imgur.com/kB7Ws.png
  [34]: http://i.stack.imgur.com/1ANMl.png
  [35]: http://i.stack.imgur.com/YnJo6.png
  [36]: http://i.stack.imgur.com/Ui2ao.png
  [37]: http://i.stack.imgur.com/RQKDI.png
  [38]: http://i.stack.imgur.com/dnrhi.png
  [39]: http://i.stack.imgur.com/5hFAA.png
  [40]: http://i.stack.imgur.com/Mz1e8.png
  [41]: http://i.stack.imgur.com/dsQkM.png
  [42]: http://i.stack.imgur.com/St8cI.png
  [43]: http://i.stack.imgur.com/Q0jnZ.png
  [44]: http://i.stack.imgur.com/YVu2z.png
  [45]: http://i.stack.imgur.com/woYd8.png
  [46]: http://i.stack.imgur.com/UauLn.png
  [47]: http://i.stack.imgur.com/TZoP1.png
  [48]: http://i.stack.imgur.com/mU5Ao.png
  [49]: http://i.stack.imgur.com/kUZUE.png
  [50]: http://i.stack.imgur.com/oAq7U.png
  [51]: http://i.stack.imgur.com/t3EEP.png
  [52]: http://i.stack.imgur.com/T2jzS.png
  [53]: http://i.stack.imgur.com/XSTBL.png
  [54]: http://i.stack.imgur.com/67NpE.png
  [55]: http://i.stack.imgur.com/mj8Uh.png
  [56]: http://i.stack.imgur.com/U0T5D.png
  [57]: http://i.stack.imgur.com/BtLwR.png
  [58]: http://i.stack.imgur.com/rvbJk.png
  [59]: http://i.stack.imgur.com/fIn2e.png
  [60]: http://i.stack.imgur.com/xZV1l.png
  [61]: http://i.stack.imgur.com/Y9Sn4.png
  [62]: http://i.stack.imgur.com/prMC1.png
  [63]: http://i.stack.imgur.com/YPrh0.png
  [64]: http://i.stack.imgur.com/giHoF.png
  [65]: http://i.stack.imgur.com/aZGQE.png
  [66]: http://i.stack.imgur.com/adxCj.png
  [67]: http://i.stack.imgur.com/38Fr3.png
  [68]: http://i.stack.imgur.com/TWZb7.png
  [69]: http://i.stack.imgur.com/8Yun8.png
  [70]: http://i.stack.imgur.com/hC4eY.png
  [71]: http://i.stack.imgur.com/D3scU.png
  [72]: http://i.stack.imgur.com/5A6Ef.png
  [73]: http://i.stack.imgur.com/gLPoR.png
  [74]: http://i.stack.imgur.com/2VvNK.png
  [75]: http://i.stack.imgur.com/Ekikz.png
  [76]: http://i.stack.imgur.com/JxzwJ.png
  [77]: http://i.stack.imgur.com/XsjW4.png
  [78]: http://i.stack.imgur.com/txw7K.png
  [79]: http://i.stack.imgur.com/x2SLs.png
  [80]: http://i.stack.imgur.com/k8Y23.png
  [81]: http://i.stack.imgur.com/8wsSt.png
  [82]: http://i.stack.imgur.com/QGEjh.png
  [83]: http://i.stack.imgur.com/etPK7.png
  [84]: http://i.stack.imgur.com/IIA3t.png
  [85]: http://i.stack.imgur.com/OSEq5.png
  [86]: http://i.stack.imgur.com/UL5lW.png
  [87]: http://i.stack.imgur.com/hMH8V.png
  [88]: http://i.stack.imgur.com/6WAqo.png
  [89]: http://i.stack.imgur.com/6PWeI.png
  [90]: http://i.stack.imgur.com/op66E.png
  [91]: http://i.stack.imgur.com/IKsEM.png
  [92]: http://i.stack.imgur.com/jweG4.png
  [93]: http://i.stack.imgur.com/OX3RE.png
  [94]: http://i.stack.imgur.com/3B3P5.png
  [95]: http://i.stack.imgur.com/Eymkn.png
  [96]: http://i.stack.imgur.com/llyIE.png
  [97]: http://i.stack.imgur.com/2DJyF.png
  [98]: http://i.stack.imgur.com/kdsyq.png
  [99]: http://i.stack.imgur.com/74kMX.png
  [100]: http://i.stack.imgur.com/rN1Vt.png
  [101]: http://i.stack.imgur.com/YPIiR.png
  [102]: http://i.stack.imgur.com/crswN.png
  [103]: http://i.stack.imgur.com/6KGAc.png
  [104]: http://i.stack.imgur.com/iC0zi.png
  [105]: http://i.stack.imgur.com/QWYbj.png
  [106]: http://i.stack.imgur.com/PLSrS.png
  [107]: http://i.stack.imgur.com/CpUCV.png
  [108]: http://i.stack.imgur.com/BtICR.png
  [109]: http://i.stack.imgur.com/B7grq.png
  [110]: http://i.stack.imgur.com/MywLd.png
  [111]: http://i.stack.imgur.com/0IYp9.png
  [112]: http://i.stack.imgur.com/NTY24.png
  [113]: http://i.stack.imgur.com/3dl4v.png
  [114]: http://i.stack.imgur.com/cYrOX.png
  [115]: http://i.stack.imgur.com/0TaRO.png
  [116]: http://i.stack.imgur.com/2sr8O.png
  [117]: http://i.stack.imgur.com/NNjmo.png
  [118]: http://i.stack.imgur.com/2v6DK.png
  [119]: http://i.stack.imgur.com/qD3Ou.png
  [120]: http://i.stack.imgur.com/lBOwr.png
  [121]: http://i.stack.imgur.com/uiBYF.png
  [122]: http://i.stack.imgur.com/PJFid.png
  [123]: http://i.stack.imgur.com/nt8Is.png
  [124]: http://i.stack.imgur.com/wOUue.png
  [125]: http://i.stack.imgur.com/eJ0bG.png
  [126]: http://i.stack.imgur.com/75qiD.png
  [127]: http://i.stack.imgur.com/xZIev.png
  [128]: http://i.stack.imgur.com/TyMam.png
  [129]: http://i.stack.imgur.com/x8Jrq.png
  [130]: http://i.stack.imgur.com/hh4eT.png
  [131]: http://i.stack.imgur.com/tnrm5.png
  [132]: http://i.stack.imgur.com/ubuww.png
  [133]: http://i.stack.imgur.com/pHN1B.png
  [134]: http://i.stack.imgur.com/uGW3Z.png
  [135]: http://i.stack.imgur.com/ls53F.png
  [136]: http://i.stack.imgur.com/SWuAT.png
  [137]: http://i.stack.imgur.com/gK6oL.png
  [138]: http://i.stack.imgur.com/UOatT.png
  [139]: http://i.stack.imgur.com/jIYOb.png
  [140]: http://i.stack.imgur.com/5EamN.png
  [141]: http://i.stack.imgur.com/IChJO.png
  [142]: http://i.stack.imgur.com/vPdms.png
  [143]: http://i.stack.imgur.com/eWWnU.png
  [144]: http://i.stack.imgur.com/XN0kJ.png
  [145]: http://i.stack.imgur.com/hpQVN.png
  [146]: http://i.stack.imgur.com/cAQ4D.png
  [147]: http://i.stack.imgur.com/3fAnQ.png
  [148]: http://i.stack.imgur.com/q7mKa.png

## Hexadecimal Value
## Background ##

CSS colors may also be represented as a hex triplet, where the members represent the red, green and blue components of a color. Each of these values represents a number in the range of `00` to `FF`, or `0` to `255` in decimal notation. Uppercase and/or lowercase Hexidecimal values may be used (i.e. `#3fc` = `#3FC` = `#33ffCC`). The browser interprets `#369` as `#336699`. If that is not what you intended but rather wanted `#306090`, you need to specify that explicitly.

The total number of colors that can be represented with hex notation is 256 ^ 3 or 16,777,216.

## Syntax ##

    color: #rrggbb;
    color: #rgb

| Value | Description |
| ------ | ------ |
| `rr` | `00` - `FF` for the amount of red |
| `gg` | `00` - `FF` for the amount of green |
| `bb` | `00` - `FF` for the amount of blue |

<!-- language: lang-css -->

    .some-class {
        /* This is equivalent to using the color keyword 'blue' */
        color: #0000FF;
    }
    
    .also-blue {
        /* If you want to specify each range value with a single number, you can!
           This is equivalent to '#0000FF' (and 'blue') */
        color: #00F;
    }

[Hexadecimal notation](https://en.wikipedia.org/wiki/Hexadecimal) is used to specify color values in the RGB color format, per the [W3C's 'Numerical color values'](https://www.w3.org/TR/css3-color/#numerical).


There are a lot of tools available on the Internet for looking up hexadecimal (or simply hex) color values.

Search for "**hex color palette**" or "**hex color picker**" with your favorite web browser to find a bunch of options!

Hex values always start with a pound sign (#), are up to six "digits" long, and are case-insensitive: that is, they don't care about capitalization. `#FFC125` and `#ffc125` are the same color.

## rgb() Notation
RGB is an additive color model which represents colors as mixtures of red, green, and blue light. In essence, the RGB representation is the decimal equivalent of the Hexadecimal Notation. In Hexadecimal each number ranges from 00-FF which is equivalent to 0-255 in decimal and 0%-100% in percentages.

<!-- language: lang-css -->

    .some-class {
        /* Scalar RGB, equivalent to 'blue'*/
        color: rgb(0, 0, 255);
    }
    
    .also-blue {
        /* Percentile RGB values*/
        color: rgb(0%, 0%, 100%);
    }

## Syntax ##

    rgb(<red>, <green>, <blue>)

| Value | Description |
| ------ | ------ |
| `<red>` | an integer from 0 - 255 or percentage from 0 - 100% |
| `<green>` | an integer from 0 - 255 or percentage from 0 - 100% |
| `<blue>` | an integer from 0 - 255 or percentage from 0 - 100% |


## rgba() Notation
Similar to [rgb() notation](https://www.wikiod.com/css/colors#rgb() Notation), but with an additional alpha (opacity) value.

<!-- language: lang-css -->

    .red {
        /* Opaque red */
        color: rgba(255, 0, 0, 1);
    }

    .red-50p {
        /* Half-translucent red. */
        color: rgba(255, 0, 0, .5);
    }

## Syntax ##

    rgba(<red>, <green>, <blue>, <alpha>);

| Value | Description |
| ------ | ------ |
| `<red>` | an integer from 0 - 255 or percentage from 0 - 100% |
| `<green>` | an integer from 0 - 255 or percentage from 0 - 100% |
| `<blue>` | an integer from 0 - 255 or percentage from 0 - 100% |
| `<alpha>` | a number from 0 - 1, where 0.0 is fully transparent and 1.0 is fully opaque |

## hsl() Notation
HSL stands for **hue** ("which color"), **saturation** ("how much color") and **lightness** ("how much white").

Hue is represented as an angle from 0° to 360° (without units), while saturation and lightness are represented as percentages.

    p {
        color: hsl(240, 100%, 50%); /* Blue */
    }

[![The HSL Color Wheel][1]][1]
# Syntax

    color: hsl(<hue>, <saturation>%, <lightness>%);

| Value | Description |
| ------ | ------ |
| `<hue>` | specified in degrees around the color wheel (without units), where 0° is red, 60° is yellow, 120° is green, 180° is cyan, 240° is blue, 300° is magenta, and 360° is red |
| `<saturation>` | specified in percentage where 0% is fully desaturated (grayscale) and 100% is fully saturated (vividly colored) |
| `<lightness>` | specified in percentage where 0% is fully black and 100% is fully white |

# Notes
- A saturation of 0% always produces a grayscale color; changing the hue has no effect.
- A lightness of 0% always produces black, and 100% always produces white; changing the hue or saturation has no effect.

  [1]: https://upload.wikimedia.org/wikipedia/commons/c/cb/HSL_color_solid_cylinder_alpha_lowgamma.png

## hsla() Notation
Similar to [hsl() notation](https://www.wikiod.com/css/colors#hsl() Notation), but with an added alpha (opacity) value.

    hsla(240, 100%, 50%, 0)     /* transparent */
    hsla(240, 100%, 50%, 0.5)   /* half-translucent blue */
    hsla(240, 100%, 50%, 1)     /* fully opaque blue */

# Syntax

    hsla(<hue>, <saturation>%, <lightness>%, <alpha>);

| Value | Description |
| ----- | ----------- |
| `<hue>` | specified in degrees around the color wheel (without units), where 0° is red, 60° is yellow, 120° is green, 180° is cyan, 240° is blue, 300° is magenta, and 360° is red |
| `<saturation>` | percentage where 0% is fully desaturated (grayscale) and 100% is fully saturated (vividly colored) |
| `<lightness>` | percentage where 0% is fully black and 100% is fully white |
| `<alpha>` | a number from 0 - 1 where 0 is fully transparent and 1 is fully opaque |

