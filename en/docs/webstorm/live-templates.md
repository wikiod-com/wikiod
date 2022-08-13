---
title: "Live templates"
slug: "live-templates"
draft: false
images: []
weight: 9999
type: docs
toc: true
---

## Parameters
| Parameter | Details |  
| --------- | ------- |  
| $END$ | place where cursor would placed after generate live template |  


## Init javascript section and attach scripts to page
**Create new script section**. I call that **sa**(Script Anonymuos) that meens for me generate JavaScript section with anonymuous function call inside.

    <script type="text/javascript">
       $(function () {
           $END$
       });
    </script>

If in your project still use old jQuery you could add next template **sao** (Script Anonymuos Old) for have ability use `$` variable in scope.

    <script type="text/javascript">
       jQuery(function ($) {
           $END$
       })(jQuery);
    </script>

**Attach script from file**. I called it **ss**(Script source) and use it for attaching internal `*.js` files

    <script type="text/javascript" src="$END$"></script>

**Generate local scope function**. If you need to generate local scope function use that template that I called **ja**(jQuery anonymuous)

    $(function() {
        $END$
    });

**Debug**. Very often for debug code I use **cl**(Console Log)

    console.log($END$);

