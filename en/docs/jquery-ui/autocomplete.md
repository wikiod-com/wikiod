---
title: "Autocomplete"
slug: "autocomplete"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Simple example
The Autocomplete widgets provides suggestions while you type into the field. 

    <script>
      $(document).ready(function() {
        var tags = ["ask","always", "all", "alright", "one", "foo", "blackberry", "tweet","force9", "westerners", "sport"];
        $( "#tags" ).autocomplete({
          source: tags
        });
      });
    </script>
    <input type='text' title='Tags' id='tags' />    

