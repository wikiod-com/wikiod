---
title: "Recipes"
slug: "recipes"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

This topic is a collection of solutions to common tasks in PHP. The examples provided here will help you overcome a specific problem. You should already be familiar with the basics of PHP.

## Create a site visit counter
    <?php
    $visit = 1;
    
    if(file_exists("counter.txt"))
    {
        $fp    = fopen("counter.txt", "r");
        $visit = fread($fp, 4);
        $visit = $visit + 1;
    }

    $fp = fopen("counter.txt", "w");
    fwrite($fp, $visit);
    echo "Total Site Visits: " . $visit;
    fclose($fp);

