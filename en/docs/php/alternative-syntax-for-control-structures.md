---
title: "Alternative Syntax for Control Structures"
slug: "alternative-syntax-for-control-structures"
draft: false
images: []
weight: 9842
type: docs
toc: true
---

## Syntax
 - structure: /* code */ endstructure;

When mixing the alternative structure for `switch` with HTML, it is important to not have any whitespace between the initial `switch($condition):` and first `case $value:`. Doing this is attempting to echo something (whitespace) before a case.

All control structures follow the same general idea. Instead of using curly braces to encapsulate the code, you're using a colon and `endstructure;` statement: `structure: /* code */ endstructure;`

## Alternative if/else statement
    <?php
    
    if ($condition):
        do_something();
    elseif ($another_condition):
        do_something_else();
    else:
        do_something_different();
    endif;
    
    ?>
    
    <?php if ($condition): ?>
        <p>Do something in HTML</p>
    <?php elseif ($another_condition): ?>
        <p>Do something else in HTML</p>
    <?php else: ?>
        <p>Do something different in HTML</p>
    <?php endif; ?>

## Alternative for statement
    <?php
    
    for ($i = 0; $i < 10; $i++):
        do_something($i);
    endfor;
    
    ?>
    
    <?php for ($i = 0; $i < 10; $i++): ?>
        <p>Do something in HTML with <?php echo $i; ?></p>
    <?php endfor; ?>

## Alternative while statement
    <?php
    
    while ($condition):
        do_something();
    endwhile;
    
    ?>
    
    <?php while ($condition): ?>
        <p>Do something in HTML</p>
    <?php endwhile; ?>

## Alternative foreach statement
    <?php
    
    foreach ($collection as $item):
        do_something($item);
    endforeach;
    
    ?>
    
    <?php foreach ($collection as $item): ?>
        <p>Do something in HTML with <?php echo $item; ?></p>
    <?php endforeach; ?>

## Alternative switch statement
    <?php
    
    switch ($condition):
        case $value:
            do_something();
            break;
        default:
            do_something_else();
            break;
    endswitch;
    
    ?>
    
    <?php switch ($condition): ?>
    <?php case $value: /* having whitespace before your cases will cause an error */ ?>
        <p>Do something in HTML</p>
        <?php break; ?>
    <?php default: ?>
        <p>Do something else in HTML</p>
        <?php break; ?>
    <?php endswitch; ?>



