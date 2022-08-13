---
title: "Remove Auto Line Breaks From Content and Excerpt"
slug: "remove-auto-line-breaks-from-content-and-excerpt"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

For sites that rely on HTML by hand in the editor or excerpts, ones you want to code yourself, the auto line breaks can be an annoyance. You can disable them by removing these filters.

These must be executed directly in an include file. Whether it is in functions.php or in another include file, these cannot be wrapped in a hook. They won't work on init or any other I have found so far.

They can also be included directly in a template like page.php to execute only for that template.

NOTE: DO NOT INCLUDE THIS IN A DISTRIBUTED THEME OR PLUGIN (unless it is disabled by default, like not including the include file it's in unless the user specifies). 

This is bad practice to include in a site you don't control because it can and will break the output of any other themes or plugins. 

## Remove the Filters
    // Remove the auto-paragraph and auto-line-break from the content
    remove_filter( 'the_content', 'wpautop' );

    // Remove the auto-paragraph and auto-line-break from the excerpt
    remove_filter( 'the_excerpt', 'wpautop' );

## Function to remove the filters
    /**
     * Remove the automatic line breaks from content and excerpts.
     *
     * @since 1.0.0
     */
    function remove_content_auto_line_breaks() {
        // Remove the auto-paragraph and auto-line-break from the content
        remove_filter( 'the_content', 'wpautop' );

        // Remove the auto-paragraph and auto-line-break from the excerpt
        remove_filter( 'the_excerpt', 'wpautop' );
    }
    
    // Execute the function
    remove_content_auto_line_breaks();

