---
title: "Security in WordPress - Escaping"
slug: "security-in-wordpress---escaping"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Syntax
 - esc_html( string $text )
 - esc_url( string $url, array $protocols, string $_context )
 - esc_js( string $text )
 - wp_json_encode( mixed $data, int $options, int $depth = 512 )
 - esc_attr( string $text )
 - esc_textarea( string $text )

Security should be always in mind when developing. Without security an app is open to various attacks such as SQL Injections, XSS, CSRF, RFI etc that can lead to serious problems.

Untrusted data comes from many sources (users, third party sites, your own database!, ...) and all of it needs to be validated both on input and output. (Source: WordPress Codex)

The data should be validated, sanitized or escaped depending the use and the purpose.

To validate is to ensure the data you've requested of the user matches what they've submitted. (Source: WordPress Codex)

Sanitization is a bit more liberal of an approach to accepting user data. We can fall back to using these methods when there's a range of acceptable input. (Source: WordPress Codex)

To escape is to take the data you may already have and help secure it prior to rendering it for the end user. (Source: WordPress Codex)

## escape data in HTML code
esc_html should be used anytime we're outputting data inside HTML code.

    <h4><?php echo esc_html( $title ); ?></h4>

## escape a url
    <a href="<?php echo esc_url( home_url( '/' ) ); ?>">Home</a>

    <img src="<?php echo esc_url( $user_picture_url ); ?>" />

## escape data in js code
`esc_js()` is intended to be used for inline JS, inside a tag attribute.

For data inside a `<script>` tag use `wp_json_encode()`.

    <input type="text" onfocus="if( this.value == '<?php echo esc_js( $fields['input_text'] ); ?>' ) { this.value = ''; }" name="name">

`wp_json_encode()` encodes a variable into JSON, with some sanity checks. 

Note that `wp_json_encode()` includes the string-delimiting quotes automatically.

    <?php
    $book = array(
        "title" => "JavaScript: The Definitive Guide",
        "author" => "Stack Overflow",
    );
    ?>
    <script type="text/javascript">
    var book = <?php echo wp_json_encode($book) ?>;
    /* var book = {
        "title": "Security in WordPress",
        "author" => "Stack Overflow",
    }; */
    </script>

or

    <script type="text/javascript">
        var title = <?php echo wp_json_encode( $title ); ?>;
        var content = <?php echo wp_json_encode( $content ); ?>;
        var comment_count = <?php echo wp_json_encode( $comment_count ); ?>;
    </script>

## escape attributes
    <input type="text" value="<?php echo esc_attr($_POST['username']); ?>" />

## escape data in textarea
    <textarea><?php echo esc_textarea( $text ); ?></textarea>

