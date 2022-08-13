---
title: "Security in WordPress - Sanitization"
slug: "security-in-wordpress---sanitization"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Syntax
 - sanitize_text_field( string $str )
 - sanitize_title( string $title, string $fallback_title, string $context )
 - sanitize_email( string $email )
 - sanitize_html_class( string $class, string $fallback )
 - sanitize_file_name( string $name )
 - sanitize_user( string $username, boolean $strict )

Security should be always in mind when developing. Without security an app is open to various attacks such as SQL Injections, XSS, CSRF, RFI etc that can lead to serious problems.

Untrusted data comes from many sources (users, third party sites, your own database!, ...) and all of it needs to be validated both on input and output. (Sourse: WordPress Codex)

The data should be validated, sanitized or escaped depending the use and the purpose. 

To validate is to ensure the data you've requested of the user matches what they've submitted. (Sourse: WordPress Codex)

Sanitization is a bit more liberal of an approach to accepting user data. We can fall back to using these methods when there's a range of acceptable input. (Sourse: Wordpress Codex)

To escape is to take the data you may already have and help secure it prior to rendering it for the end user. (Sourse: WordPress Codex)

## Sanitize text field
    $title = sanitize_text_field( $_POST['title'] );

## Sanitize title
The returned value is intended to be suitable for use in a URL, not as a human-readable title. Use sanitize_text_field instead.

    $new_url = sanitize_title($title);

## Sanitize email
    $sanitized_email = sanitize_email('     admin@example.com!     ');

## Sanitize html class


    $post_class = sanitize_html_class( $post->post_title );
    echo '<div class="' . $post_class . '">';

## Sanitize file name


    $incfile = sanitize_file_name($_REQUEST["file"]);
    include($incfile . ".php");

Without sanitizing the file name an attacker could simple pass http://attacker_site/malicous_page as input and execute whatever code in your server.

## Sanitize user name
    $user = sanitize_user("attacker username<script>console.log(document.cookie)</script>");

$user value after sanitize is "attacker username"

