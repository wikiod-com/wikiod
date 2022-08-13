---
title: "Making network requests with HTTP API"
slug: "making-network-requests-with-http-api"
draft: false
images: []
weight: 9940
type: docs
toc: true
---

## Syntax
 - $response = wp_remote_get( $url, $args );
 - $response = wp_remote_post( $url, $args );
 - $response = wp_safe_remote_post( $url, $args );

## Parameters
| Parameter | Details |
| --------- | ------- |
| $url  | (string) (Required) Site URL to retrieve. |
| $args | (array) (Optional) Request arguments.     |

## Returns

_(WP_Error | array)_ The response as an array, or WP_Error on failure.

## GET a remote JSON resource
This snippet will grab a JSON formatted resource, decode it and print it in PHP array format.
```
// Fetch 
$response = wp_remote_get( 'http://www.example.com/resource.json' );

if ( ! is_wp_error( $response ) ) {
  $headers = wp_remote_retrieve_headers( $response );

  if ( isset( $headers[ 'content-type' ] ) && 'application/json' === $headers[ 'content-type' ] ) {
    print_r( json_decode( wp_remote_retrieve_body( $response ) ) );
  }
}
```

