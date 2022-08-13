---
title: "Customizer Hello World"
slug: "customizer-hello-world"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| --- | --- |
| mytheme | A unique identifier for your theme (or child theme). This can be your theme slug |


## Hello World Example
The fundamental concept of the customizer is that admins can live preview changes to their site, and then permanently add them.

The following can be copied and pasted into a theme's `functions.php` file to
 - Add a customizer section called `My First Section`
 - Add a customizer setting called `Hello World Color` allowing the admin to choose a color
 - Add a css rule for `.hello-world` that will corespond with the color chosen and default to `#000000` if nothing is chosen. The setting will be put in a `<style>` tag at the end of the `<head>`.

```

function mytheme_customize_register( $wp_customize ) {

    $wp_customize->add_section( 'my_first_section_id' , array(
        'title'      => __( 'My First Section', 'mytheme' ),
        'priority'   => 30,
    ) );

    $wp_customize->add_setting( 'hello_world_color' , array(
        'default'     => '#000000',
        'transport'   => 'refresh',
    ) );

    $wp_customize->add_control( new WP_Customize_Color_Control( $wp_customize, 'link_color', array(
        'label'        => __( 'Hello World Color', 'mytheme' ),
        'section'    => 'my_first_section_id',
        'settings'   => 'hello_world_color',
    ) ) );


}
add_action( 'customize_register', 'mytheme_customize_register' );


function mytheme_customize_css()
{
    ?>
    <style type="text/css">
        .hello-world { color: #<?php echo get_theme_mod('hello_world_color', '000000'); ?>; }
    </style>
    <?php
}
add_action( 'wp_head', 'mytheme_customize_css');

```

