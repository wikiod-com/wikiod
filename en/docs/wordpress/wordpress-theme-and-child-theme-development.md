---
title: "Wordpress theme and child-theme development"
slug: "wordpress-theme-and-child-theme-development"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Wordpress is a widely used CMS for creating simple information websites but also for creating more sophisticated websites and even small webshops.

Wordpress makes use of themes. These themes are used for creating the lay-out and content functionality of a Wordpress website. The themes can be found all over the internet.

Each thme has his own unique functionality and lay-out but sometimes it's hard to find just the right theme for a website. Luckily we're also able to create our own theme.

## Developing your own theme
A wordpress theme consists two types of files. The basic files that each theme has and the files that define the theme's layout and functionality. This second group i'm going to call the theme specific files.

**The basic theme files**<br>
The basic theme files are the files that are used to setup and register a theme. In the list below i will shortly describe each file and its usage. Later on i'll add the most basic example files that are needed to set up your own wordpress theme.
- `functions.php`: The functions.php file is used to register all the functions, sidebars, scripts and includes of the theme. In this file you're able to, for example, include CSS files, JS files, etc.
- `Header and footer`: The header and footer files (header.php and footer.php) are the files that are used to call on the header and the footer. The header and footer file for example hold the link to the wordpress back-end system.
- `index.php`: The index.php file is the file that creates the default-page template. In this file you can see, edit and remove pieces of this default-template lay-out.
- `single.php`: The single.php file is the file that creates the single posts template page. Just like the default-template for the pages but now for the single post pages.
- `format.php` The format.php file is the file that builds the content-text template from a page. So if you would have a home page and you would edit it from the back-end by adding a text. This file creates the standard markup of this text.
- `404.php` The 404.php file creates the 404 template. This file consists of the basic lay-out of this page.
- `archive.php` The archive.php file creates the lay-out of the archive page.
- `style.css` The basic stylesheet file.

So  in this list you can see all the **required** files for the set up of your very own Wordpress theme. Now lets take a look at some files that you're able to create if you want to but are **not required** files for a wordpress theme. These files are mostly template files and other functional extentions.

**Custom page templates**<br>
`page-<your own name>.php`: In a Wordpress theme you're able to create multiple page templates. by creating new page template files. A standard page template file consists of the following name attributes. `page` `name of the template` and `.php` If for example you would like to create a new page template for your blog page you could call it `page-blog.php`  Wordpress automaticly reads the file and adds the file to the choose template menu. Do make sure that you've atleast included the `get_header()` and `get_footer()` functions. Also make sure you name your template in a comment at the top of the file by adding the following example.

    <?php
        /*
         * Template Name: Homepage Template
         */
        get_header();
    ?>

**Custom single post page templates**<br>
`single-<your own name>.php`: In a Wordpress theme just like the page template described above you’re also able to create your own single posts page templates. Just like the page template the file consists of three parts `single` for declaring it’s a single post page `<your name of the template>` and the file extention `.php`. Just like the page template minimum requirements to make sure Wordpress reads the new template are adding the functions  `get_header()` and `get_footer()`. And ofcourse also adding your template name like the example below

    <?php
    
    /*
    * Template Name: Post Portfolio
    * Template Post Type: post, page
    */
    
    ?>
We also indicate the `Template post type:` wich stands for the kind of template it is, in this case post and page.

**Custom post text templates**<br>
`format -<your own name>.php`: In a Wordpress theme you’re also able to create post output templates. These format templates are the lay-out and contents of a post. For example if in some cases you want the post to only show the content or the title of the post you can use these templates to create those kind of adjustments. Since these kind of templates are only formatting the post back-ends content that was created by a user we don’t need to include `get_header()` and `get_footer()` since these are already defined in the pages templates. Do make sure your template is able to recognize a post by using the following basic example.

    <div>
        <article id="post-<?php the_ID(); ?>" <?php post_class(); ?>>
        </article>
    </div>

So now that we know something about the basic files and some of the many template specific files it's time to start talking about sidebars and widgets. In the future this will be added together with a start on the step to step tutorial on creating a very own Wordpress theme.


