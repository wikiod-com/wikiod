---
title: "Addremove contact info for users with user_contactmethods filter hook"
slug: "addremove-contact-info-for-users-with-user_contactmethods-filter-hook"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Enabling most popular social networks
    function social_profiles( $contactmethods ) {
        
        $contactmethods['facebook_profile']  = 'Facebook Profile URL';
        $contactmethods['twitter_profile']   = 'Twitter Profile URL';
        $contactmethods['google_profile']    = 'Google Profile URL';
        $contactmethods['linkedin_profile']  = 'Linkedin Profile URL';
        $contactmethods['github_profile']    = 'GitHub Profile URL';
        $contactmethods['behance_profile']   = 'Behance Profile URL';
        $contactmethods['dribbble_profile']  = 'Dribbble Profile URL';
        $contactmethods['stack_profile']     = 'Stack Exchange Profile URL';
        $contactmethods['twitch_profile']    = 'Twitch Profile URL';
        $contactmethods['angellist_profile'] = 'AngelList Profile URL';
        
        return $contactmethods;
    }
    
    add_filter( 'user_contactmethods', 'social_profiles', 10, 1);

You will get this fileds in your dashboard:

[![WordPress dashboard screenshot][1]][1]


  [1]: http://i.stack.imgur.com/xPbGY.png

And this is how you retrieve it in code

    <?php $user_stack_exchange = get_the_author_meta( 'stack_profile' ); ?>

## Removing contact method
    function remove_contact_methods( $contactmethods ) {
        
        unset($contactmethods['facebook_profile']);
        unset($contactmethods['twitter_profile']);
       
        return $contactmethods;
    }
    
    add_filter( 'user_contactmethods', 'remove_contact_methods', 10, 1);

