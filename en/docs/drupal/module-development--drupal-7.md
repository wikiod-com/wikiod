---
title: "Module development - Drupal 7"
slug: "module-development---drupal-7"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

[Examples for developers](https://drupal.org/project/examples) module should be used as a reference for module development ideally. It has explanation of all the major APIs, well documented usage. It is all in for begineers to understand module development.

## Basic module providing a simple page
really_neat.info

<!-- language: lang-none -->

    name = Really Neat Module
    description = Provides a really neat page for your site
    core = 7.x

really_neat.module

<!-- language: lang-php -->

    <?php

    /**
     * @file
     * Hook implementation and shared functions for the Really Neat Module.
     */

    /**
     * Implements hook_menu().
     */
    function really_neat_menu() {
      $items = array();
      
      $items ['really/neat'] = array(
          'title' => 'A Really Neat Page',
          'page_callback' => 'really_neat_page',
          'access_callback' => TRUE, //Anyone can access. 
          // Or replace with array([name-of-permission]),
        ),

      return $items;
    }

    /**
     * Page callback: Displays something really neat
     */
    function really_neat_page() {
      return "Really Neat!"
    }

## Basic module providing a custom block
custom_module.info

    name = Custom Module
    description = Creates a block containing a custom output.
    core = 7.x

custom_module.module

    /**
     * Initiates hook_block_info.
     *
     * Registers the block with Drupal.
     */
    function custom_module_block_info() {
      $blocks = array();
        //Registers the machine name of the block.
      $blocks['custom_block'] = array(
          //Sets the human readable, administration name.
        'info' => t('My Custom Block'),
          //Tells Drupal not to cache this block.
          //Used if there is dynamic content.
        'cache' => DRUPAL_NO_CACHE,
      );
      return $blocks;
    }

    /**
     * Initiates hook_block_view().
     *
     * Sets the block title and content callback.
     */
    function custom_module_block_view($delta = '') {
      $block = array();

      switch ($delta) {
          //Must be the machine name defined in the hook_block_info.
        case 'custom_block':
            //The blocks title.
          $block['subject'] = 'My custom block';
            //The string or function that will provide the content of the block.
          $block['content'] = custom_module_block_content(); 
          break;
      }

      return $block;
    }

    /**
     * Returns the content of the custom block.
     */
    function custom_module_block_content() {
      $content = "This function only returns a string, but could do anything."

      return $content;
    }

## Basic custom form for inclusion in either page or block examples.
Simple form, validation and submission functions to create a "mailing list" feature.
This can then be applied to either the basic page or basic block examples.

Assumes you have created a table in the drupal database called 'mailing_list' with the fields first name, last name and email address.

Additional information on the Form API and additional field options: https://api.drupal.org/api/drupal/developer!topics!forms_api_reference.html/7.x/

    function custom_module_form($form, &$form_state) {
      $form['first_name'] = array (
        '#type' => 'textfield',
        '#title' => 'First Name',
        '#required' => TRUE,
      );
      $form['last_name'] = array (
        '#type' => 'textfield',
        '#title' => 'Last Name',
        '#required' => TRUE,
      );
      $form['email'] = array (
        '#type' => 'textfield',
        '#title' => 'First Name',
        '#required' => TRUE,
      );

      return $form;
    }

    function custom_module_form_validate($form, &$form_state) {
      if (!filter_var($email, FILTER_VALIDATE_EMAIL)) {
        form_set_error('email', t('Please provide a valid email address.'));
      }
    }

    function custom_module_form_submit($form, &$form_state) {
      //Useful function for just getting the submitted form values
      form_state_values_clean($form_state);

      //Save time later by assigning the form values to variables.
      $first_name = $form_state['values']['first_name'];
      $last_name = $form_state['values']['last_name'];
      $email = $form_state['values']['email'];

      //Insert the submitted data to the mailing_list database table.
      db_insert('mailing_list')
        ->fields(array(
          'first name' => $first_name,
          'last name' => $last_name,
          'email' => $email,
        ))
        ->execute();
      //Set a thank you message.
      drupal_set_message('Thank you for subscribing to our mailing list!');

      //drupal_goto() could be used here to redirect to another page or omitted to reload the same page.
      //If used, drupal_goto() must come AFTER drupal_set_message() for the message to be displayed on the new page.
    }

## Basic module providing a custom block
custom_module.info

    name = Custom Module
    description = Creates a block containing a custom output.
    core = 7.x

custom_module.module

    /**
     * Initiates hook_block_info.
     *
     * Registers the block with Drupal.
     */
    function custom_module_block_info() {
      $blocks = array();
        //Registers the machine name of the block.
      $blocks['custom_block'] = array(
          //Sets the human readable, administration name.
        'info' => t('Titania Price Widget'),
          //Tells Drupal not to cache this block.
          //Used if there is dynamic content.
        'cache' => DRUPAL_NO_CACHE,
      );
      return $blocks;
    }

    /**
     * Initiates hook_block_view().
     *
     * Sets the block title and content callback.
     */
    function custom_module_block_view($delta = '') {
      $block = array();

      switch ($delta) {
          //Must be the machine name defined in the hook_block_info.
        case 'custom_block':
            //The blocks title.
          $block['subject'] = 'My custom block';
            //The string or function that will provide the content of the block.
          $block['content'] = custom_module_block_content(); 
          break;
      }

      return $block;
    }

    /**
     * Returns the content of the custom block.
     */
    function custom_module_block_content() {
      $content = "This function only returns a string, but could do anything."

      return $content;
    }

## Basic custom form for inclusion in either page or block examples.
Simple form, validation and submission functions to create a "mailing list" feature.
This can then be applied to either the basic page or basic block examples.

Assumes you have created a table in the drupal database called 'mailing_list' with the fields first name, last name and email address.

Additional information on the Form API and additional field options: https://api.drupal.org/api/drupal/developer!topics!forms_api_reference.html/7.x/

    function custom_module_form($form, &$form_state) {
      $form['first_name'] = array (
        '#type' => 'textfield',
        '#title' => 'First Name',
        '#required' => TRUE,
      );
      $form['last_name'] = array (
        '#type' => 'textfield',
        '#title' => 'Last Name',
        '#required' => TRUE,
      );
      $form['email'] = array (
        '#type' => 'textfield',
        '#title' => 'First Name',
        '#required' => TRUE,
      );

      return $form;
    }

    function custom_module_form_validate($form, &$form_state) {
      if (!filter_var($email, FILTER_VALIDATE_EMAIL)) {
        form_set_error('email', t('Please provide a valid email address.'));
      }
    }

    function custom_module_form_submit($form, &$form_state) {
      //Useful function for just getting the submitted form values
      form_state_values_clean($form_state);

      //Save time later by assigning the form values to variables.
      $first_name = $form_state['values']['first_name'];
      $last_name = $form_state['values']['last_name'];
      $email = $form_state['values']['email'];

      //Insert the submitted data to the mailing_list database table.
      db_insert('mailing_list')
        ->fields(array(
          'first name' => $first_name,
          'last name' => $last_name,
          'email' => $email,
        ))
        ->execute();
      //Set a thank you message.
      drupal_set_message('Thank you for subscribing to our mailing list!');

      //drupal_goto() could be used here to redirect to another page or omitted to reload the same page.
      //If used, drupal_goto() must come AFTER drupal_set_message() for the message to be displayed on the new page.
    }

## Example custom_module.install file for creating a database table
*Can be used in conjunction with the **custom form example** to create a table in the drupal database for a Mailing List feature.*

This example was made by creating the table directly in my development database, then created the data for hook_schema() using the [Schema module][1]. 

This allows for automatic table creation during module install on staging and production sites.

custom_module.install

    /**
     * Installs the database schema.
     */
    function custom_module_install() {
      drupal_install_schema('mailing_list');
    }

    /**
     * Uninstalls the database schema.
     */
    function custom_module_uninstall() {
      drupal_uninstall_schema('mailing_list');
    }
    
    /**
    * Creates the tables using the schema API.
    */
    function custom_module_schema() {
      $schema['mailing_list'] = array(
        'description' => 'TODO: please describe this table!',
        'fields' => array(
          'first name' => array(
            'description' => 'TODO: please describe this field!',
            'type' => 'int',
            'not null' => TRUE,
          ),
          'last name' => array(
            'description' => 'TODO: please describe this field!',
            'type' => 'int',
            'not null' => TRUE,
          ),
          'email' => array(
            'description' => 'TODO: please describe this field!',
            'type' => 'int',
            'not null' => TRUE,
          ),
        ),
      );
    }


  [1]: https://www.drupal.org/project/schema

