---
title: "Drupal 8 Entity API"
slug: "drupal-8-entity-api"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

The Entity System in Drupal 8 allows developers to easily create custom content types and model data relationships around these types.The extensive API provides support for form generation,data validation,configuration,routing and a lot of other features.

## Create a content entity using Drupal Console
Drupal console brings scaffolding to the Drupal ecosystem and makes it easy to generate a content entity.

In most instances you will find it easier to work with a custom entity within a custom module.

## Step 1 : Generate a module

    vendor/bin/drupal generate:module

Follow the prompts and create your custom module.

## Step 2: Generate a content entity

    vendor/bin/drupal generate:entity:content

Follow the prompts on your commandline making sure to select the custom module created in the previous step.

 

