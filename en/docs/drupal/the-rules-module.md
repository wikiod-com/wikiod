---
title: "The Rules module"
slug: "the-rules-module"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

The [Rules][1] module is an engine which allows site administrators to automate actions to be conditionally executed, either programmatically or in response to predetermined events.

Rules can react to **Rules Events** occurring on a Drupal site, such as a user logging in. And it can perform customized follow-up **Rules Actions**, such as redirecting to a certain page, which are to be conditionally executed if some **Rules Conditions** are satisfied.

  [1]: http://drupal.org/project/rules

### Resources

- **Video tutorials**: [Johan Falk][1] did an amazing job in the early Drupal 7 days by creating an impressive set of tutorials to "*Learn the Rules Framework*" with a set of over 30 videos and related blogposts hosted at `nodeone.se` (license = [Attribution-Noncommercial-Share Alike 3.0][2]).

  However the nodeone.se domain is no longer hosting them. [Learn Rules][3] is an attempt to recover these valuable blogposts (with related links to the corresponding videos).

- **[The Tiny Book of Rules][4]** is a (15 pages) jumpstart about the [Rules][5] module.


  [1]: https://www.drupal.org/user/153998
  [2]: http://creativecommons.org/licenses/by-nc-sa/3.0/
  [3]: http://drupal.placeto.be/tutorial/learn-the-rules-framework
  [4]: http://www.archive.org/details/TheTinydrupalBookOfRules
  [5]: https://www.drupal.org/project/rules

## A custom rule shown using the Rules UI
[![enter image description here][1]][1]

  [1]: http://i.stack.imgur.com/la3vj.png

## A custom rule shown in Rules Export format

Here is a Rules example in ***Rules export format***:

    { "rules_display_userpoints_after_updating_content" : {
        "LABEL" : "Display userpoints after updating content",
        "PLUGIN" : "reaction rule",
        "OWNER" : "rules",
        "REQUIRES" : [ "userpoints_rules", "rules", "rules_conditional" ],
        "ON" : { "node_update" : [] },
        "DO" : [
          { "userpoints_rules_get_current_points" : {
              "USING" : { "user" : [ "site:current-user" ], "tid" : "all" },
              "PROVIDE" : { "loaded_points" : { "total_points" : "Number of points in all categories together" } }
            }
          },
          { "drupal_message" : { "message" : "You now have [total-points:value] points" } },
          { "CONDITIONAL" : [
              {
                "IF" : { "NOT data_is" : { "data" : [ "total-points" ], "op" : "\u003C", "value" : "20" } },
                "DO" : [
                  { "drupal_message" : { "message" : "You have sufficient points (you still have [total-points:value] ...)." } }
                ]
              },
              { "ELSE" : [
                  { "drupal_message" : { "message" : "You DO NOT have sufficient points (you only have [total-points:value] ...)." } }
                ]
              }
            ]
          }
        ]
      }
    }

It does retrieve, as the very first Rules Action (not Rules Condition!) the current amount of user points of a user. If the amount is at least 20, it will display a message starting with "You have sufficient points ...", otherwise the message starts with "You DO NOT have sufficient points ...".

## Processing field collection items with Rules
Processing [Field collection][1] items with [Rules][2] is fun, really! Have a look at this Rule (in Rules export format):

    { "rules_calculate_sum_of_prices_in_all_field_collection_items" : {
        "LABEL" : "Calculate sum of prices in all field collection items",
        "PLUGIN" : "reaction rule",
        "OWNER" : "rules",
        "REQUIRES" : [ "rules" ],
        "ON" : { "node_view--article" : { "bundle" : "article" } },
        "IF" : [
          { "entity_has_field" : { "entity" : [ "node" ], "field" : "field_article_details" } }
        ],
        "DO" : [
          { "drupal_message" : { "message" : "\u003Cstrong\u003EDrupal calculator\u003C\/strong\u003E started ..." } },
          { "variable_add" : {
              "USING" : { "type" : "decimal", "value" : "0" },
              "PROVIDE" : { "variable_added" : { "total_price" : "Price total" } }
            }
          },
          { "LOOP" : {
              "USING" : { "list" : [ "node:field-article-details" ] },
              "ITEM" : { "article_details_item" : "Article details item" },
              "DO" : [
                { "data_calc" : {
                    "USING" : {
                      "input_1" : [ "total-price" ],
                      "op" : "+",
                      "input_2" : [ "article-details-item:field-price" ]
                    },
                    "PROVIDE" : { "result" : { "calculation_result" : "Calculation result" } }
                  }
                },
                { "data_set" : { "data" : [ "total-price" ], "value" : [ "calculation-result" ] } },
                { "drupal_message" : { "message" : "After adding a price of \u003Cstrong\u003E[article-details-item:field-price]\u003C\/strong\u003E for field collection item with id \u003Cstrong\u003E[article-details-item:item-id]\u003C\/strong\u003E, subtotal is \u003Cstrong\u003E[calculation-result:value]\u003C\/strong\u003E." } }
              ]
            }
          },
          { "drupal_message" : { "message" : "The \u003Cstrong\u003ETotal price\u003C\/strong\u003E for all prices included as field collection items is \u003Cstrong\u003E[total-price:value]\u003C\/strong\u003E." } },
          { "drupal_message" : { "message" : "\u003Cstrong\u003EDrupal calculator\u003C\/strong\u003E ended ..." } }
        ]
      }
    }

Some more details about this rule are below ...

### Rules Event:

Content is viewed (of type Article), adapt the machine name of the content type `article` to whatever fits (or use any other Rules Event that fits).

### Rules Condition:

Entity has field, whereas the entity is "node", and the machine name of my field collection field is `field_article_details` (adapt this machine name to whatever fits, but make sure you use the field collection field itself).

### Rules Actions:

Wake up, here is where the magic (fun?) is going to happen ... These are the Rules Actions involved:

1. <kbd>Show a message on the site</kbd>, with a message like so:

  > Drupal calculator started ...

2. <kbd>Add a variable</kbd>, whereas it is a variable named `total_price`, decimal (2 digits), initial value 0.

3. <kbd>Add a loop</kbd>, to iterate over each item of my field collection field (with machine name `field_article_details`), and perform these Rules Actions for each iteration:

  - <kbd>Calculate a value</kbd>, which calculates the sum of `total_price` (defined in Rules Action 2 above) and `article-details-item:field-price` (this is the machine name of the field in the field collection that contains the prices, decimal with 2 digits), and stores the result (sum) in variable `calculation_result`.

  - <kbd>Set a data value</kbd>, which simply copies the value stored in variable `calculation_result` in my `total_price` (defined in Rules Action 2 above). Remark: not sure (not tested), but maybe this `calculation_result` variable can be replaced straight by `total_price` (in the previous action), so that you would not need this action.

  - <kbd>Show a message on the site</kbd>, with a message like so:

    > After adding a price of 3.40 for field collection item with id 3, subtotal is 15.00.


4. <kbd>Show a message on the site</kbd>, with a message like so:

  > The Total price for all prices included as field collection items is 26.23.

5. <kbd>Show a message on the site</kbd>, with a message like so:

  > Drupal calculator ended ...

Obviously, this rule is rather a prototype. After you're convinced it works as it should, just remove all Rules Actions with <kbd>Show a message on the site</kbd>. So that only item 2 and 3 (without its last sub-bullet) is left as Rules Actions.

### Showtime ...

Here is a sample of my test results, i.e. the Drupal messages that are shown:


    Drupal calculator started ...
    After adding a price of 2.45 for field collection item with id 1, subtotal is 2.45.
    After adding a price of 9.15 for field collection item with id 2, subtotal is 11.60.
    After adding a price of 3.40 for field collection item with id 3, subtotal is 15.00.
    After adding a price of 1.23 for field collection item with id 4, subtotal is 16.23.
    The Total price for all prices included as field collection items is 26.23.
    Drupal calculator ended ...

### More info

If you're not familiar with field collections, try to first digest the answer to "[this question](http://drupal.stackexchange.com/questions/184848/how-to-iterate-over-all-field-collection-items-in-the-rules-module/208700#208700)".


  [1]: https://www.drupal.org/project/field_collection
  [2]: https://www.drupal.org/project/rules

