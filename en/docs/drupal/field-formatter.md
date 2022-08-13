---
title: "Field Formatter"
slug: "field-formatter"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

A field formatter specifies the way a field is rendered in Drupal templates. The formatter used by each field can be configured in the *Manage display* tab associated with the entity type that you are configuring.

Fields can have different formatters depending on the view mode that is being displayed which allows you to control how a field is rendered in different parts of your website.

Some things are important to consider when implementing a Field Formatter.

The implementation of your formatter must be inside your module in the folder `src/Plugin/Field/FieldFormatter`. The annotations are also critical as they identify your module and which field types it's applicable to.

In this example, this formatter is applicable only to fields of the type `email`. You can apply your formatter to a number of fields if necessary. If your formatter would, for whatever reason, be applicable to email and date fields:

    field_type = {
      "email",
      "date",
    }

One pitfall I've faced when first implementing field formatters with settings is that the settings weren't saved when changed. There is no explicit save method and the solution is to implement the `defaultSettings()` method and specify the field names that make up your configuration form. Also don't forget to set the `#default_value` in the `settingsForm` method.

If you want to have a specific TWIG template for your formatter it's as simple as configuring a `#theme` key while building the render array in the `viewElements` method then in your `.module` file implement `hook_theme`

    function obfuscator_field_formatter_theme() {
      return [
        'obfuscator_field_formatter' => [
          'variables' => array('title' => NULL, 'url' => NULL),
          'template' => 'obfuscator-field-formatter'
        ],
      ];
    }

Then create the `templates` folder in the root of your module and have a file named `obfuscator-field-formatter.twig.html` where you output the markup you need. In this example the variables the from the render `#title` and `#url` will be available.

## Obfuscated Email Formatter
In our example we will be creating a customized formatter for email addresses that will allow us to display obfuscated email addresses to fool those nasty spammers.

The formatter will have a some configuration options that will allow us to control how the email address is obfuscated:

- Remove @ and . and replace them with a space,
- Replace @ by at and . by dot,

Please note that this is just an academic example to show how field formatters are displayed and configured and is obviously not very useful for actual anti-spamming.

This example assumes that you have a module named `obfuscator_field_formatter` properly configured and activated.

    namespace Drupal\obfuscator_field_formatter\Plugin\Field\FieldFormatter;
    
    use Drupal\Core\Field\FieldDefinitionInterface;
    use Drupal\Core\Field\FieldItemInterface;
    use Drupal\Core\Field\FieldItemListInterface;
    use Drupal\Core\Field\FormatterBase;
    use Drupal\Core\Form\FormStateInterface;
    
    /**
     * Plugin implementation of the 'example_field_formatter' formatter.
     *
     * @FieldFormatter(
     *   id = "email_obfuscator_field_formatter",
     *   label = @Translation("Obfuscated Email"),
     *   field_types = {
     *     "email"
     *   }
     * )
     */
    class ObfuscatorFieldFormatter extends FormatterBase {
      private $options = [];
    
      public function __construct($plugin_id, $plugin_definition, FieldDefinitionInterface $field_definition, array $settings, $label, $view_mode, array $third_party_settings) {
        parent::__construct($plugin_id, $plugin_definition, $field_definition, $settings, $label, $view_mode, $third_party_settings);
    
        $this->options = [
          'remove_chars' => $this->t('Remove @ and . and replace them with a space'),
          'replace_chars' => $this->t('Replace @ by at and . by dot'),
        ];
      }
    
      public static function defaultSettings() {
        return [
          'obfuscator_formatter_type' => '',
        ] + parent::defaultSettings();
      }
    
      public function settingsForm(array $form, FormStateInterface $form_state) {
        return [
          'obfuscator_formatter_type' => [
            '#type' => 'select',
            '#title' => $this->t('Obfuscation Type'),
            '#options' => $this->options,
            '#default_value' => $this->getSetting('obfuscator_formatter_type'),
          ]
        ] + parent::settingsForm($form, $form_state);
      }
    
      public function settingsSummary() {
        $summary = parent::settingsSummary();
        $type = $this->getSetting('obfuscator_formatter_type');
    
        if(!is_null($type) && !empty($type)) {
          $summary[] = $this->t('Obfuscation: @value', ['@value' => $this->options[$type]]);
        }
    
        return $summary;
      }
    
      public function viewElements(FieldItemListInterface $items, $langcode) {
        $elements = [];
    
        foreach ($items as $delta => $item) {
          $elements[$delta] = [
            '#type' => 'inline_template',
            '#template' => '{{ value|nl2br }}',
            '#context' => ['value' => $this->viewValue($item)],
          ];
        }
    
        return $elements;
      }
    
      protected function viewValue(FieldItemInterface $item) {
        $obfuscated = $item->value;
        $type = $this->getSetting('obfuscator_formatter_type');
    
        switch($type) {
          case 'remove_chars': {
            $obfuscated = str_ireplace(['@', '.'], ' ', $item->value);
            break;
          }
    
          case 'replace_chars': {
            $obfuscated = str_ireplace(['@', '.'], [' AT ', ' DOT '], $item->value);
            break;
          }
        }
    
        return $obfuscated;
      }
    }


