---
title: "Localization"
slug: "localization"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Syntax
 - `string gettext (string $message)`

## Localizing strings with gettext()
<!-- language-all: lang-php -->
GNU `gettext` is an extension within PHP that must be included at the `php.ini`:
```php 
extension=php_gettext.dll #Windows
extension=gettext.so #Linux
```

The `gettext` functions implement an NLS (Native Language Support) API which can be used to internationalize your PHP applications.

---

Translating strings can be done in PHP by setting the locale, setting up your translation tables and calling `gettext()` on any string you want to translate.

    <?php
    // Set language to French
    putenv('LC_ALL=    fr_FR');
    setlocale(LC_ALL, 'fr_FR');
    
    // Specify location of translation tables for 'myPHPApp' domain
    bindtextdomain("myPHPApp", "./locale");
    
    // Select 'myPHPApp' domain
    textdomain("myPHPApp");

**myPHPApp.po**

    #: /Hello_world.php:56
    msgid "Hello"
    msgstr "Bonjour"
    
    #: /Hello_world.php:242
    msgid "How are you?"
    msgstr "Comment allez-vous?"

gettext() loads a given post-complied .po file, a .mo. which maps your to-be translated strings as above.

After this small bit of setup code, translations will now be looked for in the following file:

 - `./locale/fr_FR/LC_MESSAGES/myPHPApp.mo`.

Whenever you call `gettext('some string')`, if `'some string'` has been translated in the `.mo` file, the translation will be returned. Otherwise, `'some string'` will be returned untranslated.
    
    // Print the translated version of 'Welcome to My PHP Application'
    echo gettext("Welcome to My PHP Application");
    
    // Or use the alias _() for gettext()
    echo _("Have a nice day");



