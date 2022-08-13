---
title: "Initialize Foundation or Bootstrap on ember-cli in a proper way"
slug: "initialize-foundation-or-bootstrap-on-ember-cli-in-a-proper-way"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

Bootstrap : I think that's not proper way. The best way in my opinion is an ember-bootstrap addon.

ember-bootstrap uses the Bootstrap CSS classes while replacing the behaviors from the components Bootstrap implements in bootstrap.js, such as toggle, navbar, modal, etc., with equivalent, CSS class-compatible native Ember components.

Foundation: 
There is an addon called Ember CLI Foundation 6 SASS, it's also installed using command line.


## Parameters
| Parameter | Usage|
| ------ | ------ |
| Ember install   | Download a new extension package using Ember|
|npm install|Download a new extension package using node.js|
|SASS|CSS language necessary in Foundation|
|Ember-cli-build.js|File with Ember imports, configuration, etc.|
|{{#bs-modal-simple}}|Creation of a new bootstrap modal|
|fade=fade|Set the modal animations|
|{{#bs-button}}|Button with a Bootstrap cool look|
|{{#bs-form onSubmit(action = "Submit")}}|New form with an action after submitting|

Both addons are not mine, I thought it will be nice to present them to You, there are github pages of addons: <br /><br />
Ember Bootstrap: 
https://github.com/kaliber5/ember-bootstrap <br /><br />
Ember foundation 6 https://github.com/acoustep/ember-cli-foundation-6-sass
<br /><br />
You can find documentation there.

## Ember build file with Foundation addons
    // ember-cli-build.js
    
    /* global require, module */
    var EmberApp = require('ember-cli/lib/broccoli/ember-app');
    
    module.exports = function(defaults) {
      var app = new EmberApp(defaults, {
        // Add options here
        'ember-cli-foundation-6-sass': {
                'foundationJs': [
                    'core',
                    'util.box',
                    'util.keyboard',
                    'util.mediaQuery',
                    'util.motion',
                    'util.nest',
                    'util.timerAndImageLoader',
                    'util.touch',
                    'util.triggers',
                    'abide',
                    'accordion',
                    'accordionMenu',
                    'drilldown',
                    'dropdown',
                    'dropdownMenu',
                    'equalizer',
                    'interchange',
                    'magellan',
                    'offcanvas',
                    'orbit',
                    'responsiveMenu',
                    'responsiveToggle',
                    'reveal',
                    'slider',
                    'sticky',
                    'tabs',
                    'toggler',
                    'tooltip'
                ]
            },
        }
      });
    
      return app.toTree();
    };

## Ember Bootstrap sample form
    {{#bs-form model=this onSubmit=(action "submit") as |form|}}
      {{#form.element label="Email" placeholder="Email" property="email" as |el|}}
        <div class="input-group">
          <input value={{el.value}} class="form-control" placeholder="Email" oninput={{action (mut el.value) value="target.value"}} onchange={{action (mut el.value) value="target.value"}} id={{el.id}} type="text">
          <span class="input-group-addon">@example.com</span>
        </div>
      {{/form.element}}
      {{bs-button defaultText="Submit" type="primary" buttonType="submit"}}
    {{/bs-form}}

## Install ember-bootstrap with default version
    ember install ember-bootstrap

## Instal ember-bootstrap with version 4 and SASS - experimental
    ember generate ember-bootstrap --bootstrap-version=4 --preprocessor=sass



## Install SASS and Foundation
    npm install --save-dev ember-cli-sass
    ember install ember-cli-foundation-6-sass

## Install Foundation dependencies
    ember g ember-cli-foundation-6-sass



