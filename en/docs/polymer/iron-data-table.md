---
title: "iron-data-table"
slug: "iron-data-table"
draft: false
images: []
weight: 9843
type: docs
toc: true
---

## Hello world
Initial starting point for `iron-data-table`.

[Working jsBin][1]

    <!DOCTYPE html>
    <html>  
      <head>
        <base href="https://polygit.org/polymer+:master/iron-data-table+Saulis+:master/components/">
        <link rel="import" href="polymer/polymer.html">
        
        <script src="webcomponentsjs/webcomponents-lite.min.js"></script>
       
        <link rel="import" href="iron-ajax/iron-ajax.html">
        <link rel="import" href="paper-button/paper-button.html">
        
        <link rel="import" href="iron-data-table/iron-data-table.html">
      </head>
      <body>
        <dom-module id="x-foo">
          <template>
            <style>
            </style>
            <paper-button on-tap="msg">Click Me</paper-button>
    
            <iron-ajax auto
              url="https://saulis.github.io/iron-data-table/demo/users.json" 
              last-response="{{users}}"
              >
            </iron-ajax>
            <iron-data-table selection-enabled items="[[users.results]]">
              <data-table-column name="Picture" width="50px" flex="0">
                <template>
                  <img src="[[item.user.picture.thumbnail]]">
                </template>
              </data-table-column>
              <data-table-column name="First Name">
                <template>[[item.user.name.first]]</template>
              </data-table-column>
              <data-table-column name="Last Name">
                <template>[[item.user.name.last]]</template>
              </data-table-column>
              <data-table-column name="Email">
                <template>[[item.user.email]]</template>
              </data-table-column>
            </iron-data-table>
    
          </template>
          <script>
            (function(){
              'use strict';
              Polymer({
                is: 'x-foo',
                msg: function() {
                  console.log('This proves Polymer is working!');
                },
              });
            })();
          </script>
        </dom-module>
        <x-foo></x-foo>
      </body>
    </html>


  [1]: http://jsbin.com/torizajafe/1/edit?html,output

## CSS import
Import external style sheet.

[Working jsBin][1]

    <!DOCTYPE html>
    <html>  
      <head>
        <base href="https://polygit.org/polymer+:master/iron-data-table+Saulis+:master/components/">
        <link rel="import" href="polymer/polymer.html">
        
        <script src="webcomponentsjs/webcomponents-lite.min.js"></script>
       
        <link rel="import" href="iron-ajax/iron-ajax.html">
        <link rel="import" href="paper-button/paper-button.html">
        
        <link rel="import" href="iron-data-table/iron-data-table.html">
        <link rel="import" href="iron-data-table/default-styles.html">
      </head>
      <body>
        <dom-module id="x-foo">
          <template>
            <style>
            </style>
            <paper-button on-tap="msg">Click Me</paper-button>
    
            <iron-ajax auto
              url="https://saulis.github.io/iron-data-table/demo/users.json" 
              last-response="{{users}}"
              >
            </iron-ajax>
            <iron-data-table selection-enabled items="[[users.results]]">
              <data-table-column name="Picture" width="50px" flex="0">
                <template>
                  <img src="[[item.user.picture.thumbnail]]">
                </template>
              </data-table-column>
              <data-table-column name="First Name">
                <template>[[item.user.name.first]]</template>
              </data-table-column>
              <data-table-column name="Last Name">
                <template>[[item.user.name.last]]</template>
              </data-table-column>
              <data-table-column name="Email">
                <template>[[item.user.email]]</template>
              </data-table-column>
            </iron-data-table>
    
          </template>
          <script>
            (function(){
              'use strict';
              Polymer({
                is: 'x-foo',
                msg: function() {
                  console.log('This proves Polymer is working!');
                },
              });
            })();
          </script>
        </dom-module>
        <x-foo></x-foo>
      </body>
    </html>

  [1]: http://jsbin.com/yexokijavo/1/edit?html,output

## Row details
Expand row details to display additional data.

[Working jsBin][1]

    <!DOCTYPE html>
    <html>  
      <head>
        <base href="https://polygit.org/polymer+:master/iron-data-table+Saulis+:master/components/">
        <link rel="import" href="polymer/polymer.html">
        
        <script src="webcomponentsjs/webcomponents-lite.min.js"></script>
       
        <link rel="import" href="iron-ajax/iron-ajax.html">
        <link rel="import" href="paper-button/paper-button.html">
        
        <link rel="import" href="iron-data-table/iron-data-table.html">
        <link rel="import" href="iron-data-table/default-styles.html">
      </head>
      <body>
        <dom-module id="x-foo">
          <template>
            <style>
              #grid1 data-table-row-detail {
                height: 100px;
              }
              #grid1 .detail {
                width: 100%;
                display: flex;
                justify-content: space-around;
                align-items: center;
                border: 2px solid #aaa;
              }
            </style>
            <paper-button on-tap="msg">Click Me</paper-button>
    
            <iron-ajax auto
              url="https://saulis.github.io/iron-data-table/demo/users.json" 
              last-response="{{users}}"
              >
            </iron-ajax>
            <iron-data-table id="grid1" details-enabled items="[[users.results]]">
              <template is="row-detail">
                <div class="detail">
                  <img src="[[item.user.picture.medium]]">
                  <p>[[item.user.username]]</p>
                  <p>[[item.user.email]]</p>
                </div>
              </template>
              <data-table-column name="First Name">
                <template>[[item.user.name.first]]</template>
              </data-table-column>
              <data-table-column name="Last Name">
                <template>[[item.user.name.last]]</template>
              </data-table-column>
            </iron-data-table>
          </template>
          <script>
            (function(){
              'use strict';
              Polymer({
                is: 'x-foo',
                msg: function() {
                  console.log('This proves Polymer is working!');
                },
              });
            })();
          </script>
        </dom-module>
        <x-foo></x-foo>
      </body>
    </html>


  [1]: http://jsbin.com/fanotababa/1/edit?html,output

## Edit row details
[Working jsBin][1]

    <!DOCTYPE html>
    <html>  
      <head>
        <base href="https://polygit.org/polymer+:master/iron-data-table+Saulis+:master/components/">
        <link rel="import" href="polymer/polymer.html">
        
        <script src="webcomponentsjs/webcomponents-lite.min.js"></script>
       
        <link rel="import" href="iron-ajax/iron-ajax.html">
        <link rel="import" href="paper-button/paper-button.html">
        <link rel="import" href="paper-input/paper-input.html">
        
        <link rel="import" href="iron-data-table/iron-data-table.html">
        <link rel="import" href="iron-data-table/default-styles.html">
      </head>
      <body>
        <dom-module id="x-foo">
          <template>
            <style>
              #grid1 data-table-row-detail {
                height: 100px;
              }
              #grid1 .detail {
                width: 100%;
                display: flex;
                justify-content: space-around;
                align-items: center;
                border: 2px solid #aaa;
              }
            </style>
            <paper-button on-tap="msg">Click Me</paper-button>
    
            <iron-ajax auto
              url="https://saulis.github.io/iron-data-table/demo/users.json" 
              last-response="{{users}}"
              >
            </iron-ajax>
            <iron-data-table id="grid1" details-enabled items="[[users.results]]">
              <template is="row-detail">
                <div class="detail">
                  <img src="[[item.user.picture.medium]]">
                  <p>[[item.user.username]]</p>
                  <p>[[item.user.email]]</p>
                  <paper-input></paper-input>
                </div>
              </template>
              <data-table-column name="First Name">
                <template>[[item.user.name.first]]</template>
              </data-table-column>
              <data-table-column name="Last Name">
                <template>[[item.user.name.last]]</template>
              </data-table-column>
            </iron-data-table>
          </template>
          <script>
            (function(){
              'use strict';
              Polymer({
                is: 'x-foo',
                msg: function() {
                  console.log('This proves Polymer is working!');
                },
              });
            })();
          </script>
        </dom-module>
        <x-foo></x-foo>
      </body>
    </html>


  [1]: http://jsbin.com/boxiyafoya/1/edit?html,output

## Edit row details using sub-element
This example uses a separate element to edit bound data to the `row-detail` template.

[Working jsBin][1]

    <!DOCTYPE html>
    <html>  
      <head>
        <base href="https://polygit.org/polymer+:master/iron-data-table+Saulis+:master/components/">
        <link rel="import" href="polymer/polymer.html">
        
        <script src="webcomponentsjs/webcomponents-lite.min.js"></script>
       
        <link rel="import" href="iron-ajax/iron-ajax.html">
        <link rel="import" href="paper-button/paper-button.html">
        <link rel="import" href="paper-input/paper-input.html">
        
        <link rel="import" href="iron-data-table/iron-data-table.html">
        <link rel="import" href="iron-data-table/default-styles.html">
      </head>
      <body>
        <dom-module id="row-detail">
          <template>
            <img src="[[item.user.picture.medium]]">
            <span>[[item.user.username]]</span>
            <span>[[item.user.email]]</span>
            <paper-input></paper-input>
          </template>
          <script>
            (function() {
              'use strict';
              Polymer({
                is: 'row-detail',
                properties: {
                  item: Object,
                },        
              });
            })();
          </script>
        </dom-module>
        <dom-module id="x-foo">
          <template>
            <style>
              #grid1 data-table-row-detail {
                height: 150px;
              }
              #grid1 .detail {
                width: 100%;
                display: flex;
                justify-content: space-around;
                align-items: center;
                border: 2px solid #aaa;
              }
            </style>
            <paper-button on-tap="msg">Click Me</paper-button>
    
            <iron-ajax auto
              url="https://saulis.github.io/iron-data-table/demo/users.json" 
              last-response="{{users}}"
              >
            </iron-ajax>
            <iron-data-table id="grid1" details-enabled items="[[users.results]]">
              <template is="row-detail">
                <div class="detail">
                  <row-detail item="{{item}}"></row-detail>
                </div>
              </template>
              <data-table-column name="First Name">
                <template>[[item.user.name.first]]</template>
              </data-table-column>
              <data-table-column name="Last Name">
                <template>[[item.user.name.last]]</template>
              </data-table-column>
            </iron-data-table>
          </template>
          <script>
            (function(){
              'use strict';
              Polymer({
                is: 'x-foo',
                msg: function() {
                  console.log('This proves Polymer is working!');
                },
              });
            })();
          </script>
        </dom-module>
        <x-foo></x-foo>
      </body>
    </html>

Note
-
[There is currently an issue described here][2] that causes row details section to collapse if it contains a sub-element. The patch is to include `tabindex="0"` as follows. ([See this Stack Overflow answer][3].)

<h4>x-foo.html</h4>

    <template is="row-detail">
      <div class="detail">
        <row-detail item="{{item}}" tabindex="0"></row-detail>
      </div>
    </template>


  [1]: http://jsbin.com/sevafikolu/1/edit?html,output
  [2]: http://stackoverflow.com/q/38693018/1640892
  [3]: http://stackoverflow.com/a/38693675/1640892

## Select row, prevent deselection
Default behavior is to de-select row when clicked twice. In some use cases, you might want to disable this de-selecting behavior.

Note
-
`table.deselectItem(item)` method will imperatively deselect an item. This works with both `item` or `index` (when using items array) as an argument.

[Working jsBin][1]

    <!DOCTYPE html>
    <html>  
      <head>
        <base href="https://polygit.org/polymer+:master/iron-data-table+Saulis+:master/components/">
        <link rel="import" href="polymer/polymer.html">
        
        <script src="webcomponentsjs/webcomponents-lite.min.js"></script>
       
        <link rel="import" href="iron-ajax/iron-ajax.html">
        <link rel="import" href="paper-button/paper-button.html">
        
        <link rel="import" href="iron-data-table/iron-data-table.html">
        <link rel="import" href="iron-data-table/default-styles.html">
      </head>
      <body>
        <x-foo></x-foo>
        <dom-module id="x-foo">
          <template>
            <style>
            </style>
            [[_computeSelectedStr(selectedItem)]]
    
            <iron-ajax
                auto
                url="https://saulis.github.io/iron-data-table/demo/users.json" 
                last-response="{{users}}"
                >
            </iron-ajax>
            <iron-data-table id="grid"
                             selection-enabled
                             on-deselecting-item="_deselecting"
                             items="[[users.results]]"
                             selected-item="{{selectedItem}}"
                             >
              <data-table-column name="Picture" width="50px" flex="0">
                <template>
                  <img src="[[item.user.picture.thumbnail]]">
                </template>
              </data-table-column>
              <data-table-column name="First Name">
                <template>[[item.user.name.first]]</template>
              </data-table-column>
              <data-table-column name="Last Name">
                <template>[[item.user.name.last]]</template>
              </data-table-column>
              <data-table-column name="Email">
                <template>[[item.user.email]]</template>
              </data-table-column>
            </iron-data-table>
    
          </template>
          <script>
            (function(){
                  'use strict';
              Polymer({
                is: 'x-foo',
                observers: [
                  '_selectedItemChanged(selectedItem)' ,
                ],
                _selectedItemChanged: function(ob) {
                  console.log('selectedItem', ob);
                },
                _computeSelectedStr: function(ob) {
                  return JSON.stringify(ob);
                },
                _deselecting: function(e) {
                  e.preventDefault();
                }
              });
            })();
          </script>
        </dom-module>
      </body>
    </html>


  [1]: http://jsbin.com/fewihuxozu/1/edit?html,output

