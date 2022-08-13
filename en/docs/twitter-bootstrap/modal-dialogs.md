---
title: "Modal Dialogs"
slug: "modal-dialogs"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

For more information, visit the official documentation at http://getbootstrap.com/javascript/#modals, where the 'Basic HTML Usage' example was derived from.

## Basic HTML usage
A Bootstrap modal dialog is a Bootstrap component which creates a modal dialog window which floats over page-level content.

Here is an example of the basic usage of a Bootstrap modal dialog in HTML:

    <div class="modal fade" tabindex="-1" role="dialog">
      <div class="modal-dialog">
        <div class="modal-content">
          <div class="modal-header">
            <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
            <h4 class="modal-title">Modal title</h4>
          </div>
          <div class="modal-body">
            <p>One fine body&hellip;</p>
          </div>
          <div class="modal-footer">
            <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
            <button type="button" class="btn btn-primary">Save changes</button>
          </div>
        </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
    </div><!-- /.modal -->

## Basic Javascript usage and initialization
Modal dialog components can be instantiated via jQuery with the function `$('#myModal').modal(options)`, where `$('#myModal')` is a top-level reference to the specific modal dialog and `options` is a Javascript object specifying the modal dialog's default attributes.

The `options` object allows for multiple properties to be defined which will affect how the modal dialog behaves. These properties are defined as such:

 - The `backdrop` property allows a user to define whether or not they want a grey background overlay to appear behind the modal dialog.  Both boolean values and the string "static" are recognized.  If "static" is specified, the modal dialog will not be closed when a user clicks on the background overlay. 
 - The `keyboard` property allows a user to define whether or not they want the modal dialog to be closed when the escape key is pressed on the keyboard.
 - The `show` property allows a user to define whether or not they want the modal dialog to appear when the modal is initialized.

Here is an example of the basic Javascript usage:

    $('#carModal').modal({ backdrop: false, keyboard: true, show: false });

As with other Bootstrap components, the modal's options can also be specified in HTML via data attributes.

