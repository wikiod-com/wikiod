---
title: "Modals"
slug: "modals"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

Modals require bootstrap.min.js to function properly.

More details can be found here: http://getbootstrap.com/javascript/#modals

## Basic HTML Modal
A modal is a dialog window which can be displayed over the current page.

    <!-- Clicking the button will open the modal window -->
    <button type="button" class="btn btn-success btn-lg" data-toggle="modal" data-target="#theModal">Open The Modal</button>
    
    <!-- The Modal -->
    <div id="theModal" class="modal fade" role="dialog">
        <div class="modal-dialog">
            <div class="modal-content">
                <div class="modal-header">
                    <button type="button" class="close" data-dismiss="modal">&times;</button>
                    <h4 class="modal-title">Text For The Modal Header</h4>
                </div>
                <div class="modal-body">
                    <p>Text for The Modal Body.</p>
                </div>
                <div class="modal-footer">
                    <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
                </div>
            </div>
        </div>
    </div>

