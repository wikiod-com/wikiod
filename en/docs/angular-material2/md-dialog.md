---
title: "md-dialog"
slug: "md-dialog"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

This topic includes examples of `md-dialog`.

To find more details on `md-dialog`, please check the documentation [here][1].


  [1]: https://material.angular.io/components/dialog/overview

## Initialize md-dialog with data passed from parent component
This example requires `MdDialogRef` and `MD_DIALOG_DATA`. Please import them in the component module.
<!-- language: typescript -->

    import {MdDialog, MdDialogRef, MD_DIALOG_DATA} from '@angular/material';

**input-overview-example.html:**
<!-- language: lang-html -->

    <md-input-container>
      <input mdInput 
             [(ngModel)]="id"
             placeholder="Value passed to md-dialog">
    </md-input-container>
    
    <p></p>
    
    <button md-raised-button
         (click)="openDialog(id)">
      Open Dialog
    </button>


**input-overview-example.ts:**
<!-- language: typescript -->

    import {Component, Inject, Input, OnInit } from '@angular/core';
    import {MdDialog, MdDialogRef, MD_DIALOG_DATA} from '@angular/material';
    
    @Component({
      selector: 'input-overview-example',
      templateUrl: 'input-overview-example.html'
    })
    export class InputOverviewExample {
      
      id: any;
      
      @Input() isChecked: boolean;
      
      constructor(public dialog: MdDialog) {}
    
        openDialog(value) {
          let dialogRef = this.dialog.open(DialogResultExampleDialog, {
            data: {
              id: value
            }
          });
          dialogRef.afterClosed().subscribe(result => {
            console.log(result);
          });
        }
    }
    
    @Component({
      selector: 'dialog-result-example-dialog',
      template: `<p md-dialog-title>Confirm Toggle </p>
                 <p md-dialog-content>Id passed from component: {{ this.passedId }}</p>
                 <md-dialog-actions>
                    <button md-button color="primary" (click)="dialogRef.close('Cancel')">Cancel</button>
                    <button md-button color="primary" (click)="dialogRef.close('continue')">Continue</button>
                </md-dialog-actions>
              `,
    })
    export class DialogResultExampleDialog implements OnInit {
      
      passedId: string;
      
      constructor(@Inject(MD_DIALOG_DATA) private data: { id: string }, 
                  public dialogRef: MdDialogRef<DialogResultExampleDialog>) {}
                  
      ngOnInit(){
        this.passedId = this.data.id;  
      }
    }


[Live demo][1]


  [1]: https://plnkr.co/edit/a9RBS1?p=preview

