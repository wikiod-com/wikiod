---
title: "Workaround for 'show-delete' in <ion-list> deprecation"
slug: "workaround-for-show-delete-in-ion-list-deprecation"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Solution
I am developing a mobile app using ionic 2 with Angular 2. 

I have an ion-list filled ion-items. I want those ion-item to have the ability to be deleted if needed as presented [here][1] on the ionic website.

However, a lot have changed in **ionic 2** since the first version and the above style of one button opening all the **ion-item** at one is not possible anymore since the **show-delete** and **show-reorder** are no longer supported. The only option available is to have **ion-item-sliding** as ion-item, which gives us the ability to slide each item one at a time in order to reveal the delete button.

That is not what I wanted. I wanted one button that opens all ion-item at the same time.

After spending some time on that, I came up with a working solution and managed to achieve the desired outcome using ionic 2, and I am going to share it with you.

Here is my solution: 

In the .html file:

<!-- language: lang-html-->
    <ion-header>
      <ion-navbar>
        <ion-buttons start (click)="manageSlide()">
          <button>
            <ion-icon name="ios-remove"></ion-icon>
          </button>
        </ion-buttons>
        <ion-title>PageName</ion-title>
      </ion-navbar>
    </ion-header>

and for the list:
<!-- language: lang-html -->

    <ion-list #list1>
      <ion-item-sliding #slidingItem *ngFor="let contact of contacts | sortOrder">
        <button #item ion-item>
          <p>{{ item.details }}</p>
          <ion-icon id="listIcon" name="arrow-forward" item-right></ion-icon>
        </button>
        <ion-item-options side="left">
          <button danger (click)="doConfirm(contact, slidingItem)">
            <ion-icon name="ios-remove-circle-outline"></ion-icon>
          Remove
          </button>
        </ion-item-options>
      </ion-item-sliding>
    </ion-list>

In the **.ts** file, first do your imports:
<!-- language: lang-js -->

    import { ViewChild } from '@angular/core';
    import { Item } from 'ionic-angular';
    import { ItemSliding, List } from 'ionic-angular';

then refer to the html element by declaring a ViewChild:
<!-- language: lang-js -->

    @ViewChild(List) list: List;

Finally, add your classes to handle the work:
<!-- language: lang-js -->

    public manageSlide() {

        //loop through the list by the number retreived of the number of ion-item-sliding in the list
        for (let i = 0; i < this.list.getElementRef().nativeElement.children.length; i++) {

            // retreive the current ion-item-sliding
            let itemSlide = this.list.getElementRef().nativeElement.children[i].$ionComponent;

            // retreive the button to slide within the ion-item-sliding
            let item = itemSlide.item;

            // retreive the icon
            let ic = item._elementRef.nativeElement.children[0].children[1];

            if (this.deleteOpened) {
                this.closeSlide(itemSlide);
            } else {
                this.openSlide(itemSlide, item, ic);
            } 
        }

        if (this.deleteOpened) {
            this.deleteOpened = false;
        } else {
            this.deleteOpened = true;
        } 
    }

Then the opening class:
<!-- language: lang-js -->

    private openSlide(itemSlide: ItemSliding, item: Item, inIcon) {
      itemSlide.setCssClass("active-sliding", true);
      itemSlide.setCssClass("active-slide", true);
      itemSlide.setCssClass("active-options-left", true);
      item.setCssStyle("transform", "translate3d(72px, 0px, 0px)")
    }

And the closing class:
<!-- language: lang-js -->

    private closeSlide(itemSlide: ItemSliding) {
      itemSlide.close();
      itemSlide.setCssClass("active-sliding", false);
      itemSlide.setCssClass("active-slide", false);
      itemSlide.setCssClass("active-options-left", false);
  }

I hope it will help some you out there.

Enjoy and good coding...

  [1]: http://ionicframework.com/docs/api/directive/ionList/

