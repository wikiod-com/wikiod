---
title: "md-datepicker"
slug: "md-datepicker"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

This topic focuses on examples related to md-datepicker.

For more details, please check the `md-datepicker` documentation [here][1].


  [1]: https://material.angular.io/components/datepicker/overview

## Data binding with md-datapicker
**datepicker-overview-example.html:**
<!-- language: lang-html -->

    <md-input-container>
      <input mdInput 
             [mdDatepicker]="picker" 
             [(ngModel)]="date"
             placeholder="Choose a date">
      <button mdSuffix [mdDatepickerToggle]="picker"></button>
    </md-input-container>
    <md-datepicker  #picker></md-datepicker>
    <div>
      Date Chosen using 'ngModel':
      <div>{{ date }}</div>
    </div>


**datepicker-overview-example.ts:**
<!-- language: typescript -->

    import {Component, OnInit} from '@angular/core';
    
    @Component({
      selector: 'datepicker-overview-example',
      templateUrl: 'datepicker-overview-example.html'
    })
    export class DatepickerOverviewExample implements OnInit {
      
      date;
      
      ngOnInit(){
        this.date = new Date();
      }
      
    }



[Live demo][1]


  [1]: https://plnkr.co/edit/ucGODHnqlaRi7IgoJNmt?p=preview

## Passing selected date value to a function using $event
**datepicker-overview-example.html:**
<!-- language: lang-html -->

    <md-input-container>
        <input mdInput [mdDatepicker]="picker" placeholder="Choose a date" [(ngModel)]="value">
        <button mdSuffix [mdDatepickerToggle]="picker"></button>
    </md-input-container>
    <md-datepicker #picker [startAt]="startDate" (selectedChanged)="selectedDate($event)"></md-datepicker>
    
    <p>ngModel Value: {{value}}</p>
    
    <p>Date from selectedDate(): {{checkDate}}</p>


**datepicker-overview-example.ts:**
<!-- language: typescript -->

    import {Component} from '@angular/core';
    
    @Component({
      selector: 'datepicker-overview-example',
      templateUrl: 'datepicker-overview-example.html'
    })
    export class DatepickerOverviewExample {
      
      value: Date = new Date();
      
      checkDate: Date;
      
      selectedDate(date){
       // ngModel still returns the old value
          console.log("ngModel: " + this.value);
          
       // date passes the newly selected value  
          console.log("Selected Value: " + date);
          this.checkDate = date;
      }
    }



[Live demo][1]


  [1]: https://plnkr.co/edit/05yGj6Zbt4OYIfD3DcON?p=preview

## Open datepicker on focus
This example also includes the use of properties:

- min
- max
- startAt
- startView
- touchUi

**datepicker-overview-example.html:**
<!-- language: lang-html -->

    <h2>Options</h2>
    <p>
      <md-checkbox [(ngModel)]="touch">Use touch UI</md-checkbox>
      <md-checkbox [(ngModel)]="filterOdd">Filter odd months and dates</md-checkbox>
      <md-checkbox [(ngModel)]="yearView">Start in year view</md-checkbox>
    </p>
    <p>
      <md-input-container>
        <input mdInput [mdDatepicker]="minDatePicker" [(ngModel)]="minDate" placeholder="Min date" (keydown)="false" (click)="minDatePicker.open()">
        <button mdSuffix [mdDatepickerToggle]="minDatePicker"></button>
      </md-input-container>
      <md-datepicker #minDatePicker [touchUi]="touch"></md-datepicker>
      <md-input-container>
        <input mdInput [mdDatepicker]="maxDatePicker" [(ngModel)]="maxDate" placeholder="Max date" (keydown)="false" (focus)="maxDatePicker.open()">
        <button mdSuffix [mdDatepickerToggle]="maxDatePicker"></button>
      </md-input-container>
      <md-datepicker #maxDatePicker [touchUi]="touch"></md-datepicker>
    </p>
    <p>
      <md-input-container>
        <input mdInput [mdDatepicker]="startAtPicker" [(ngModel)]="startAt" placeholder="Start at date" (keydown)="false" (focus)="startAtPicker.open()">
        <button mdSuffix [mdDatepickerToggle]="startAtPicker"></button>
      </md-input-container>
      <md-datepicker #startAtPicker [touchUi]="touch"></md-datepicker>
    </p>
    
    <h2>Result</h2>
    
    <p>
      <button [mdDatepickerToggle]="resultPicker"></button>
      <md-input-container>
        <input mdInput
               #resultPickerModel="ngModel"
               [mdDatepicker]="resultPicker"
               [(ngModel)]="date"
               [min]="minDate"
               [max]="maxDate"
               [mdDatepickerFilter]="filterOdd ? dateFilter : null"
               placeholder="Pick a date"
               (keydown)="false"
               (focus)="resultPicker.open()">
        <md-error *ngIf="resultPickerModel.hasError('mdDatepickerMin')">Too early!</md-error>
        <md-error *ngIf="resultPickerModel.hasError('mdDatepickerMax')">Too late!</md-error>
        <md-error *ngIf="resultPickerModel.hasError('mdDatepickerFilter')">Date unavailable!</md-error>
      </md-input-container>
      <md-datepicker
          #resultPicker
          [touchUi]="touch"
          [startAt]="startAt"
          [startView]="yearView ? 'year' : 'month'">
      </md-datepicker>
    </p>
    <p>
      <input [mdDatepicker]="resultPicker2"
             [(ngModel)]="date"
             [min]="minDate"
             [max]="maxDate"
             [mdDatepickerFilter]="filterOdd ? dateFilter : null"
             (focus)="resultPicker2.open()"
             placeholder="Pick a date"
             (keydown)="false">
      <button [mdDatepickerToggle]="resultPicker2"></button>
      <md-datepicker
          #resultPicker2
          [touchUi]="touch"
          [startAt]="startAt"
          [startView]="yearView ? 'year' : 'month'">
      </md-datepicker>
    </p>


**datepicker-overview-example.ts:**
<!-- language: typescript -->

    import {Component, OnInit} from '@angular/core';
    import {MdDatepicker} from '@angular/material';
    
    @Component({
      selector: 'datepicker-overview-example',
      templateUrl: 'datepicker-overview-example.html'
    })
    export class DatepickerOverviewExample implements OnInit {
      
      touch: boolean;
      filterOdd: boolean;
      yearView: boolean;
      minDate: Date;
      maxDate: Date;
      startAt: Date;
      date: Date;
      dateFilter = (date: Date) => date.getMonth() % 2 == 1 && date.getDate() % 2 == 0;
      
    }



[Live demo][1]


  [1]: https://plnkr.co/edit/sE8hFm7QL2OmfuSQ57yw?p=preview

## Set different locale for md-datepicker
This example requires importing `DateAdapter`.

    import {DateAdapter} from '@angular/material';

**datepicker.component.html:**
<!-- language: lang-html -->

    <md-input-container>
      <input mdInput [mdDatepicker]="picker" placeholder="Choose a date">
      <button mdSuffix [mdDatepickerToggle]="picker"></button>
    </md-input-container>
    <md-datepicker #picker></md-datepicker>
    
    <p></p>
    <div>
      <button md-raised-button (click)="setLocale('en')">English - US</button>
      
      <button md-raised-button (click)="setLocale('es')">Spanish</button>
    
      <button md-raised-button (click)="setLocale('zh')">Chinese</button>
    
      <button md-raised-button (click)="setLocale('nl')">Dutch</button>
      
      <button md-raised-button (click)="setLocale('bn')">Bengali</button>
      
      <button md-raised-button (click)="setLocale('hi')">Hindi</button>
      
      <button md-raised-button (click)="setLocale('ar')">Arabic</button>
    </div>



**datepicker.component.ts:**
<!-- language: typescript -->

    import {Component} from '@angular/core';
    import {DateAdapter} from '@angular/material';
    
    @Component({
      selector: 'datepicker-overview-example',
      templateUrl: './datepicker-overview-example.html',
      styleUrls: ['./datepicker-overview-example.css'],
    })
    export class DatepickerOverviewExample {
      
      constructor(private dateAdapter: DateAdapter<Date>) {
        this.dateAdapter.setLocale('en');   
      }
      
      setLocale(val){
        console.log(val);
        this.dateAdapter.setLocale(val); 
      }
      
    }


[Live demo][1]

A list of locale language code can be found [here][2].


  [1]: https://plnkr.co/edit/tpwMyFkCiC4PQ3yapst8?p=preview
  [2]: http://www.science.co.il/language/Locale-codes.php

