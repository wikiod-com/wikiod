---
title: "md-autocomplete"
slug: "md-autocomplete"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

This topic includes coding examples related to Angular Material 2 Autocomplete (md-autocomplete)

These examples don't cover all features of md-autocomplete. Please read the [documentation][1] learn more about md-autocomplete.


  [1]: https://material.angular.io/components/autocomplete/overview

## Separate control and display
This example shows how to to display specific property in dropdown but bind with the whole object.

**autocomplete-overview-example.html:**

    <md-input-container>
      <input mdInput placeholder="State" [(ngModel)]="selection"
             [mdAutocomplete]="auto" [formControl]="stateCtrl">
    </md-input-container>
    
    <md-autocomplete #auto="mdAutocomplete" [displayWith]="displayFn">
      <md-option *ngFor="let state of filteredStates | async" [value]="state" >
        {{ state.Country }}
      </md-option>
    </md-autocomplete>
    
    
    <p>Selected Country: {{selection | json}}</p>
    <p>Selected Country Id: {{selection?.CountryID}}</p>

**autocomplete-overview-example.ts:**

    import {Component} from '@angular/core';
    import {FormControl} from '@angular/forms';
    
    import 'rxjs/add/operator/startWith';
    import 'rxjs/add/operator/map';
    
    @Component({
      selector: 'autocomplete-overview-example',
      templateUrl: 'autocomplete-overview-example.html',
    })
    export class AutocompleteOverviewExample {
      stateCtrl: FormControl;
      filteredStates: any;
      
      selection: any;
    
      states = [
        { Country: "United States Of America" , CountryID: "1"},
        { Country: "United Kingdom" , CountryID: "2"},
        { Country: "United Arab Emirates" , CountryID: "3"},
      ];
    
      constructor() {
        this.stateCtrl = new FormControl();
        this.filteredStates = this.stateCtrl.valueChanges
            .startWith(null)
            .map(country => country && typeof country === 'object' ? country.Country : country)
            .map(name => this.filterStates(name));
      }
    
      filterStates(val) {
        return val ? this.states.filter(s => s.Country.toLowerCase().indexOf(val.toLowerCase()) === 0)
                   : this.states;
      }
      
      displayFn(country): string {
        console.log(country);
          return country ? country.Country : country;
       }
    
    }

[Live Example][1]


  [1]: https://plnkr.co/edit/5wrr1YGxBzAFPj0sJgzT?p=info

## Get md-autocomplete's options/searchable data from API
data.service.ts:

    import { Injectable } from '@angular/core';
    import { Http } from '@angular/http';
    import 'rxjs/add/operator/map';
    
    @Injectable()
    export class DataService {
    
      constructor(private http: Http) { }
    
      fetchData(){
        return this.http.get('https://dinstruct-d4b62.firebaseio.com/.json')
          .map((res) => res.json())
        
      }
    
    }

**autocomplete-overview-example.html:**

    <md-input-container>
      <input mdInput placeholder="Name" [mdAutocomplete]="auto" [formControl]="stateCtrl">
    </md-input-container>
    
    <md-autocomplete #auto="mdAutocomplete" [displayWith]="displayFn">
      <md-option *ngFor="let sector of filteredSectors | async" [value]="sector">
        {{ sector.name }}
      </md-option>
    </md-autocomplete>
    
    <div>
        <h2>Data :</h2>
        <span>{{ allSectors | json  }}</span>
    </div>

**autocomplete-overview-example.ts:**

    import {Component, OnInit} from '@angular/core';
    import {FormControl} from '@angular/forms';
    
    import { DataService } from './data.service'; 
    import 'rxjs/add/operator/startWith';
    import 'rxjs/add/operator/map';
    
    @Component({
      selector: 'autocomplete-overview-example',
      templateUrl: './autocomplete-overview-example.html',
    })
    export class AutocompleteOverviewExample implements OnInit{
      stateCtrl: FormControl;
      
      filteredSectors: any;
      
      allSectors;
    
      constructor(private dataService: DataService) {
        this.stateCtrl = new FormControl();
      }
      
      ngOnInit(){
        this.dataService.fetchData()
          .subscribe(
            (data) => {
              this.allSectors = data.customers;
              this.filteredSectors = this.stateCtrl.valueChanges
                .startWith(null)
                .map(val => val ? this.filter(val) : this.allSectors.slice());
            }
        );
        
      }
    
      filter(name) {
       return this.allSectors.filter(sector => new RegExp(`^${name}`, 'gi').test(sector.name)); 
      }
      
       displayFn(sector) {
          return sector ? sector.name : sector;
       }
    
    }

[Live Example][1]


  [1]: https://plnkr.co/edit/aa2NNOHdKhB7n8iJcQbT?p=preview

## Utilize md-autocomplete inside a reactive form
This example requires `FormsModule` and `ReactiveFormsModule`. Please import them in your application/module.

    import {FormsModule, ReactiveFormsModule} from '@angular/forms';

**input-form-example.html**

    <form class="example-form" (ngSubmit)="submit(addForm.value)" [formGroup]="addForm">
      <md-input-container class="example-full-width">
        <input mdInput placeholder="Company (disabled)" disabled value="Google" formControlName="company">
      </md-input-container>
    
      <table class="example-full-width" cellspacing="0"><tr>
        <td><md-input-container class="example-full-width">
          <input mdInput placeholder="First name" formControlName="fname">
        </md-input-container></td>
        <td><md-input-container class="example-full-width">
          <input mdInput placeholder="Long Last Name That Will Be Truncated">
        </md-input-container></td>
      </tr></table>
    
      <p>
        <md-input-container class="example-full-width">
          <textarea mdInput placeholder="Address" formControlName="address">1600 Amphitheatre Pkwy</textarea>
        </md-input-container>
        <md-input-container class="example-full-width">
          <textarea mdInput placeholder="Address 2"></textarea>
        </md-input-container>
      </p>
    
      <table class="example-full-width" cellspacing="0"><tr>
        <td><md-input-container class="example-full-width">
          <input mdInput placeholder="City" formControlName="city">
        </md-input-container></td>
        
        <td><md-input-container>
          <input mdInput placeholder="State" 
                [mdAutocomplete]="auto"
                [formControl]="stateCtrl"
                formControlName="state">
        </md-input-container></td>
        
        <td><md-input-container class="example-full-width">
          <input mdInput #postalCode maxlength="5" placeholder="Postal Code" value="94043" formControlName="zip">
          <md-hint align="end">{{postalCode.value.length}} / 5</md-hint>
        </md-input-container></td>
      </tr></table>
      
       <button md-raised-button  type="submit">Submit</button>
       
       <md-autocomplete #auto="mdAutocomplete" >
        <md-option *ngFor="let state of filteredStates | async" [value]="state" (onSelectionChange)="selectState(state, addForm.value)">
          {{ state }}
        </md-option>
      </md-autocomplete>
       
    </form>
    
    <p>Form values:</p> 
    <p>{{ addForm.value | json }}</p>

**input-form-example.ts:**

    import {Component} from '@angular/core';
    import {FormBuilder, FormGroup, FormControl} from '@angular/forms';
    import 'rxjs/add/operator/startWith';
    import 'rxjs/add/operator/map';
    
    @Component({
      selector: 'input-form-example',
      templateUrl: 'input-form-example.html',
      styleUrls: ['input-form-example.css'],
    })
    export class InputFormExample {
      stateCtrl: FormControl;
      filteredStates: any;
      
      addForm: FormGroup;
      
      state;
    
      states = [
        'Alabama',
        'Alaska',
        'Arizona',
        'Arkansas',
        'California',
        'Colorado',
        'Connecticut',
        'Delaware',
        'Florida',
        'Georgia',
        'Hawaii',
        'Idaho',
        'Illinois',
        'Indiana',
        'Iowa',
        'Kansas',
        'Kentucky',
        'Louisiana',
        'Maine',
        'Maryland',
        'Massachusetts',
        'Michigan',
        'Minnesota',
        'Mississippi',
        'Missouri',
        'Montana',
        'Nebraska',
        'Nevada',
        'New Hampshire',
        'New Jersey',
        'New Mexico',
        'New York',
        'North Carolina',
        'North Dakota',
        'Ohio',
        'Oklahoma',
        'Oregon',
        'Pennsylvania',
        'Rhode Island',
        'South Carolina',
        'South Dakota',
        'Tennessee',
        'Texas',
        'Utah',
        'Vermont',
        'Virginia',
        'Washington',
        'West Virginia',
        'Wisconsin',
        'Wyoming',
      ];
    
      constructor(private fb: FormBuilder) {
        
        this.addForm = this.fb.group({
          fname: '',
          address: '',
          address2: '',
          city: '',
          "state": this.state,
          zip: '',
          company: '',
          lname: ''
        });
        this.stateCtrl = new FormControl();
        this.filteredStates = this.stateCtrl.valueChanges
            .startWith(null)
            .map(name => this.filterStates(name));
      }
    
      filterStates(val: string) {
        return val ? this.states.filter(s => new RegExp(`^${val}`, 'gi').test(s))
                   : this.states;
      }
      
      submit(form){
        alert(JSON.stringify(form));
      }
      
      selectState(state, form){
        // console.log(state);
        // console.log(form);
        form.state = state;
      }
    }

**input-form-example.css:**

    .example-form {
      width: 500px;
    }
    
    .example-full-width {
      width: 100%;
    }

[Live Example][1]


  [1]: https://plnkr.co/edit/Hle6PV?p=preview

## One md-autocomplete for multiple formControl
This example requires `FormsModule` and `ReactiveFormsModule`. Please import them in your application/module.

    import {FormsModule, ReactiveFormsModule} from '@angular/forms';

**autocomplete-overview-example.html:**

    <md-input-container>
      <input mdInput placeholder="State" [mdAutocomplete]="auto" [formControl]="stateCtrl">
    </md-input-container>
    
    <p></p>
    
    <md-input-container>
      <input mdInput placeholder="State2" [mdAutocomplete]="auto" [formControl]="stateCtrl2">
    </md-input-container>
    
    <p></p>
    
    <md-input-container>
      <input mdInput placeholder="State3" [mdAutocomplete]="auto" [formControl]="stateCtrl3">
    </md-input-container>
    
    <md-autocomplete #auto="mdAutocomplete">
      <md-option *ngFor="let state of filteredStates | async" [value]="state">
        {{ state }}
      </md-option>
    </md-autocomplete>

**autocomplete-overview-example.ts:**

    import {Component} from '@angular/core';
    import {FormControl} from '@angular/forms';
    
    import 'rxjs/add/operator/startWith';
    import 'rxjs/add/operator/map';
    
    @Component({
      selector: 'autocomplete-overview-example',
      templateUrl: 'autocomplete-overview-example.html',
    })
    export class AutocompleteOverviewExample {
      stateCtrl: FormControl;
      stateCtrl2: FormControl;
      stateCtrl3: FormControl;
      filteredStates: any;
    
      states = [
        'Alabama',
        'Alaska',
        'Arizona',
        'Arkansas',
        'California',
        'Colorado',
        'Connecticut',
        'Delaware',
        'Florida',
        'Georgia',
        'Hawaii',
        'Idaho',
        'Illinois',
        'Indiana',
        'Iowa',
        'Kansas',
        'Kentucky',
        'Louisiana',
        'Maine',
        'Maryland',
        'Massachusetts',
        'Michigan',
        'Minnesota',
        'Mississippi',
        'Missouri',
        'Montana',
        'Nebraska',
        'Nevada',
        'New Hampshire',
        'New Jersey',
        'New Mexico',
        'New York',
        'North Carolina',
        'North Dakota',
        'Ohio',
        'Oklahoma',
        'Oregon',
        'Pennsylvania',
        'Rhode Island',
        'South Carolina',
        'South Dakota',
        'Tennessee',
        'Texas',
        'Utah',
        'Vermont',
        'Virginia',
        'Washington',
        'West Virginia',
        'Wisconsin',
        'Wyoming',
      ];
    
      constructor() {
        this.stateCtrl = new FormControl();
        this.stateCtrl2 = new FormControl();
        this.stateCtrl3 = new FormControl();
        this.filteredStates = this.stateCtrl.valueChanges
            .startWith(null)
            .map(name => this.filterStates(name));
        this.filteredStates = this.stateCtrl2.valueChanges
            .startWith(null)
            .map(name => this.filterStates(name));
        this.filteredStates = this.stateCtrl3.valueChanges
            .startWith(null)
            .map(name => this.filterStates(name));
      }
    
      filterStates(val: string) {
        return val ? this.states.filter(s => s.toLowerCase().indexOf(val.toLowerCase()) === 0)
                   : this.states;
      }
    
    }

[Live Example][1]


  [1]: https://plnkr.co/edit/PHJkJpkgOXLTEWpDI3eX?p=info

