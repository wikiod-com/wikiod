---
title: "md-table"
slug: "md-table"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

This topic includes examples related to md-table

For more details on `md-table` please check the [documentation][1]


  [1]: https://material.angular.io/components/table/overview

## Connect DataSource from external API
Please me mindful of importing all necessary libraries required. 

This example uses `InMemoryDbService` from `angular-in-memory-web-api` to provide the JSON data from mock API.

[Live demo][1]


**service.ts:**
<!-- language: typescript -->

    import { Injectable }    from '@angular/core';
    import { Headers, Http } from '@angular/http';
    import 'rxjs/add/operator/toPromise';
    
    @Injectable()
    export class AppState {
      
      private headers = new Headers({'Content-Type': 'application/json'});
      private apiUrl = 'api/data';
      
      constructor(private http: Http) { }
      
      fetchFilterFields() {
        console.log(this.apiUrl);
        return this.http.get(this.apiUrl)
                   .toPromise()
                   .then(response => response.json().data)
                   .catch(this.handleError);
      }
      
      private handleError(error: any): Promise<any> {
        console.error('An error occurred', error); // for demo purposes only
        return Promise.reject(error.message || error);
      }
    
    }


**component.ts:**
<!-- language: typescript -->


    import {Component} from '@angular/core';
    import {DataSource} from '@angular/cdk';
    import {BehaviorSubject} from 'rxjs/BehaviorSubject';
    import {Observable} from 'rxjs/Observable';
    import 'rxjs/add/observable/merge';
    
    import { AppState } from './shared.service';
    
    @Component({
      selector: 'md-table-example',
      templateUrl: 'select-form-example.html',
      styleUrls: ['select-form-example.css']
    })
    export class SelectFormExample implements OnInit {
      
      displayedColumns = ['id', 'name'];
      dataSource: ExampleDataSource | null;
      
      constructor(private appState: AppState){ }
      
      ngOnInit(){
        this.dataSource = new ExampleDataSource(this.appState);
      }
      
    }
    
    export interface UserData {
      id: string;
      name: string;
    }
    
    export class ExampleDataSource extends DataSource<any> {
      constructor(private appState: AppState) {
        super();
      }
      
      subject: BehaviorSubject<Hero[]> = new BehaviorSubject<Hero[]>([]);
    
      connect(): Observable<Hero[]> {
          console.log('connect');
          if (!this.subject.isStopped)
              this.appState.fetchFilterFields()
                  .then(res => {
                      this.subject.next(res)
                  });
          return Observable.merge(this.subject);
      }
    
      disconnect() {
          this.subject.complete();
          this.subject.observers = [];
      }
    }



**component.html:**
<!-- language: lang-html -->

    <h2> Material Table </h2>
    
    <div *ngIf="dataSource" class="example-container mat-elevation-z8">
      <md-table #table [dataSource]="dataSource">
    
        <!-- ID Column -->
        <ng-container cdkColumnDef="id">
          <md-header-cell *cdkHeaderCellDef> ID </md-header-cell>
          <md-cell *cdkCellDef="let row"> {{row.id}} </md-cell>
        </ng-container>
    
        <!-- Name Column -->
        <ng-container cdkColumnDef="name">
          <md-header-cell *cdkHeaderCellDef> Name </md-header-cell>
          <md-cell *cdkCellDef="let row"> {{row.name}} </md-cell>
        </ng-container>
    
        <md-header-row *cdkHeaderRowDef="displayedColumns"></md-header-row>
        <md-row *cdkRowDef="let row; columns: displayedColumns;"></md-row>
      </md-table>
    </div>


***For reference:***

**in-memory-data-service.ts:**

    import { InMemoryDbService } from 'angular-in-memory-web-api';
    export class InMemoryDataService implements InMemoryDbService {
      createDb() {
        const data = [
          { id: 1,  name: 'Ironcast' },
          { id: 2, name: 'Mr. Nice' },
          { id: 3, name: 'Narco' },
          { id: 4, name: 'Bombasto' },
          { id: 5, name: 'Celeritas' },
          { id: 6, name: 'Magneta' },
          { id: 7, name: 'RubberMan' },
          { id: 8, name: 'Dynama' },
          { id: 9, name: 'Dr. IQ' },
          { id: 10, name: 'Magma' },
          { id: 11, name: 'Tornado' }
        ];
        return {data};
      }
    }


  [1]: https://plnkr.co/edit/gVN7D52iaRjAs1tIhvaA

