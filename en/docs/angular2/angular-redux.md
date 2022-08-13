---
title: "angular redux"
slug: "angular-redux"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Basic
> app.module.ts

    import {appStoreProviders} from "./app.store";
    providers : [
      ...
      appStoreProviders,
      ...
    ]

> app.store.ts

    import {InjectionToken} from '@angular/core';
    import {createStore, Store, compose, StoreEnhancer} from 'redux';
    import {AppState, default as reducer} from "../app.reducer";
    
    
    
    export const AppStore = new InjectionToken('App.store');
    
    const devtools: StoreEnhancer<AppState> =
        window['devToolsExtension'] ?
        window['devToolsExtension']() : f => f;
    
    export function createAppStore(): Store<AppState> {
      return createStore<AppState>(
        reducer,
        compose(devtools)
      );
    }
    
    export const appStoreProviders = [
      {provide: AppStore, useFactory: createAppStore}
    ];

> app.reducer.ts

    export interface AppState {
      example : string
    }
    
    const rootReducer: Reducer<AppState> = combineReducers<AppState>({
     example : string
    });
    
    export default rootReducer;

> store.ts

    export interface IAppState {
      example?: string;
    }
    
    export const INITIAL_STATE: IAppState = {
      example: null,
    };
    
    export function rootReducer(state: IAppState = INITIAL_STATE, action: Action): IAppState {
      switch (action.type) {
        case EXAMPLE_CHANGED:
          return Object.assign(state, state, (<UpdateAction>action));
        default:
          return state;
      }
    }


>   actions.ts

    import {Action} from "redux";
    export const EXAMPLE_CHANGED = 'EXAMPLE CHANGED';
    
    export interface UpdateAction extends Action {
      example: string;
    }

## Get current state
    import * as Redux from 'redux';
    import {Inject, Injectable} from '@angular/core';
    
    @Injectable()
    export class exampleService {
        constructor(@Inject(AppStore) private store: Redux.Store<AppState>) {}
        getExampleState(){
          console.log(this.store.getState().example);
       }
    }

## change state
    import * as Redux from 'redux';
    import {Inject, Injectable} from '@angular/core';
    
    @Injectable()
    export class exampleService {
        constructor(@Inject(AppStore) private store: Redux.Store<AppState>) {}
        setExampleState(){
           this.store.dispatch(updateExample("new value"));
       }
    }


> actions.ts


    export interface UpdateExapleAction extends Action {
      example?: string;
    }

    export const updateExample: ActionCreator<UpdateExapleAction> =
       (newVal) => ({
         type: EXAMPLE_CHANGED,
         example: newVal
    });
    
    

## Add redux chrome tool

> app.store.ts

    

   

     import {InjectionToken} from '@angular/core';
     import {createStore, Store, compose, StoreEnhancer} from 'redux';
     import {AppState, default as reducer} from "../app.reducer";
        
        
     export const AppStore = new InjectionToken('App.store');
        
    const devtools: StoreEnhancer<AppState> =
         window['devToolsExtension'] ?
         window['devToolsExtension']() : f => f;
        
    export function createAppStore(): Store<AppState> {
       return createStore<AppState>(
         reducer,
         compose(devtools)
       );
    }
        
        export const appStoreProviders = [
          {provide: AppStore, useFactory: createAppStore}
        ];

> install Redux DevTools chrome extention

