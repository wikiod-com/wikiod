---
title: "VueJS + Redux with Vua-Redux (Best Solution)"
slug: "vuejs-+-redux-with-vua-redux-best-solution"
draft: false
images: []
weight: 9845
type: docs
toc: true
---

## How to use Vua-Redux
**Installing Vua Redux from NPM:**

Install through:

    npm i vua-redux --save
 

**Initialize:**
---------------

===============

// main.js
 

    import Vue from 'vue';
    import { reduxStorePlugin } from 'vua-redux';
    import AppStore from './AppStore';
    import App from './Component/App';
     
    // install vua-redux 
    Vue.use(reduxStorePlugin); 
     
    new Vue({
        store: AppStore,
        render(h) {
            return <App />
        }
    });

// AppStore.js 

    import { createStore } from 'redux';
     
    const initialState = { 
      todos: [] 
    };
     
    const reducer = (state = initialState, action) => {
      switch(action.type){
        case 'ADD_TODO':
          return {
            ...state,
            todos: [...state.todos, action.data.todo]
          }
     
        default:
          return state;
        }
    }
     
    const AppStore = createStore(reducer);
     
    export default AppStore;

**Use in your component :**

// components/App.js 
 

    import { connect } from 'vua-redux';
     
    const App = {
        props: ['some-prop', 'another-prop'],
     
        /**
         * everything you do with vue component props
         * you can do inside collect key
         */
        collect: {
            todos: {
                type: Array,
            },
            addTodo: {
                type: Function,
            },
        },
     
        methods: {
            handleAddTodo() {
                const todo = this.$refs.input.value;
                this.addTodo(todo);
            }
        },
     
        render(h) {
            return <div>
                <ul>
                    {this.todos.map(todo => <li>{todo}</li>)}
                </ul>
     
                <div>
                    <input type="text" ref="input" />
                    <button on-click={this.handleAddTodo}>add todo</button>
                </div>
            </div>
        }
    };
     
    function mapStateAsProps(state) {
        return {
            todos: state.todos
        };
    }
     
    function mapActionsAsProps(dispatch) {
        return {
            addTodo(todo) {
                dispatch({
                    type: 'ADD_TODO',
                    data: { todo }
                })
            }
        }
    }
     
    export default connect(mapStateAsProps, mapActionsAsProps)(App);
     

