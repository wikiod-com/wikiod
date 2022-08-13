---
title: "Using ReactJS with jQuery"
slug: "using-reactjs-with-jquery"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## ReactJS with jQuery
Firstly, you have to import jquery library . We also need to import findDOmNode as we’re going to manipulate the dom. And obviously we are importing React as well.

    import React from 'react';
    import { findDOMNode } from ‘react-dom’;
    import $ from ‘jquery’;

We are setting an arrow function ‘handleToggle’ that will fire when an icon will be clicked. We’re just showing and hiding a div with a reference naming ‘toggle’ onClick over an icon. 

    handleToggle = () => {
        const el = findDOMNode(this.refs.toggle);
        $(el).slideToggle();
    };

Let’s now set the reference naming ‘toggle’

    <ul className=”profile-info additional-profile-info-list” ref=”toggle”>
      <li>
        <span className=”info-email”>Office Email</span>   me@shuvohabib.com
      </li>
    </ul>

The div element where we will fire the ‘handleToggle’ on onClick.
   

     <div className=”ellipsis-click” onClick={this.handleToggle}>
       <i className=”fa-ellipsis-h”/>
     </div>

Let review the full code below, how it looks like .

    import React from ‘react’;
    import { findDOMNode } from ‘react-dom’;
    import $ from ‘jquery’;
    
    export default class FullDesc extends React.Component {
        constructor() {
            super();
        }
    
        handleToggle = () => {
            const el = findDOMNode(this.refs.toggle);
            $(el).slideToggle();
        };
    
        render() {
            return (
                <div className=”long-desc”>
                    <ul className=”profile-info”>
                        <li>
                            <span className=”info-title”>User Name : </span> Shuvo Habib
                        </li>
                    </ul>
    
                    <ul className=”profile-info additional-profile-info-list” ref=”toggle”>
                        <li>
                            <span className=”info-email”>Office Email</span> me@shuvohabib.com
                        </li>
                    </ul>
                    
                    <div className=”ellipsis-click” onClick={this.handleToggle}>
                        <i className=”fa-ellipsis-h”/>
                    </div>
                </div>
            );
        }
    }

We are done! This is the way, how we can use **jQuery in React** component.

