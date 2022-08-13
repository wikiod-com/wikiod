---
title: "RefreshControl with ListView"
slug: "refreshcontrol-with-listview"
draft: false
images: []
weight: 9767
type: docs
toc: true
---

References:

RefreshControl: https://facebook.github.io/react-native/docs/refreshcontrol.html

ListView: https://facebook.github.io/react-native/docs/listview.html

## Refresh Control with ListView Full Example
**RefreshControl** 
is used inside a ScrollView or ListView to add pull to refresh functionality. at this example we will use it with ListView  
    
    'use strict'
    import React, { Component } from 'react';
    import { StyleSheet, View, ListView, RefreshControl, Text } from 'react-native'
    
    
    class RefreshControlExample extends Component {
      constructor () {
        super()
        this.state = {
          refreshing: false,
          dataSource: new ListView.DataSource({
            rowHasChanged: (row1, row2) => row1 !== row2 }),
          cars : [
            {name:'Datsun',color:'White'},
            {name:'Camry',color:'Green'}
          ]
        }
      }
    
       componentWillMount(){
         this.setState({ dataSource:
           this.state.dataSource.cloneWithRows(this.state.cars) })
       }
    
      render() {
        return (
          <View style={{flex:1}}>
            <ListView
              refreshControl={this._refreshControl()}
              dataSource={this.state.dataSource}
              renderRow={(car) => this._renderListView(car)}>
            </ListView>
          </View>
        )
      }
    
      _renderListView(car){
        return(
          <View style={styles.listView}>
            <Text>{car.name}</Text>
            <Text>{car.color}</Text>
          </View>
        )
      }
    
      _refreshControl(){
        return (
          <RefreshControl
            refreshing={this.state.refreshing}
            onRefresh={()=>this._refreshListView()} />
        )
      }
    
      _refreshListView(){
        //Start Rendering Spinner
        this.setState({refreshing:true})
        this.state.cars.push(
          {name:'Fusion',color:'Black'},
          {name:'Yaris',color:'Blue'}
        )
        //Updating the dataSource with new data
        this.setState({ dataSource:
            this.state.dataSource.cloneWithRows(this.state.cars) })
        this.setState({refreshing:false}) //Stop Rendering Spinner
      }
    
    }
    
    const styles = StyleSheet.create({
    
      listView: {
        flex: 1,
        backgroundColor:'#fff',
        marginTop:10,
        marginRight:10,
        marginLeft:10,
        padding:10,
        borderWidth:.5,
        borderColor:'#dddddd',
        height:70
      }
    
    })
    
    module.exports = RefreshControlExample



## Refresh Control
      _refreshControl(){
        return (
          <RefreshControl
            refreshing={this.state.refreshing}
            onRefresh={()=>this._refreshListView()} />
        )
      }

**refreshing:** is the state of the spinner (true, false).

**onRefresh:** this function will invoke when refresh the ListView/ScrollView.

## onRefresh function Example
      _refreshListView(){
        //Start Rendering Spinner
        this.setState({refreshing:true})
        this.state.cars.push(
          {name:'Fusion',color:'Black'},
          {name:'Yaris',color:'Blue'}
        )
        //Updating the dataSource with new data
        this.setState({ dataSource:
            this.state.dataSource.cloneWithRows(this.state.cars) })
        this.setState({refreshing:false}) //Stop Rendering Spinner
      }

here we are updating the array and after that we will update the dataSource. we can use [fetch][1] to request something from server and use async/await.


  [1]: https://github.com/github/fetch

