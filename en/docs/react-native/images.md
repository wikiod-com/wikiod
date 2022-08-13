---
title: "Images"
slug: "images"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Image Module
You're going to have to import `Image` from the `react-native` package like so then use it: 

<!-- language: lang-js -->
    import { Image } from 'react';
    
    <Image source={{uri: 'https://image-souce.com/awesomeImage'}} />

You can also use a local image with a slightly different syntax but same logic like so:

<!-- language: lang-js -->
    import { Image } from 'react';
    
    <Image source={require('./img/myCoolImage.png')} />

Note:- You should give height, width to the image otherwise it won't show.

## Image Example
    class ImageExample extends Component {
      render() {
        return (
          <View>
            <Image style={{width: 30, height: 30}}
              source={{uri: 'http://facebook.github.io/react/img/logo_og.png'}}
            />
          </View>
        );
      }
    }

## Conditional Image Source
    <Image style={[this.props.imageStyle]}
            source={this.props.imagePath
            ? this.props.imagePath
            : require('../theme/images/resource.png')}
        />
If the path is available in `imagePath` then it will be assigned to source else the default image path will be assigned.

## Using variable for image path
    let imagePath = require("../../assets/list.png");

    <Image style={{height: 50, width: 50}} source={imagePath} />

From external resource:

    <Image style={{height: 50, width: 50}} source={{uri: userData.image}} />

## To fit an Image
    <Image 
        resizeMode="contain" 
        style={{height: 100, width: 100}} 
        source={require('../assets/image.png')} />

Try also **cover**, **stretch**, **repeat** and **center** parameters.

