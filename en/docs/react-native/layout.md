---
title: "Layout"
slug: "layout"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

## Flexbox
Flexbox is a layout mode providing for the arrangement of elements on a page such that the elements behave predictably when the page layout must accommodate different screen sizes and different display devices. By default flexbox arranges children in a column. But you can change it to row using `flexDirection: 'row'`.

## flexDirection
<!-- language: lang-js -->
    const Direction = (props)=>{
      return (
        <View style={styles.container}>
          <Box/>
          <Box/>
          <Box/>
          <View style={{flexDirection:'row'}}>
            <Box/>
            <Box/>
            <Box/>
          </View>
        </View>
      )
    }

    const styles = StyleSheet.create({
      container: {
        flex:1,
        backgroundColor: '#AED581',
      }
    });
[![direction][1]][1]

## Alignment axis
<!-- language: lang-js -->
    const AlignmentAxis = (props)=>{
      return (
        <View style={styles.container}>
          <Box />
          <View style={{flex:1, alignItems:'flex-end', justifyContent:'flex-end'}}>
            <Box />
            <Box />
          </View>
          <Box />
        </View>
      )
    }

    const styles = StyleSheet.create({
      container: {
        flex:1,
        backgroundColor: `#69B8CC`,
      },
      text:{
        color: 'white',
        textAlign:'center'
      }
    });

[![alignment axis][2]][2]

## Alignment
<!-- language: lang-js -->
    const Alignment = (props)=>{
      return (
        <View style={styles.container}>
          <Box/>
          <View style={{alignItems:'center'}}>
            <Box/>
            <View style={{flexDirection:'row'}}>
              <Box/>
              <Box/>
              <Box/>
            </View>
            <Box/>
          </View>
          <Box/>
        </View>
      )
    }

    const styles = StyleSheet.create({
      container: {
        flex:1,
        backgroundColor: `#69B8CC`,
      },
      text:{
        color: 'white',
        textAlign:'center'
      }
    });

[![alignment][3]][3]

# Flex size
<!-- language: lang-js -->
    const FlexSize = (props)=>{
      return (
        <View style={styles.container}>
          <View style={{flex:0.1}}>
            <Box style={{flex:0.7}}/>
            <Box style={{backgroundColor: 'yellow'}}/>
            <Box/>
            <Box style={{flex:0.3, backgroundColor: 'yellow'}}/>
          </View>
          <View style={{flex:0.1}}>
            <Box style={{flex:1}}/>
            <Box style={{backgroundColor: 'yellow'}}/>
            <Box/>
            <Box style={{flex:1, backgroundColor: 'yellow'}}/>
          </View>
        </View>
      )
    }

    const styles = StyleSheet.create({
      container: {
        flex:1,
        flexDirection:'row',
        backgroundColor: colors[1],
      },
    });
[![flex size][4]][4]

More about Facebook's flexbox implementation [here][5].


  [1]: http://i.stack.imgur.com/KCGqA.png
  [2]: http://i.stack.imgur.com/47xGP.png
  [3]: http://i.stack.imgur.com/I9XFl.png
  [4]: http://i.stack.imgur.com/i4FAm.png
  [5]: https://github.com/facebook/yoga

