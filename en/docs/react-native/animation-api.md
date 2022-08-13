---
title: "Animation API"
slug: "animation-api"
draft: false
images: []
weight: 9949
type: docs
toc: true
---

## Animate an Image
    class AnimatedImage extends Component {
        constructor(props){
            super(props)
            this.state = {
                logoMarginTop: new Animated.Value(200)
            }
        }
        componentDidMount(){
            Animated.timing(
                this.state.logoMarginTop,
                { toValue: 100 }
            ).start()
        }
        render () {
          return (
            <View>
               <Animated.Image source={require('../images/Logo.png')} style={[baseStyles.logo, {
                  marginTop: this.state.logoMarginTop
               }]} />
            </View>
          )
        }
    }

This example is animating the image position by changing the margin.

