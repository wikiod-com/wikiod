---
title: "Multiple props rendering"
slug: "multiple-props-rendering"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## render multiple variables
For rendering multiple props or variables we can use **``**. 

      render() {
        let firstName = 'test';
        let lastName = 'name';
        return (
          <View style={styles.container}>
            <Text>{`${firstName} ${lastName}` } </Text>
          </View>
        );
      }

Output:
test name


