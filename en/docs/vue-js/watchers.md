---
title: "Watchers"
slug: "watchers"
draft: false
images: []
weight: 9914
type: docs
toc: true
---

## How it works
You can watch data property of any Vue instance. When watching a property, you trigger a method on change:

    export default {
        data () {
            return {
                watched: 'Hello World'
            }
        },
        watch: {
            'watched' () {
                console.log('The watched property has changed')
            }
        }
    }

You can retrieve the old value and the new one: 

    export default {
        data () {
            return {
                watched: 'Hello World'
            }
        },
        watch: {
            'watched' (value, oldValue) {
                console.log(oldValue) // Hello World
                console.log(value) // ByeBye World
            }
        },
        mounted () {
            this.watched = 'ByeBye World'
        }
    }

If you need to watch nested properties on an object, you will need to use the `deep` property:

    export default {
        data () {
            return {
                someObject: {
                    message: 'Hello World'
                }
            }
        },
        watch: {
            'someObject': {
                deep: true,
                handler (value, oldValue) {
                    console.log('Something changed in someObject')
                }
            }
        }
    }

**When is the data updated?** 

If you need to trigger the watcher before making some new changes to an object, you need to use the `nextTick()` method: 

    export default {
        data() {
            return {
                foo: 'bar',
                message: 'from data'
            }
        },
        methods: {
            action () {
                this.foo = 'changed'
                // If you juste this.message = 'from method' here, the watcher is executed after. 
                this.$nextTick(() => {
                    this.message = 'from method'
                })
            }
        },
        watch: {
            foo () {
                this.message = 'from watcher'
            }
        }
    }

