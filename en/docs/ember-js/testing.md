---
title: "Testing"
slug: "testing"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

Creating and maintaining a comprehensive test suite should be a priority for each developer. Testing in Ember.js involves dealing with asynchrony, Ember Run Loop and mocking your API. It is common for Ember.js developers to struggle when writing tests. However, there are some tips which could save your time and energy.

## Waiting for promises in tests in elegant way
You can make `function` passed to `test()` method `async` - then you can use `await` keyword. Your test will wait until Promises resolve and testing asynchronous code becomes easier and more readable. In the following example call that returns a Promise is `changeset.validate()`. Please notice also wrapping `set` call in `Ember.run`. Setting quantity has asynchronous effects (observers, computed properties) and thus we need to wrap it in `Ember.run`.

    test('quantity validation: greater than 0', async function (assert) {
        assert.expect(3);
    
        const model = this.subject({
            quantity: 1
        });
    
        const changeset = createChangeset(model);
    
        await changeset.validate();
    
        assert.ok(!changeset.get('error.quantity'));
    
        Ember.run(() => {
            changeset.set('quantity', -1);
        });
    
        await changeset.validate();
    
        assert.equal(changeset.get('error.quantity.validation.length'), 1);
        assert.ok(!changeset.get('isValid'));
    });

