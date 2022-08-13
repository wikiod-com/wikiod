---
title: "Unit testing frameworks"
slug: "unit-testing-frameworks"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Mocha Asynchronous (async/await)
    const { expect } = require('chai')

    describe('Suite Name', function() {
      describe('#method()', function() {
        it('should run without an error', async function() {
          const result = await answerToTheUltimateQuestion()
          expect(result).to.be.equal(42)
        })
      })
    })

## Mocha synchronous
    describe('Suite Name', function() {
      describe('#method()', function() {
        it('should run without an error', function() {
          expect([ 1, 2, 3 ].length).to.be.equal(3)
        })
      })
    })

## Mocha asynchronous (callback)
    var expect = require("chai").expect;
    describe('Suite Name', function() {
      describe('#method()', function() {
        it('should run without an error', function(done) {
          testSomething(err => {
            expect(err).to.not.be.equal(null)
            done()
          })
        })
      })
    })

## Mocha asynchronous (Promise)
    describe('Suite Name', function() {
      describe('#method()', function() {
        it('should run without an error', function() {
          return doSomething().then(result => {
             expect(result).to.be.equal('hello world')
          })
        })
      })
    })

