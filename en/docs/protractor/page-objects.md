---
title: "Page Objects"
slug: "page-objects"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Page objects is a design pattern which results in less code duplicates, easy maintenance and more readability.


## First Page Object
    /* save the file in 'pages/loginPage'
    var LoginPage = function(){
    
    };
    
    /*Application object properties*/
    LoginPage.prototype = Object.create({}, {
        userName: {
            get: function() {
                return browser.driver.findElement(By.id('userid'));
            }
        },
        userPass: {
            get: function() {
                return browser.driver.findElement(By.id('password'));
            }
        },
        submitBtn: {
            get: function() {
                return browser.driver.findElement(By.id('btnSubmit'));
            }
        }
    });
    
    /* Adding functions */
    LoginPage.prototype.login = function(strUser, strPass) {
        browser.driver.get(browser.baseUrl);
        this.userName.sendKeys(strUser);
        this.userPass.sendKeys(strPass);
        this.submitBtn.click();
    };
    
    module.exports = LoginPage;

Let's use our first page object file in our test.

    var LoginPage = require('../pages/loginPage');
    describe('User Login to Application', function() {
        var loginPage = new LoginPage();

        beforeAll(function() {
            loginPage.login(browser.params.userName, browser.params.userPass);
        });
        
        it('and see a success message in title', function() {
            expect(browser.getTitle()).toEqual('Success');
        });

     });

