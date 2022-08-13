---
title: "Custom Settings"
slug: "custom-settings"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

Introduction
---
Unlike custom objects which have records based on them, custom settings let you utilize custom data sets across your org, or distinguish particular users or profiles based on custom criteria. This means, for example, that admins can edit hierarchy custom settings to deactivate Workflow / Validation Rules for single users or profiles, without having to switch them off for the whole org (see the Using Hierarchy Custom Settings To Disable Workflow / Validation Rules example above).

Validation rules commonly need to be disabled temporarily when:

 - Code is updating old records, which were last edited before a validation rule was activated & therefore don't meet the newer rule's criteria.
 - Code is inserting new records without the values required by a validation rule's criteria.

Workflow rules commonly need to be disabled temporarily when:
 - They would trigger an Email Alert or Field Update which would overwrite or interfere the changes you are making to the record.

Use of a custom setting grants admins some declarative control over code so one of the many use cases is that when utilized, they can make it unnecessary to deploy code in order to disable triggers (see the Using Hierarchy Custom Settings To Disable Apex Code example above).

A key benefit for developers is that custom setting's data is exposed in the application cache, which enables efficient access without the cost of repeated queries to the database. This data can then be used by formula fields, validation rules, flows, Apex, and the SOAP API - see the [Salesforce documentation][1].

The limits & considerations for custom settings are documented [here][2].

List Custom Settings
---
It is possible to create List Custom Settings too, common use cases include storing two-letter state abbreviations, international dialing prefixes, and catalog numbers for products. However Salesforce is now promoting the use Custom Metadata Types, instead of List Custom Settings.

When you go to create a new Custom Setting, the following message will be displayed

>**Tip: Use Custom Metadata Types for App Configuration**  
If you're thinking of using list custom settings, consider using custom metadata types instead. Unlike list custom settings, you can migrate the records of custom metadata types using using packages or Metadata API tools.

Custom Metadata Types have additional benefits vs List Custom Settings as described in this [answer][3]. And according to the lead developer of CMDs ["There’s a lot more planned for custom metadata types than custom settings on steroids."][4]


  [1]: https://help.salesforce.com/apex/HTViewHelpDoc?id=cs_about.htm
  [2]: https://help.salesforce.com/HTViewHelpDoc?id=cs_limits.htm&language=en_US
  [3]: http://salesforce.stackexchange.com/a/93357/10720
  [4]: http://salesforce.stackexchange.com/a/74139/10720

## Creating & Managing Custom Settings
Creation
---
To create a Custom Setting, go to:  

**Classic**  
Setup > Develop > Custom Settings > New

**Lightning**  
Setup > Custom Code > Custom Settings > New

Create your setting (see the Remarks later in this document for the differences between Hierarchy & List custom settings). You can ignore the Visibility picklist, unless you plan to deploy your setting in a managed package.

To create your setting fields click the New button and follow the [usual process][1] for creating a custom field.

Management
---
Once you have created your field(s) you can start configuring the setting by clicking the Manage button.

It's easier to manage the setting if you create a new view and include any fields that you've created to give yourself a comprehensive overview of the setting, at a glance. The Setup Owner is the user or profile that the setting applies to.

To manage the setting at the org level, click the New button above the Default Organization Level Value header (in red box below).

To manage the setting at the user or profile level, click the New button in the blue box below.

[![Edit Custom Setting][2]][2]


  [1]: https://help.salesforce.com/HTViewHelpDoc?id=adding_fields.htm
  [2]: http://i.stack.imgur.com/iWFMH.jpg

## Using Hierarchy Custom Settings To Disable Workflow / Validation Rules
Custom Setting
---

[![Custom Setting][1]][1]

Custom Setting Field
---

[![Custom Setting Field][2]][2]

Custom Setting Field Value
---
When the field is checked the validation rule will be disabled, for the running user or in this example, their profile -

[![Custom Setting Field Edit - Profile][3]][3]

The rule can also be disabled for a whole Salesforce org -

[![Custom Setting Field Edit - Org][4]][4]

Validation Rule
---

    AND(
      /* the below is the reference to the Val_Rule_Cntrlr__c custom setting's checkbox field All_Opportunity_Disabled__c
      */
      $Setup.Val_Rule_Cntrlr__c.All_Opportunity_Disabled__c = FALSE,

      /* the below is the remainder of the validation rule's formula
      */
      CloseDate < TODAY()
    )

In the above rule, both pieces of criteria must evaluate to `TRUE` in order for the rule to be triggered.

Since `All_Opportunity_Disabled__c` checkbox will evaluate to `TRUE` when the running user's profile is System Administrator, the rule will evaluate to `FALSE`.

Workflow Rules
---
The same approach can be applied in order to deactivate Workflow Rules.


  [1]: http://i.stack.imgur.com/XchCX.jpg
  [2]: http://i.stack.imgur.com/W2uCa.jpg
  [3]: http://i.stack.imgur.com/GKRJC.jpg
  [4]: http://i.stack.imgur.com/DD4sE.jpg

## Using Hierarchy Custom Settings To Disable Apex Code
Explanation
---
In this example a simple [Trigger][1] has been created to change the Close Date of an Opportunity, that's about to be inserted or updated, to a date 10 days in the future.

The Apex Controller custom setting's checkbox field enables the code to be disabled at the user / profile / org level.

Apex Class
---
    trigger CloseDateUpdate on Opportunity (before insert, before update) {
        
        Id userId;
        Apx_Cntrlr__c userApexController;
        Boolean userSetting;
        
        userId = userinfo.getUserId();
        userApexController = Apx_Cntrlr__c.getInstance(userId);
        userSetting = userApexController.Close_Date_Update_Disabled__c;
    
        if (userSetting == false) {
            for(Opportunity opp : Trigger.new) {
                opp.CloseDate = date.today().addDays(10);
            }
        }
        
    }

Unit Test
---
    @isTest
    public class CloseDateUpdateTest {
        
        @testSetup
        static void dataSetup() {
            
            Profile p = [SELECT Id FROM Profile WHERE Name = 'System Administrator' LIMIT 1];
            
            User u = new User(LastName = 'Test',Alias = 't1',Email = 'example@gmail.com',Username = 'sotest@gmail.com',ProfileId = p.Id,TimeZoneSidKey = 'America/Denver',LocaleSidKey = 'en_US',EmailEncodingKey = 'UTF-8',LanguageLocaleKey = 'en_US');
            insert u;
        }
        
        static testMethod void testCloseDateUpdateEnabled() {
            
            User u = [SELECT Id FROM User WHERE Username = 'sotest@gmail.com'];
            // set the custom setting field to FALSE so that the trigger is not deactivated
            Apx_Cntrlr__c apexController = new Apx_Cntrlr__c(SetupOwnerId = u.Id,Close_Date_Update_Disabled__c = false);
            upsert apexController;
            
            Opportunity[] opportunities1 = new Opportunity[]{};
            
            test.startTest();
            system.runAs(u){
                    
                    for(integer i = 0; i < 200; i++) {
                        opportunities1.add(new Opportunity(
                            Name          = 'Test Opp ' + i,
                            OwnerId       = u.Id,
                            StageName     = 'Prospecting',
                            CloseDate     = date.today().addDays(1),
                            Amount        = 100));
                    }
                insert opportunities1;
            }
            test.stopTest();
            
            List<Opportunity> opportunities2 = [SELECT CloseDate FROM Opportunity];
            
            for(Opportunity o : opportunities2){
                system.assertEquals(date.today().addDays(10), o.closeDate, 'CloseDateUpdate trigger should have changed the Opportunity close date as it was not disabled by the apexController custom setting');
            }
        }
        
        static testMethod void testCloseDateUpdateDisabled() {
            
            User u = [SELECT Id FROM User WHERE Username = 'sotest@gmail.com'];
            // set the custom setting field to TRUE to deactivate the trigger
            Apx_Cntrlr__c apexController = new Apx_Cntrlr__c(SetupOwnerId = u.Id,Close_Date_Update_Disabled__c = true);
            upsert apexController;
            
            Opportunity[] opportunities1 = new Opportunity[]{};
                
            test.startTest();
            system.runAs(u){
                
                for(integer i = 0; i < 200; i++) {
                    opportunities1.add(new Opportunity(
                        Name          = 'Test Opp ' + i,
                        OwnerId       = u.Id,
                        StageName     = 'Prospecting',
                        CloseDate     = date.today().addDays(1),
                        Amount        = 100));
                }
                insert opportunities1;
            }
            test.stopTest();
            
            List<Opportunity> opportunities2 = [SELECT CloseDate FROM Opportunity];
            
            for(Opportunity o : opportunities2){
                system.assertEquals(date.today().addDays(1), o.closeDate, 'CloseDateUpdate trigger should not have changed the Opportunity close date as it was disabled by the apexController custom setting');
            }
        }
            
    }


  [1]: https://developer.salesforce.com/trailhead/en/apex_triggers/apex_triggers_intro

## Updating Hierarchy Custom Settings in Apex Code
You may wish to update your custom setting's during the execution of your code, to switch off validation or workflow rules.

In the below code, I have created a [Schedulable Apex Class][1] which will update the Close Date of any Opportunities whose Close Date is less than or equal to 6 days from the current date, changing the date to 20 days in the future.

I will use my Custom Setting Val_Rule_Cntrlr__c to deactivate any validation rules which would prevent me from updating the Opportunities that meet my criteria.

    global class Scheduled_OppCloseDateUpdate implements Schedulable {
    
        global void execute(SchedulableContext SC) {
            updOpportunityCloseDates();
        }
    
        global void updOpportunityCloseDates() {
            
            Id userId;
            Val_Rule_Cntrlr__c setting;
            Boolean validationRulesAlreadyDisabled;
            List<Opportunity> processedOpps = new List<Opportunity>();
            Date d;
            
            // get running user's Id
            userId = userinfo.getUserId();
            // retrieve Custom Setting status, for running user
            setting = Val_Rule_Cntrlr__c.getInstance(userId);
            
            // if the setting field is false, update it to disable validation rules
            if (setting.All_Opportunity_Disabled__c == false) {
                setting.All_Opportunity_Disabled__c = true;
                upsert setting;
            }
            // if the setting field was already true, there's no need to disable it
            // but it shouldn't be switched to false by this class once the process has been completed
            else {
                validationRulesAlreadyDisabled = true;
            }
            
            // execute code to manage business process
       
            d = system.today().addDays(6);
            
            for(Opportunity o : [SELECT Id, CloseDate
                                   FROM Opportunity
                                  WHERE CloseDate <= :d
                                    // class only updates open Opportunities
                                    AND Probability > 0 AND Probability < 100])
            {
                o.CloseDate = System.today().addDays(20);
                processedOpps.add(o);
            }
            
            if (processedOpps.size() > 0) {
                update processedOpps;
            }
            
            // reactivate validation rules
            if (validationRulesAlreadyDisabled == false) {
                setting.All_Opportunity_Disabled__c = false;
                upsert setting;
            }
            
        }
        
    }

To make sure that my validation rules are being deactivated by the changes to my custom setting in my class, I have created a checkbox field `Trigger_Validation_Rule__c` (which would not be visible to users or added to page layouts) & a validation rule with this criteria:

    AND(
      $Setup.Val_Rule_Cntrlr__c.All_Opportunity_Disabled__c = FALSE,
      Trigger_Validation_Rule__c = TRUE,
    
      /* allow the above criteria to be met while inserting the Opportunities, without triggering the rule, in the @testSetup portion of the test */
      NOT(ISNEW())
    )

I then set the checkbox field to `true` when creating my Opportunities so that the rules criteria would be met, if the custom setting field is not edited by my code.

    @isTest
    private class WE_ScheduledCloseDateUpdateTest {
    
        @testSetup
        static void dataSetup() {
    
            Profile p = [SELECT Id FROM Profile WHERE Name = 'System Administrator' LIMIT 1];
            
            User u = new User(LastName = 'Test',Alias = 't1',Email = 'example@gmail.com',Username = 'sotest@gmail.com',ProfileId = p.Id,TimeZoneSidKey = 'America/Denver',LocaleSidKey = 'en_US',EmailEncodingKey = 'UTF-8',LanguageLocaleKey = 'en_US');
            insert u;
    
            Val_Rule_Cntrlr__c valRuleCntrlr = new Val_Rule_Cntrlr__c(SetupOwnerId = u.Id,All_Opportunity_Disabled__c = false);
            upsert valRuleCntrlr;
                
            List<Opportunity> testOpps = new List<Opportunity>();
                
            // create the Opportunities that will be updated by the class
            for(integer i = 0; i < 200; i++) {
                testOpps.add(new Opportunity(
                    Name          = 'Test Opp Update' + i,
                    OwnerId       = u.Id,
                    StageName     = 'Prospecting',
                    CloseDate     = date.today().addDays(1),
                    Amount        = 100,
                    // set checkbox field to true, to trigger validation rules if they've not been deactivated by class
                    Trigger_Validation_Rule__c = true));
            }
            // create the Opportunities that won't be updated by the class
            for(integer i = 0; i < 200; i++) {
                testOpps.add(new Opportunity(
                    Name          = 'Test Opp Skip' + i,
                    OwnerId       = u.Id,
                    StageName     = 'Prospecting',
                    CloseDate     = date.today().addDays(15),
                    Amount        = 100,
                    Trigger_Validation_Rule__c = true));
            }           
            insert testOpps;
    
        }
        
        // code required to test a scheduled class, see https://developer.salesforce.com/docs/atlas.en-us.apexcode.meta/apexcode/apex_scheduler.htm for more details
        public static String CRON_EXP = '0 0 0 15 3 ? 2022';
    
        static testmethod void testCloseDateUpdates() {
    
            // execute scheduled class
            
            Test.startTest();
            
            String jobId = System.schedule('ScheduleApexClassTest',
                                           CRON_EXP,
                                           new Scheduled_OppCloseDateUpdate());
            
            CronTrigger ct = [SELECT Id, CronExpression, TimesTriggered, NextFireTime
                                FROM CronTrigger
                               WHERE id = :jobId];
            
            System.assertEquals(CRON_EXP, ct.CronExpression);
            System.assertEquals(0, ct.TimesTriggered);
            System.assertEquals('2022-03-15 00:00:00', String.valueOf(ct.NextFireTime));
            
            Test.stopTest();
    
            // test results
            
            Integer updateCount = 0;
            Integer skipCount   = 0;
    
            List <Opportunity> opportunitys = [SELECT Id, Name, CloseDate FROM Opportunity];
    
            for(Opportunity o : opportunitys) {
                if (o.Name.contains('Update') &&
                    updateCount == 0)
                {
                    System.assertEquals(date.today().addDays(20), o.CloseDate, 'Opportunity\'s Close Date should have been updated as it was less than 7 days away');
                    updateCount = 1;
                }
                if (o.Name.contains('Skip') &&
                    skipCount == 0)
                {
                    System.assertEquals(date.today().addDays(15), o.CloseDate, 'Opportunity should not have been updated as it\'s Close Date is more than 7 days away');
                    skipCount = 1;
                }
            }
            // check that both lists of Opportunities have been tested
            System.assertEquals(2, updateCount + skipCount, 'Count should be 2 once all assertions have been executed');
        
        }
        
        // check that the class does not change the custom setting's field to false, if it was true before class was executed
        static testmethod void testSettingUpdates() {
            
            User u = [SELECT Id FROM User WHERE UserName = 'sotest@gmail.com'];
            
            // switch the custom setting field to true before the scheduled job executes
            Val_Rule_Cntrlr__c setting;
            setting = Val_Rule_Cntrlr__c.getInstance(u.Id);
            setting.All_Opportunity_Disabled__c = true;
            upsert setting;
            
            System.runAs(u) {
                
                Test.startTest();
                
                String jobId = System.schedule('ScheduleApexClassTest',
                                               CRON_EXP,
                                               new Scheduled_OppCloseDateUpdate());
                
                CronTrigger ct = [SELECT Id, CronExpression, TimesTriggered, NextFireTime
                                    FROM CronTrigger
                                   WHERE id = :jobId];
                
                System.assertEquals(CRON_EXP, ct.CronExpression);
                System.assertEquals(0, ct.TimesTriggered);
                System.assertEquals('2022-03-15 00:00:00', String.valueOf(ct.NextFireTime));
                
                Test.stopTest();
            }
            setting = Val_Rule_Cntrlr__c.getInstance(u.Id);
            
            // check that the class did not change the All_Opportunity_Disabled__c field to false
            System.assertEquals(true, setting.All_Opportunity_Disabled__c);
        }
    
    }


  [1]: https://help.salesforce.com/apex/HTViewHelpDoc?id=code_schedule_batch_apex.htm&language=en

