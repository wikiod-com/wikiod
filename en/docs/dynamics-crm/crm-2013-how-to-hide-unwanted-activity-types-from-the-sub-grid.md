---
title: "CRM 2013 How to hide unwanted Activity Types from the sub grid"
slug: "crm-2013-how-to-hide-unwanted-activity-types-from-the-sub-grid"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

I recently had to modify the Activity sub-grid to remove certain activity types from the add activity menu.

Note, this may not be a supported method on how to do this, but there is no documented supported way to so it, so I had to come up with a solution & this worked, in CRM 2013 anyway.

## Add this function to a javascript web resource
    var _activitiesGridName = '';
    function SetupActivityGridOnload(gridName)
    {
        var btnsToHide =
        [
            'AddserviceappointmentButton',
            'AddcampaignresponseButton',
            'AddappointmentButton'
        ];
        _activitiesGridName = gridName;
        setTimeout(function ()
        {   //setting timeout beacuse subgid take some time to load after the form is loaded
            if (Xrm.Page != null && Xrm.Page != undefined)
            {   //validating to check if the sub grid is present on the form
                var grid = Xrm.Page.getControl(_activitiesGridName);
                if (!grid)
                {   // grid not loaded yet - call function again to recheck after timeout
                    console.log('grid not loaded yet');
                    SetupActivityGridOnload(_activitiesGridName);
                }
                else
                {   // grid loaded now hide unwanted activity buttons
                    var menuItem = null;
                    var parentMenu = null;
                    $.each(btnsToHide, function (i, val)
                    {
                        menuItem = document.getElementByIdval);
                        if (menuItem)
                        {
                            if (parentMenu == null)
                            {   // load parent node - if not already loaded
                                parentMenu = menuItem.parentNode;
                            }
                            console.log('removing menu item: ' + val);
                            parentMenu.removeChild(menuItem);
                        }
                        else
                        {
                            console.log('menu not found: ' + val);
                        }
                    });
                }
            }
        }, 2000);
    }

Then call the function from the onload event adding the grid name as a parameter like so

[![on load event definition][1]][1]


  [1]: https://i.stack.imgur.com/myIy8.png

