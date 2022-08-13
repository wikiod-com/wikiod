---
title: "Modifying Items in a Dropdown List"
slug: "modifying-items-in-a-dropdown-list"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

In this topic you will learn how to modify field attributes inherited from the PXStringList or PXIntList attributes. The demonstrated approach will make sure to not break functionality of the base Acumatica ERP product and require minimal maintenance, if any, while upgrading your customizations to a newer version of Acumatica.

In all samples above, you made changes to both the `_AllowedValues` and `_AllowedLabels` arrays. If your task is to modify only label (external value) of a drop-down item, consider using Translation Dictionaries. For more information on Translation Dictionaries see 
[Acumatica ERP Documentation][1]


  [1]: https://docref.acumatica.com/Wiki/ShowWiki.aspx?pageid=64819546-2b11-4e87-a7db-3a75b1249cb5

## Modifying Marital Statuses
In this example you will be modifying the **Marital Status** drop-down list found on the **Contacts** form (CR302000):
[![enter image description here][1]][1]

## To add new items to the PXStringListAttribute successor ##

The best way to extend drop-down items hard-coded inside an attribute inherited from the PXStringList or PXIntList attribute is by increasing size of the `_AllowedValues` and `_AllowedLabels` arrays in the constructor of your custom field attribute:

    [PXLocalizable(Messages.Prefix)]
    public static class MaritalStatusesMessages
    {
        public const string CommonLaw = "Living common law";
        public const string Separated = "Separated (not living common law)";
        public const string DivorcedNoCommonLaw = "Divorced (not living common law)";
        public const string NeverMarried = "Never Married";
    }

    public class MaritalStatusesCst1Attribute : MaritalStatusesAttribute
    {
        public const string CommonLaw = "L";
        public const string Separated = "P";
        public const string NeverMarried = "N";

        public MaritalStatusesCst1Attribute()
        {
            Array.Resize(ref _AllowedValues, _AllowedValues.Length + 3);
            _AllowedValues[_AllowedValues.Length - 3] = CommonLaw;
            _AllowedValues[_AllowedValues.Length - 2] = Separated;
            _AllowedValues[_AllowedValues.Length - 1] = NeverMarried;
            Array.Resize(ref _AllowedLabels, _AllowedLabels.Length + 3);
            _AllowedLabels[_AllowedLabels.Length - 3] = MaritalStatusesMessages.CommonLaw;
            _AllowedLabels[_AllowedLabels.Length - 2] = MaritalStatusesMessages.Separated;
            _AllowedLabels[_AllowedLabels.Length - 1] = MaritalStatusesMessages.NeverMarried;
        }
    }
    
In the sample above, you increased size of the `_AllowedValues` and `_AllowedLabels` arrays to add 3 additional items to the **Marital Status** drop-down list. Internal values, stored in the `_AllowedValues` array, will be assigned to DAC fields and saved in database, and external values from the `_AllowedValues` array represent internal values in the UI.

To test the results, in the Contact DAC extension, decorate **MaritalStatus** field with the `MaritalStatusesCst1Attribute`:

    public class ContactExt : PXCacheExtension<Contact>
    {
        [PXRemoveBaseAttribute(typeof(MaritalStatusesAttribute))]
        [PXMergeAttributes(Method = MergeMethod.Append)]
        [MaritalStatusesCst1]
        public string MaritalStatus { get; set; }
    }

Now there are 7 items in the **Marital Status** drop-down list:

[![enter image description here][2]][2]

## To remove items declared in the PXStringListAttribute successor ##

To remove specific drop-down item, that was hard-coded inside an attribute inherited from the PXStringList or PXIntList attribute, you need to decrease size of the `_AllowedValues` and `_AllowedLabels` arrays in the constructor of your custom field attribute:

    public class MaritalStatusesCst2Attribute : MaritalStatusesCst1Attribute
    {
        public MaritalStatusesCst2Attribute()
        {
            string[] allowedValues = new string[_AllowedValues.Length - 1];
            string[] allowedLabels = new string[_AllowedLabels.Length - 1];
            Array.Copy(_AllowedValues, 1, allowedValues, 0, _AllowedValues.Length - 1);
            Array.Copy(_AllowedLabels, 1, allowedLabels, 0, _AllowedValues.Length - 1);
            _AllowedValues = allowedValues;
            _AllowedLabels = allowedLabels;
        }
    }
    
In the sample above, you decreased size of the `_AllowedValues` and `_AllowedLabels` arrays to remove ***Single*** item from the **Marital Status** drop-down list.

To test the results, in the Contact DAC extension, decorate **MaritalStatus** field with the `MaritalStatusesCst2Attribute`:

    public class ContactExt : PXCacheExtension<Contact>
    {
        [PXRemoveBaseAttribute(typeof(MaritalStatusesAttribute))]
        [PXMergeAttributes(Method = MergeMethod.Append)]
        [MaritalStatusesCst2]
        public string MaritalStatus { get; set; }
    }

Now there are only 6 items: 3 original and 3 custom - in the **Marital Status** drop-down list:

[![enter image description here][3]][3]

## To replace items declared in the PXStringListAttribute successor ##

To replace specific drop-down item, originally hard-coded inside an attribute inherited from the PXStringList or PXIntList attribute, you need to update appropriate value in the `_AllowedValues` and `_AllowedLabels` arrays in the constructor of your custom field attribute:

    public class MaritalStatusesCst3Attribute : MaritalStatusesCst2Attribute
    {
        public const string DivorcedNoCommonLaw = "V";

        public MaritalStatusesCst3Attribute()
        {
            _AllowedValues[Array.IndexOf(_AllowedValues, Divorced)] = DivorcedNoCommonLaw;
            _AllowedLabels[Array.IndexOf(_AllowedLabels, Messages.Divorced)] = MaritalStatusesMessages.DivorcedNoCommonLaw;
        }
    }
    
In the sample above, you replaced ***D*** - ***Divorced*** item with ***V*** - ***Divorced (not living common law)*** in the `_AllowedValues` and `_AllowedLabels` arrays respectively. By doing so, we replace both internal and external values of a drop-down item.

To test the results, in the Contact DAC extension, decorate **MaritalStatus** field with the `MaritalStatusesCst3Attribute`:

    public class ContactExt : PXCacheExtension<Contact>
    {
        [PXRemoveBaseAttribute(typeof(MaritalStatusesAttribute))]
        [PXMergeAttributes(Method = MergeMethod.Append)]
        [MaritalStatusesCst3]
        public string MaritalStatus { get; set; }
    }

Now there are only 6 items: 2 original, 3 custom and 1 replaced - in the **Marital Status** drop-down list:

[![enter image description here][4]][4]


  [1]: https://i.stack.imgur.com/ak0FZ.png
  [2]: https://i.stack.imgur.com/gYomam.png
  [3]: https://i.stack.imgur.com/2DI6Sm.png
  [4]: https://i.stack.imgur.com/oAKA9m.png

