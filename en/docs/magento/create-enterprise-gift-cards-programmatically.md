---
title: "Create Enterprise Gift Cards Programmatically"
slug: "create-enterprise-gift-cards-programmatically"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Create Single Gift Card
    // Instantiate Gift Card Model
    $gift_card = Mage::getModel('enterprise_giftcardaccount/giftcardaccount');

    // Populate Gift Card values
    $gift_card
        // Your redeemable code, doesn't have to be in this format.
        ->setCode('2i2j2j-24k1ii1-67774k-231l')
        // Also has STATUS_DISABLED
        ->setStatus($gift_card::STATUS_ENABLED)
        // YYYY-MM-DD format date
        ->setDateExpires('2015-04-15')
        ->setWebsiteId(1)
        // Also has STATE_USED, STATE_REDEEMED, and STATE_EXPIRED
        ->setState($gift_card::STATE_AVAILABLE)
        // Also has NOT_REDEEMABLE value.
        ->setIsRedeemable($gift_card::REDEEMABLE)
        // Int or float (or String that can be parsed into either) currency amount.
        ->setBalance(25);

    // Save the fleshed out gift card.
    $gift_card->save();

    // Can use the gift card for further use, return it, or return it's ID
    return $gift_card // ->getId();

