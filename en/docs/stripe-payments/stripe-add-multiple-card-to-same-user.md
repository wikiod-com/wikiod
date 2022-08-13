---
title: "Stripe Add multiple card to Same User"
slug: "stripe-add-multiple-card-to-same-user"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Create customer in stripe
    public function createCustomer($data , $token)//pass form data and token id
    {
        $customer=Customer::create(array(
        "email"=>$data['email'],
        "description" => $data['name'],
        "source" => $token // obtained with Stripe.js
        ));
        return $customer['id'];
    }

For more Information follow this [Link][1]


  [1]: https://stripe.com/docs/api#create_customer

## How to retrive customer And add cards in Stripe
    public function addCard($cust_id, $token)
    {
        $retriveResult=Customer::retrieve($cust_id);
        $tokendata = Token::retrieve($token);
        $newcard = $tokendata['card'];
        $flag = 1;

        foreach ($retriveResult['sources']['data'] as $card) {
            if($card['fingerprint'] === $newcard['fingerprint'])
            {
                $cardid = $card['id'];
                $flag = 0;
                break;
            }
        }

        if($flag)
        {
            $savecard = $retriveResult->sources->create(array("source" =>$token));
            $cardid = $savecard['id'];
        }
        return $cardid;
    }

