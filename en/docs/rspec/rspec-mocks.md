---
title: "RSpec Mocks"
slug: "rspec-mocks"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

This topic documents RSpec's support for test doubles (stubs, mocks, etc.). That support is provided by [the rspec-mocks gem][1].


  [1]: https://github.com/rspec/rspec-mocks

## Stubbing with allow

The following example uses `allow` and `receive` to stub a `Cart`'s call to a `CreditCardService` so that the example doesn't have to wait for a network call or use a credit card number that the processor knows about.

    class Cart
      def check_out
        begin
          transaction_id = CreditCardService.instance.validate credit_card_number, total_price
          order = Order.new
          order.items = cart.items
          order
        rescue CreditCardService::ValidationFailedError
          # handle the error
        end
      end
    end

    describe Cart do
      describe '#check_out' do
        it "places an order" do
          allow(CreditCardService.instance).
            to receive(:validate).with("1234567812345678", 3700).and_return("transaction_id")
          cart = Cart.new
          cart.items << Item.new("Propeller beanie", 3700)
          order = cart.check_out
          expect(order.transaction_id).to eq("transaction_id")
        end
      end
    end

`with` is optional; without it, any arguments are accepted. `and_return` is optional too; without it the stub returns `nil`.

## Mocking by setting a message expectation with expect
The following example uses `expect` and `receive` to mock an `Order`'s call to a `CreditCardService`, so that the test passes only if the call is made without having to actually make it.

    class Order
      def cancel
         CreditCardService.instance.refund transaction_id
      end
    end

    describe Order do
      describe '#cancel' do
        it "refunds the money" do
          order = Order.new
          order.transaction_id = "transaction_id"
          expect(CreditCardService.instance).to receive(:refund).with("transaction_id")
          order.cancel
        end
      end
    end

In this example the mock is on the return value of `CreditCardService.instance`, which is presumably a singleton.

`with` is optional; without it, any call to `refund` would satisfy the expectation. A return value could be given with `and_return`; in this example it is not used, so the call returns `nil`.

