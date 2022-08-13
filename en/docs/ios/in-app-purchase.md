---
title: "In-App Purchase"
slug: "in-app-purchase"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

## Single IAP in Swift 2
After creating an IAP in iTunesConnect:

In the view controller that you want to buy in

    import StoreKit

and add the relevant delegates

    class ViewController: UIViewController, SKProductsRequestDelegate, SKPaymentTransactionObserver {
   
declare a variable with the product id from iTunesConnect

    var product_id: NSString?
    
    override func viewDidLoad() {
        
        
        product_id = "YOUR_PRODUCT_ID"
        super.viewDidLoad()
        SKPaymentQueue.defaultQueue().addTransactionObserver(self)

       //Check if product is purchased
       if (NSUserDefaults.standardUserDefaults().boolForKey("purchased")){
       
           // Hide ads
           adView.hidden = true
            
       } else {
           print("Should show ads...")
            
       }

    }


wire a button to a function to purchase the IAP
    
    @IBAction func unlockAction(sender: AnyObject) {
        
       print("About to fetch the product...")

    // Can make payments
    if (SKPaymentQueue.canMakePayments())
        {
            let productID:NSSet = NSSet(object: self.product_id!);
            let productsRequest:SKProductsRequest = SKProductsRequest(productIdentifiers: productID as! Set<NSString>);
            productsRequest.delegate = self;
            productsRequest.start();
            println("Fetching Products");
        }else{
            print("Can't make purchases");
        }
    
    }
    
And here are some helper methods
    
    func buyProduct(product: SKProduct){
        println("Sending the Payment Request to Apple");
        let payment = SKPayment(product: product)
        SKPaymentQueue.defaultQueue().addPayment(payment);
        
     }
    
    
the delegate methods that must be declared
    
    func productsRequest (request: SKProductsRequest, didReceiveResponse response: SKProductsResponse) {

        let count : Int = response.products.count
        if (count>0) {
            var validProduct: SKProduct = response.products[0] as SKProduct
            if (validProduct.productIdentifier == self.product_id) {
                print(validProduct.localizedTitle)
                print(validProduct.localizedDescription)
                print(validProduct.price)
                buyProduct(validProduct);
            } else {
                print(validProduct.productIdentifier)
            }
        } else {
            print("nothing")
        }
    }
    
    
    func request(request: SKRequest!, didFailWithError error: NSError!) {
        print("Error Fetching product information");
    }
    
        func paymentQueue(_ queue: SKPaymentQueue,
    updatedTransactions transactions: [SKPaymentTransaction])

    {
        print("Received Payment Transaction Response from Apple");
        
        for transaction:AnyObject in transactions {
            if let trans:SKPaymentTransaction = transaction as? SKPaymentTransaction{
                switch trans.transactionState {
                case .Purchased:
                    print("Product Purchased");
                    SKPaymentQueue.defaultQueue().finishTransaction(transaction as! SKPaymentTransaction)
                    // Handle the purchase
                    NSUserDefaults.standardUserDefaults().setBool(true , forKey: "purchased")
                    adView.hidden = true
                    break;
                case .Failed:
                    print("Purchased Failed");
                    SKPaymentQueue.defaultQueue().finishTransaction(transaction as! SKPaymentTransaction)
                    break;
                    
            

                case .Restored:
                    print("Already Purchased");
                   SKPaymentQueue.defaultQueue().restoreCompletedTransactions() 


                     // Handle the purchase
                        NSUserDefaults.standardUserDefaults().setBool(true , forKey: "purchased")
                        adView.hidden = true
                        break;
                default:
                    break;
                }
            }
        }
        
    }

And then the code to restore a non-consumable in app purchase


    if (SKPaymentQueue.canMakePayments()) {
      SKPaymentQueue.defaultQueue().restoreCompletedTransactions()
    }

## Set Up in iTunesConnect
In [iTunesConnect][1], select the app which you want to add an IAP to.

Click on features and you will see this:

[![enter image description here][2]][2]

Click the plus. You will then need to select which type of IAP you want to make.

Then you will need to fill out all of the information for your IAP.

[![enter image description here][3]][3]

If you have any trouble you can consult the [IAP Set Up Guide][4].


  [1]: https://itunesconnect.apple.com/
  [2]: http://i.stack.imgur.com/gAqOh.png
  [3]: http://i.stack.imgur.com/HXFv5.png
  [4]: https://developer.apple.com/library/ios/documentation/LanguagesUtilities/Conceptual/iTunesConnectInAppPurchase_Guide/Chapters/Introduction.html

## Most basic steps for purchasing/subscribing a user to an IAP
Assuming you know the `productID`:

First

    import StoreKit

Then in your code

    let productID: Set = ["premium"]
    let request = SKProductsRequest(productIdentifiers: productID)
    request.delegate = self
    request.start()

and in the `SKProductsRequestDelegate`:

    func productsRequest(request: SKProductsRequest, didReceiveResponse response: SKProductsResponse) {
        if response.products.count > 0 {
            let product = response.products[0]
            let payment = SKPayment(product: product)
            SKPaymentQueue.defaultQueue().addPayment(payment)
        }
    }
    

