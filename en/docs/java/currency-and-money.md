---
title: "Currency and Money"
slug: "currency-and-money"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

## Add custom currency
Required JARs on classpath: 
- javax.money:money-api:1.0   (JSR354 money and currency api)
- org.javamoney:moneta:1.0    (Reference implementation)
- javax:annotation-api:1.2.   (Common annotations used by reference implementation)


    // Let's create non-ISO currency, such as bitcoin

    // At first, this will throw UnknownCurrencyException
    MonetaryAmount moneys = Money.of(new BigDecimal("0.1"), "BTC");

    // This happens because bitcoin is unknown to default currency
    // providers
    System.out.println(Monetary.isCurrencyAvailable("BTC")); // false

    // We will build new currency using CurrencyUnitBuilder provided by org.javamoney.moneta
    CurrencyUnit bitcoin = CurrencyUnitBuilder
        .of("BTC", "BtcCurrencyProvider") // Set currency code and currency provider name
        .setDefaultFractionDigits(2)      // Set default fraction digits
        .build(true);                     // Build new currency unit. Here 'true' means
                                          // currency unit is to be registered and
                                          // accessible within default monetary context
    
    // Now BTC is available
    System.out.println(Monetary.isCurrencyAvailable("BTC")); // True

