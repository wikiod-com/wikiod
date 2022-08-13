---
title: "Testing"
slug: "testing"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Simple Testing in Symfony3
**Unit Test**

Unit tests are used to ensure that your code has no syntax error and to test the logic of your code to work as what you expected. Quick example:

src/AppBundle/Calculator/BillCalculator.php
``` php
<?php

namespace AppBundle\Calculator;

use AppBundle\Calculator\TaxCalculator;

class BillCalculator
{
    private $taxCalculator;

    public function __construct(TaxCalculator $taxCalculator)
    {
        $this->taxCalculator = $taxCalculator;
    }

    public function calculate($products)
    {
        $totalPrice = 0;
        foreach ($products as $product) {
            $totalPrice += $product['price'];
        }
        $tax = $this->taxCalculator->calculate($totalPrice);
        
        return $totalPrice + $tax;
    }
}
```

src/AppBundle/Calculator/TaxCalculator.php
``` php
<?php

namespace AppBundle\Calculator;

class TaxCalculator
{
    public function calculate($price)
    {
        return $price * 0.1; // for example the tax is 10%
    }
}
```

tests/AppBundle/Calculator/BillCalculatorTest.php
``` php
<?php

namespace Tests\AppBundle\Calculator;

class BillCalculatorTest extends \PHPUnit_Framework_TestCase
{
    public function testCalculate()
    {
        $products = [
            [
                'name' => 'A',
                'price' => 100,
            ],
            [
                'name' => 'B',
                'price' => 200,
            ],
        ];
        $taxCalculator = $this->getMock(\AppBundle\Calculator\TaxCalculator::class);

        // I expect my BillCalculator to call $taxCalculator->calculate once
        // with 300 as the parameter
        $taxCalculator->expects($this->once())->method('calculate')->with(300)->willReturn(30);

        $billCalculator = new BillCalculator($taxCalculator);
        $price = $billCalculator->calculate($products);

        $this->assertEquals(330, $price);
    }
}
```

I tested my BillCalculator class so I can ensure that my BillCalculator will return total products price + 10% tax. In unit test, we create our own test case. In this test, I provide 2 products (the prices are 100 and 200), so the tax will be 10% = 30. I expect the TaxCalculator to return 30, so that the total price will be 300 + 30 = 330.

**Functional Test**

Functional tests are used to test the input and output. With the given input, I expected some output without testing the process to create the output. (this is different with unit test because in unit test, we test the code flow). Quick example:

``` php
namespace Tests\AppBundle;

use Symfony\Bundle\FrameworkBundle\Test\WebTestCase;

class ApplicationAvailabilityFunctionalTest extends WebTestCase
{
    /**
     * @dataProvider urlProvider
     */
    public function testPageIsSuccessful($url)
    {
        $client = self::createClient();
        $client->request('GET', $url);

        $this->assertTrue($client->getResponse()->isSuccessful());
    }

    public function urlProvider()
    {
        return array(
            array('/'),
            array('/posts'),
            array('/post/fixture-post-1'),
            array('/blog/category/fixture-category'),
            array('/archives'),
            // ...
        );
    }
}
```

I tested my controller so i can ensure that my controller will return 200 response instead of 400 (Not Found) or 500 (Internal Server Error) with the given url.

References:
- http://symfony.com/doc/current/best_practices/tests.html
- http://symfony.com/doc/current/book/testing.html

