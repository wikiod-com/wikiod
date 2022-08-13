---
title: "Get products from database"
slug: "get-products-from-database"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Get products using the Product Repository
To get products from the database, you need to use Magento 2's repository design pattern. Each module can be bundled with it's own repositories, and the Product Catalog module is not any different.

You can use [dependency injection][1] in your class to access the repository. A working example would look like this:
<!-- language-all: php -->

    class Example
    {
        /**
         * @var \Magento\Catalog\Model\ProductRepository
         */
        protected $productRepository;

        /**
         * @param \Magento\Catalog\Model\ProductRepository $productRepository
         */
        public function __construct(
            \Magento\Catalog\Model\ProductRepository $productRepository
        ) {
            $this->productRepository = $productRepository;
        }

        /**
         * Get product by ID
         * @return \Magento\Catalog\Api\Data\ProductInterface
         * @throws \Magento\Framework\Exception\NoSuchEntityException
         */
        public function getProductById(int $productId)
        {
            return $this->productRepository->getById($productId);
        }
    }

A Repository has more functionality, like saving or deleting a product, as well as getting a list of products and using a filter, but that's beyond the scope of this example.

  [1]: https://www.wikiod.com/magento2/dependency-injection#Constructor Injection

