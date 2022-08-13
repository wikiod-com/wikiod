---
title: "Sql script to delete test data"
slug: "sql-script-to-delete-test-data"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Sql script to delete test data of products, customers, logs and sales.

## Delete Customer test data
    SET FOREIGN_KEY_CHECKS=0;
    
    -- Customers
    TRUNCATE `customer_address_entity`;
    TRUNCATE `customer_address_entity_datetime`;
    TRUNCATE `customer_address_entity_decimal`;
    TRUNCATE `customer_address_entity_int`;
    TRUNCATE `customer_address_entity_text`;
    TRUNCATE `customer_address_entity_varchar`;
    TRUNCATE `customer_entity`;
    TRUNCATE `customer_entity_datetime`;
    TRUNCATE `customer_entity_decimal`;
    TRUNCATE `customer_entity_int`;
    TRUNCATE `customer_entity_text`;
    TRUNCATE `customer_entity_varchar`;
    ALTER TABLE `customer_address_entity` AUTO_INCREMENT=1;
    ALTER TABLE `customer_address_entity_datetime` AUTO_INCREMENT=1;
    ALTER TABLE `customer_address_entity_decimal` AUTO_INCREMENT=1;
    ALTER TABLE `customer_address_entity_int` AUTO_INCREMENT=1;
    ALTER TABLE `customer_address_entity_text` AUTO_INCREMENT=1;
    ALTER TABLE `customer_address_entity_varchar` AUTO_INCREMENT=1;
    ALTER TABLE `customer_entity` AUTO_INCREMENT=1;
    ALTER TABLE `customer_entity_datetime` AUTO_INCREMENT=1;
    ALTER TABLE `customer_entity_decimal` AUTO_INCREMENT=1;
    ALTER TABLE `customer_entity_int` AUTO_INCREMENT=1;
    ALTER TABLE `customer_entity_text` AUTO_INCREMENT=1;
    ALTER TABLE `customer_entity_varchar` AUTO_INCREMENT=1;
    
    -- Search
    TRUNCATE `catalogsearch_query`;
    TRUNCATE `catalogsearch_fulltext`;
    TRUNCATE `catalogsearch_result`;
    ALTER TABLE `catalogsearch_query` AUTO_INCREMENT=1;
    ALTER TABLE `catalogsearch_fulltext` AUTO_INCREMENT=1;
    ALTER TABLE `catalogsearch_result` AUTO_INCREMENT=1;
    
    -- Polls
    TRUNCATE `poll`;
    TRUNCATE `poll_answer`;
    TRUNCATE `poll_store`;
    TRUNCATE `poll_vote`;
    ALTER TABLE `poll` AUTO_INCREMENT=1;
    ALTER TABLE `poll_answer` AUTO_INCREMENT=1;
    ALTER TABLE `poll_store` AUTO_INCREMENT=1;
    ALTER TABLE `poll_vote` AUTO_INCREMENT=1;
    
    -- Reports
    TRUNCATE `report_viewed_product_index`;
    ALTER TABLE `report_viewed_product_index` AUTO_INCREMENT=1;
    
    -- Newsletter
    TRUNCATE `newsletter_queue`;
    TRUNCATE `newsletter_queue_link`;
    TRUNCATE `newsletter_subscriber`;
    TRUNCATE `newsletter_problem`;
    TRUNCATE `newsletter_queue_store_link`;
    ALTER TABLE `newsletter_queue` AUTO_INCREMENT=1;
    ALTER TABLE `newsletter_subscriber` AUTO_INCREMENT=1;
    ALTER TABLE `newsletter_problem` AUTO_INCREMENT=1;
    ALTER TABLE `newsletter_queue_store_link` AUTO_INCREMENT=1;
    
    -- Wishlist
    TRUNCATE `wishlist`;
    ALTER TABLE `wishlist` AUTO_INCREMENT=1;
    
    SET FOREIGN_KEY_CHECKS=1;

## Delete Product test data
    SET FOREIGN_KEY_CHECKS = 0;
    
    TRUNCATE TABLE `catalog_product_bundle_option`;
    TRUNCATE TABLE `catalog_product_bundle_option_value`;
    TRUNCATE TABLE `catalog_product_bundle_selection`;
    TRUNCATE TABLE `catalog_product_entity_datetime`;
    TRUNCATE TABLE `catalog_product_entity_decimal`;
    TRUNCATE TABLE `catalog_product_entity_gallery`;
    TRUNCATE TABLE `catalog_product_entity_int`;
    TRUNCATE TABLE `catalog_product_entity_media_gallery`;
    TRUNCATE TABLE `catalog_product_entity_media_gallery_value`;
    TRUNCATE TABLE `catalog_product_entity_text`;
    TRUNCATE TABLE `catalog_product_entity_tier_price`;
    TRUNCATE TABLE `catalog_product_entity_varchar`;
    TRUNCATE TABLE `catalog_product_link`;
    TRUNCATE TABLE `catalog_product_link_attribute`;
    TRUNCATE TABLE `catalog_product_link_attribute_decimal`;
    TRUNCATE TABLE `catalog_product_link_attribute_int`;
    TRUNCATE TABLE `catalog_product_link_attribute_varchar`;
    TRUNCATE TABLE `catalog_product_link_type`;
    TRUNCATE TABLE `catalog_product_option`;
    TRUNCATE TABLE `catalog_product_option_price`;
    TRUNCATE TABLE `catalog_product_option_title`;
    TRUNCATE TABLE `catalog_product_option_type_price`;
    TRUNCATE TABLE `catalog_product_option_type_title`;
    TRUNCATE TABLE `catalog_product_option_type_value`;
    TRUNCATE TABLE `catalog_product_super_attribute`;
    TRUNCATE TABLE `catalog_product_super_attribute_label`;
    TRUNCATE TABLE `catalog_product_super_attribute_pricing`;
    TRUNCATE TABLE `catalog_product_super_link`;
    TRUNCATE TABLE `catalog_product_enabled_index`;
    TRUNCATE TABLE `catalog_product_website`;
    TRUNCATE TABLE `catalog_product_entity`;
    TRUNCATE TABLE `cataloginventory_stock_item`;
    TRUNCATE TABLE `cataloginventory_stock_status`;
    TRUNCATE TABLE `catalog_category_entity`;
    TRUNCATE TABLE `catalog_category_entity_datetime`;
    TRUNCATE TABLE `catalog_category_entity_decimal`;
    TRUNCATE TABLE `catalog_category_entity_int`;
    TRUNCATE TABLE `catalog_category_entity_text`;
    TRUNCATE TABLE `catalog_category_entity_varchar`;
    TRUNCATE TABLE `catalog_category_product`;
    TRUNCATE TABLE `catalog_category_product_index`;
    TRUNCATE TABLE `catalog_product_relation`;
    TRUNCATE TABLE `catalog_product_flat_1`;
    TRUNCATE TABLE `catalog_category_flat_store_1`;
    TRUNCATE TABLE `catalog_category_flat_store_2`;
    TRUNCATE TABLE `catalog_category_flat_store_3`;
    
    -- Tags
    TRUNCATE `tag`;
    TRUNCATE `tag_relation`;
    TRUNCATE `tag_summary`;
    ALTER TABLE `tag` AUTO_INCREMENT=1;
    ALTER TABLE `tag_relation` AUTO_INCREMENT=1;
    ALTER TABLE `tag_summary` AUTO_INCREMENT=1;
    
    SET FOREIGN_KEY_CHECKS = 1;

## Delete Sales test data
    SET FOREIGN_KEY_CHECKS=0;
    
    TRUNCATE `sales_payment_transaction`;
    TRUNCATE `sales_flat_creditmemo`;
    TRUNCATE `sales_flat_creditmemo_comment`;
    TRUNCATE `sales_flat_creditmemo_grid`;
    TRUNCATE `sales_flat_creditmemo_item`;
    TRUNCATE `sales_flat_order`;
    TRUNCATE `sales_flat_order_address`;
    TRUNCATE `sales_flat_order_grid`;
    TRUNCATE `sales_flat_order_item`;
    TRUNCATE `sales_flat_order_status_history`;
    TRUNCATE `sales_flat_quote`;
    TRUNCATE `sales_flat_quote_address`;
    TRUNCATE `sales_flat_quote_address_item`;
    TRUNCATE `sales_flat_quote_item`;
    TRUNCATE `sales_flat_quote_item_option`;
    TRUNCATE `sales_flat_order_payment`;
    TRUNCATE `sales_flat_quote_payment`;
    TRUNCATE `sales_flat_quote_shipping_rate`;
    TRUNCATE `sales_flat_shipment`;
    TRUNCATE `sales_flat_shipment_item`;
    TRUNCATE `sales_flat_shipment_grid`;
    TRUNCATE `sales_flat_shipment_track`;
    TRUNCATE `sales_flat_shipment_comment`;
    TRUNCATE `sales_flat_invoice`;
    TRUNCATE `sales_flat_invoice_grid`;
    TRUNCATE `sales_flat_invoice_item`;
    TRUNCATE `sales_flat_invoice_comment`;
    TRUNCATE `sales_order_tax`;
    TRUNCATE `sales_order_tax_item`;
    
    -- Reports
    TRUNCATE `sales_bestsellers_aggregated_daily`;
    TRUNCATE `sales_bestsellers_aggregated_monthly`;
    TRUNCATE `sales_bestsellers_aggregated_yearly`;
    TRUNCATE `sales_invoiced_aggregated`;
    TRUNCATE `sales_invoiced_aggregated_order`;
    TRUNCATE `sales_order_aggregated_created`;
    TRUNCATE `sales_order_aggregated_updated`;
    TRUNCATE `sales_refunded_aggregated`;
    TRUNCATE `sales_refunded_aggregated_order`;
    TRUNCATE `sales_shipping_aggregated`;
    TRUNCATE `sales_shipping_aggregated_order`;
    TRUNCATE `coupon_aggregated`;
    TRUNCATE `review`;
    TRUNCATE `review_detail`;
    TRUNCATE `review_entity_summary`;
    TRUNCATE `rating_store`;
    
    
    ALTER TABLE `sales_payment_transaction` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_creditmemo` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_creditmemo_comment` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_creditmemo_grid` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_creditmemo_item` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_order` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_order_address` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_order_grid` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_order_item` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_order_status_history` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_quote` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_quote_address` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_quote_address_item` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_quote_item` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_quote_item_option` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_order_payment` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_quote_payment` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_quote_shipping_rate` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_shipment` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_shipment_item` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_shipment_track` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_shipment_comment` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_invoice` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_invoice_grid` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_invoice_item` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_invoice_comment` AUTO_INCREMENT=1;
    ALTER TABLE `sales_flat_shipment_grid` AUTO_INCREMENT=1;
    ALTER TABLE `sales_order_tax` AUTO_INCREMENT=1;
    ALTER TABLE `sales_order_tax_item` AUTO_INCREMENT=1;
    ALTER TABLE `sales_invoiced_aggregated` AUTO_INCREMENT=1;
    ALTER TABLE `sales_invoiced_aggregated_order` AUTO_INCREMENT=1;
     
    TRUNCATE `eav_entity_store`;
    ALTER TABLE `eav_entity_store` AUTO_INCREMENT=1;
    
    SET FOREIGN_KEY_CHECKS=1;

## Delete Logs test data
    SET FOREIGN_KEY_CHECKS=0;
    
    TRUNCATE `log_customer`;
    TRUNCATE `log_visitor`;
    TRUNCATE `log_visitor_info`;
    TRUNCATE `log_visitor_online`;
    TRUNCATE `log_quote`;
    TRUNCATE `log_summary`;
    TRUNCATE `log_summary_type`;
    TRUNCATE `log_url`;
    TRUNCATE `log_url_info`;
    TRUNCATE `sendfriend_log`;
    TRUNCATE `report_event`;
    TRUNCATE `dataflow_batch_import`;
    TRUNCATE `dataflow_batch_export`;
    TRUNCATE `index_process_event`;
    TRUNCATE `index_event`;
    ALTER TABLE `log_customer` AUTO_INCREMENT=1;
    ALTER TABLE `log_visitor` AUTO_INCREMENT=1;
    ALTER TABLE `log_visitor_info` AUTO_INCREMENT=1;
    ALTER TABLE `log_visitor_online` AUTO_INCREMENT=1;
    ALTER TABLE `log_quote` AUTO_INCREMENT=1;
    ALTER TABLE `log_summary` AUTO_INCREMENT=1;
    ALTER TABLE `log_url_info` AUTO_INCREMENT=1;
    ALTER TABLE `sendfriend_log` AUTO_INCREMENT=1;
    ALTER TABLE `report_event` AUTO_INCREMENT=1;
    ALTER TABLE `dataflow_batch_import` AUTO_INCREMENT=1;
    ALTER TABLE `dataflow_batch_export` AUTO_INCREMENT=1;
    ALTER TABLE `index_event` AUTO_INCREMENT=1;
    
    SET FOREIGN_KEY_CHECKS=1;

