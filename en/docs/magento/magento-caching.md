---
title: "Magento Caching"
slug: "magento-caching"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## How to cache custom data into Magento
    const CACHE_TAG_NAMESPACE_MODULE = "YOUR_MODULES_CACHE_TAGS";
    $cacheGroup = 'namespace_module';
    $useCache = Mage::app()->useCache($cacheGroup);
    if (true === $useCache) {    
        $cacheId = 'unique_name';
        if ($cacheContent = Mage::app()->loadCache($cacheId)) {
            $html = $cacheContent;
            return $html;
        } else {
            try {
                $cacheContent = $html;
                $tags = array(model::CACHE_TAG_NAMESPACE_MODULE);
                $lifetime = Mage::getStoreConfig('core/cache/lifetime');
                Mage::app()->saveCache($cacheContent, $cacheId, $tags, $lifetime);
            } catch (Exception $e) {
                // Exception = no caching
                Mage::logException($e);
            }
            return $html;
       }
    }
    // Default:
    return $html;

## Clean cache by cache ID
    Mage::app()->removeCache($cacheId); 

Flush all Magento cache entries

    Mage::app()->cleanCache()

or:

    Mage::app()->getCacheInstance()->flush();



## Use Redis as a cache backend
Redis configuration:

1. Install redis (2.4+ required)
2. Install phpredis
3. Install the Magento extension `Cm_Cache_Backend_Redis` (only for Magento 1.7 and below)
4. Edit your `app/etc/local.xml`:


    <global>
      ...
      <cache>
        <backend>Cm_Cache_Backend_Redis</backend>
        <backend_options>
          <server>127.0.0.1</server> <!-- or absolute path to unix socket -->
          <port>6379</port>
          <persistent></persistent>
          <database>0</database>
          <password></password>
          <force_standalone>0</force_standalone>
          <connect_retries>1</connect_retries>
          <automatic_cleaning_factor>0</automatic_cleaning_factor>
          <compress_data>1</compress_data>
          <compress_tags>1</compress_tags>
          <compress_threshold>20480</compress_threshold>
          <compression_lib>gzip</compression_lib> <!-- Supports gzip, lzf and snappy -->
        </backend_options>
      </cache>
      ... 
    </global>

