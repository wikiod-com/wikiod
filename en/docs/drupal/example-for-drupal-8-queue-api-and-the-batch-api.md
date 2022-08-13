---
title: "Example for Drupal 8 Queue API and the Batch API"
slug: "example-for-drupal-8-queue-api-and-the-batch-api"
draft: false
images: []
weight: 9880
type: docs
toc: true
---

## An example module to help understanding the Queue API and the Batch API in Drupal 8
**xml_import_example.info.yml**

    type: module
    name: XML import example
    package: Examples
    description: "This module helps understanding the Batch API and Queue API with an XML import example"
    core: 8.x

**xml_import_example.permissions.yml**

    import content from xml:
      title: 'Import content from xml'
      description: 'With this permission user can import contents from a XML source'
      restrict access: TRUE

**xml_import_example.routing.yml**

    # Get contents from the xml source
    xml_import_example.get_contents_from_xml:
      path: '/get-contents-from-xml'
      defaults: { _controller: '\Drupal\xml_import_example\Controller\ImportContentFromXML::getContentsFromXMLPage' }
      requirements:
        _permission: 'import content from xml'
    # Process all queue items with batch
    xml_import_example.process_all_queue_items_with_batch:
      path: '/process-all-queue-items'
      defaults: { _controller: '\Drupal\xml_import_example\Controller\ImportContentFromXML::processAllQueueItemsWithBatch' }
      requirements:
        _permission: 'import content from xml'

**src/Controller/ImportContentFromXML.php**

    <?php
    /**
     * @file
     * Contains \Drupal\xml_import_example\Controller\ImportContentFromXML.
     */

    namespace Drupal\xml_import_example\Controller;

    use Symfony\Component\DependencyInjection\ContainerInterface;
    use Drupal\Core\Controller\ControllerBase;
    use Drupal\Core\Queue\QueueWorkerManager;
    use Drupal\Core\Queue\QueueFactory;

    /**
     * You can use this constant to set how many queued items
     * you want to be processed in one batch operation 
     */
    define("IMPORT_XML_BATCH_SIZE", 1);

    class ImportContentFromXML extends ControllerBase {
      
      /**
       * We add QueueFactory and QueueWorkerManager services with the Dependency Injection solution
       */
       
      /**
       * @var QueueFactory
       */
      protected $queueFactory;

      /**
       * @var QueueWorkerManager
       */
      protected $queueManager;
      
      /**
       * {@inheritdoc}
       */
      public function __construct(QueueFactory $queue_factory, QueueWorkerManager $queue_manager) {
        $this->queue_factory = $queue_factory;
        $this->queue_manager = $queue_manager;
      }
      
      /**
       * {@inheritdoc}
       */
      public static function create(ContainerInterface $container) {
        $queue_factory = $container->get('queue');
        $queue_manager = $container->get('plugin.manager.queue_worker');
        
        return new static($queue_factory, $queue_manager);
      }
      
      /**
       * Get XML from the API and convert it to 
       */
      protected function getContentsFromXML() {
        // Here you should get the XML content and convert it to an array of content arrays for example
        // I use now an example array of contents:
        $contents = array();
        
        for ($i = 1; $i <= 20; $i++) {
          $contents[] = array(
            'title' => 'Test title ' . $i,
            'body' => 'Test body ' . $i,
          );
        }
        
        // Return with the contents    
        return $contents;
      }
      
      /**
       * Page where the xml source is preprocessed
       */
      public function getContentsFromXMLPage() {
        // Get contents array
        $contents = $this->getContentsFromXML();
        
        foreach ($contents as $content) {
          // Get the queue implementation for import_content_from_xml queue
          $queue = $this->queue_factory->get('import_content_from_xml');
          
          // Create new queue item
          $item = new \stdClass();
          $item->data = $content;
          $queue->createItem($item);
        }
        
        return array(
          '#type' => 'markup',
          '#markup' => $this->t('@count queue items are created.', array('@count' => count($contents))),
        );
      }
      
      /**
       * Process all queue items with batch
       */
      public function processAllQueueItemsWithBatch() {
        
        // Create batch which collects all the specified queue items and process them one after another
        $batch = array(
          'title' => $this->t("Process all XML Import queues with batch"),
          'operations' => array(),
          'finished' => 'Drupal\xml_import_example\Controller\ImportContentFromXML::batchFinished',
        );
        
        // Get the queue implementation for import_content_from_xml queue
        $queue_factory = \Drupal::service('queue');
        $queue = $queue_factory->get('import_content_from_xml');
        
        // Count number of the items in this queue, and create enough batch operations
        for($i = 0; $i < ceil($queue->numberOfItems() / IMPORT_XML_BATCH_SIZE); $i++) {
          // Create batch operations
          $batch['operations'][] = array('Drupal\xml_import_example\Controller\ImportContentFromXML::batchProcess', array());
        }
        
        // Adds the batch sets
        batch_set($batch);
        // Process the batch and after redirect to the frontpage
        return batch_process('<front>');
      }

      /**
       * Common batch processing callback for all operations.
       */
      public static function batchProcess(&$context) {
        
        // We can't use here the Dependency Injection solution
        // so we load the necessary services in the other way
        $queue_factory = \Drupal::service('queue');
        $queue_manager = \Drupal::service('plugin.manager.queue_worker');
        
        // Get the queue implementation for import_content_from_xml queue
        $queue = $queue_factory->get('import_content_from_xml');
        // Get the queue worker
        $queue_worker = $queue_manager->createInstance('import_content_from_xml');
        
        // Get the number of items
        $number_of_queue = ($queue->numberOfItems() < IMPORT_XML_BATCH_SIZE) ? $queue->numberOfItems() : IMPORT_XML_BATCH_SIZE;
        
        // Repeat $number_of_queue times
        for ($i = 0; $i < $number_of_queue; $i++) {
          // Get a queued item
          if ($item = $queue->claimItem()) {
            try {
              // Process it
              $queue_worker->processItem($item->data);
              // If everything was correct, delete the processed item from the queue
              $queue->deleteItem($item);
            }
            catch (SuspendQueueException $e) {
              // If there was an Exception trown because of an error
              // Releases the item that the worker could not process.
              // Another worker can come and process it
              $queue->releaseItem($item);
              break;
            }
          }
        }
      }

      /**
       * Batch finished callback.
       */
      public static function batchFinished($success, $results, $operations) {
        if ($success) {
         drupal_set_message(t("The contents are successfully imported from the XML source."));
        }
        else {
          $error_operation = reset($operations);
          drupal_set_message(t('An error occurred while processing @operation with arguments : @args', array('@operation' => $error_operation[0], '@args' => print_r($error_operation[0], TRUE))));
        }
      }
    }

**src/Plugin/QueueWorker/ImportContentFromXMLQueueBase.php**

    <?php

    /**
     * @file
     * Contains Drupal\xml_import_example\Plugin\QueueWorker\ImportContentFromXMLQueueBase
     */

    namespace Drupal\xml_import_example\Plugin\QueueWorker;

    use Drupal\Core\Plugin\ContainerFactoryPluginInterface;
    use Drupal\Core\Queue\QueueWorkerBase;
    use Drupal\Core\Queue\SuspendQueueException;
    use Symfony\Component\DependencyInjection\ContainerInterface;
    use Drupal\node\Entity\Node;

    /**
     * Provides base functionality for the Import Content From XML Queue Workers.
     */
    abstract class ImportContentFromXMLQueueBase extends QueueWorkerBase implements ContainerFactoryPluginInterface {
      
      // Here we don't use the Dependency Injection, 
      // but the create method and __construct method are necessary to implement
      
      /**
       * {@inheritdoc}
       */
      public function __construct() {}
      
      /**
       * {@inheritdoc}
       */
      public static function create(ContainerInterface $container, array $configuration, $plugin_id, $plugin_definition) {
        return new static();
      }
      
      /**
       * {@inheritdoc}
       */
      public function processItem($item) {
        // Get the content array
        $content = $item->data;
        // Create node from the array
        $this->createContent($content);
      }
      
      /**
       * Create content
       *
       * @return int
       */
      protected function createContent($content) {
        // Create node object from the $content array
        $node = Node::create(array(
          'type'  => 'page',
          'title' => $content['title'],
          'body'  => array(
            'value'  => $content['body'],
            'format' => 'basic_html',
          ),
        ));
        $node->save();
      }
    }

**src/Plugin/QueueWorker/ImportContentFromXMLQueue.php**

    <?php

    namespace Drupal\xml_import_example\Plugin\QueueWorker;

    /**
     * Create node object from the imported XML content
     *
     * @QueueWorker(
     *   id = "import_content_from_xml",
     *   title = @Translation("Import Content From XML"),
     *   cron = {"time" = 60}
     * )
     */
    class ImportContentFromXMLQueue extends ImportContentFromXMLQueueBase {}

So this is the working module, you can test it in you site.

If you visit the **/get-contents-from-xml** URL 20 queue items are made from a contents array.

The **src/Plugin/QueueWorker/ImportContentFromXMLQueue.php** contains this annotation:
**cron = {"time" = 60}**

So if you run cron, the queue items are processed for maximum 60 seconds.
You can increase or decrease this time, with that annotation.

If you remove the **cron = {"time" = 60}** line, cron do nothing with your queue items.

If you would like to process all the queue items in your browser, you have to visit the following url:
**/process-all-queue-items**

It will collect all of your queue items, creates batch operations from them, and after that it process one after another.

