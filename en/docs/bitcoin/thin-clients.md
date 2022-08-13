---
title: "Thin Clients"
slug: "thin-clients"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

This client downloads a complete copy of the headers for all blocks in the entire block chain. This means that the download and storage requirements scale linearly with the amount of time since Bitcoin was invented.

## Request a merkle block with bitcore-p2p
In this example we will ask the bitcoin network for the merkle block number [442603][1].

In order to do this we need to send a [filterload message][2] and then we have to send a [getdata message][3] using the inventory type MSG_MERKLEBLOCK.

The peers should reply with a [merkleblock message][4] for the requested block and a [tx message][5] for any transactions in the requested block that matched the filter.

In [bitcore-p2p][6] we need to register an event for each message's type that we want recive.

    let Pool = require('bitcore-p2p').Pool;
    let BloomFilter = require('bitcore-p2p').BloomFilter;
    let NetworksData = require('bitcore-lib').Networks;
    let Messages = require('bitcore-p2p').Messages;
    
    let network = 'livenet';  // Network can be livenet or testnet
    let txs = []; // Here we store the transactions
    let filteredBlocks = []; // Here we store the merkleblocks
    
    // Date that we are loocking for
    let data = {
      code: '88adcf0215d5fcbca5c6532aaecffb48128cf1a6',  // 1DTh7XPb42PgCFnuMHSitMPWxCfNNFej8n in hex fromat
      format: 'hex',
    };
    
    // Isatnciate and connect a node Pool
    let pool = new Pool({network: NetworksData[network]});
    pool.connect();
    
    // Create a filter and a bitcoin message with the filter
    let filter = BloomFilter.create(1000, 0.1).insert(new Buffer(data.code, data.format));
    let filterLoad = new Messages({network: NetworksData[network]}).FilterLoad(filter);
    
    // Create a bitcoin message for require a merkleblock
    let blockHashRequested = '0000000000000000004f8325a66388e22c10e6de9f0f6e5809eaf1e0393efe02';
    let getDataForFilteredBlock = new Messages({network: NetworksData[network]}).GetData.forFilteredBlock(blockHashRequested);
    
    // Transactions and merkleblock are sent in different messages
    pool.on('peertx', function(peer, message) {
      txs.push({
        peer: peer,
        message: message,
      });
    
      console.log('Recived from: ', peer.host);
      console.log('The transaction: ', message.transaction.hash);
    });
    
    pool.on('peermerkleblock', function(peer, message) {
      filteredBlocks.push({
        peer: peer,
        message: message,
      });
    
      console.log('Recived from: ', peer.host);
      console.log('The merkleBlock: ', message.merkleBlock.header.hash);
    });
    
    // Wait for pool to connect
    setTimeout(function(){
      pool.sendMessage(filterLoad);
      pool.sendMessage(getDataForFilteredBlock);
    }, 5000);
    
    //Recived from:  138.68.111.63
    //The merkleBlock:  0000000000000000004f8325a66388e22c10e6de9f0f6e5809eaf1e0393efe02
    //Recived from:  138.68.111.63
    //The transaction:  9aec6cc42ddcf5900d280f3fa598f5cdb101f00614785165f777a6856314f4d9
    //Recived from:  103.3.61.48
    //The merkleBlock:  0000000000000000004f8325a66388e22c10e6de9f0f6e5809eaf1e0393efe02
    //Recived from:  103.3.61.48
    //The transaction:  9aec6cc42ddcf5900d280f3fa598f5cdb101f00614785165f777a6856314f4d9


  [1]: https://blockexplorer.com/block/0000000000000000004f8325a66388e22c10e6de9f0f6e5809eaf1e0393efe02
  [2]: https://bitcoin.org/en/developer-reference#filterload
  [3]: https://bitcoin.org/en/developer-reference#getdata
  [4]: https://bitcoin.org/en/developer-reference#merkleblock
  [5]: https://bitcoin.org/en/developer-reference#tx
  [6]: https://bitcore.io/api/p2p

## Download an header chain with bitcore-p2p
**IMPORTANT** This is just an example code, do not use in production.

In order to download an header chain we have to send a [getHeaders message][1].

In this example we will require as much as possible headers after the 40000th one.The peer will respond with batch of 2000 headers so, we have to take the last header hash for be able to require the next 2000 headers.

Consider the fact that bitcore-p2p is an event based library the most robust option could be promisify the network interface in order to use async/await for download the header chain. Here for the sake of simplicity we will use a generator to face the asynchronous nature of the network.

    let Messages = require('bitcore-p2p').Messages;
    let Pool = require('bitcore-p2p').Pool;
    let NetworksData = require('bitcore-lib').Networks;
    
    let network = 'livenet';
    let headers = [];          // do not do that in production!
    let validHeaders = [];     // do not do that in production!
    let firsHeader = '000000000000000004ec466ce4732fe6f1ed1cddc2ed4b328fff5224276e3f6f';
    
    // Isatnciate and connect a node Pool
    let pool = new Pool({network: NetworksData[network]});
    pool.connect();
    
    // we have to reverese the hash becaouse is in the format xxxx0000 instead of 0000xxxx or vice versa
    function reverseHash(hash){
      return hash.match(/.{1,2}/g).reverse().join('');
    }
    
    // check if the response is the one associate with the last request becaouse could be a response associate to an old request
    function isValidResponse(firstHeaderRecived, headerHashRequested){
      // the header hash of the block before the first block header that we get on the response
      let headerHashBeforeFirstHeaderRecived = reverseHash(firstHeaderRecived.prevHash.toString('hex'));
      if (headerHashBeforeFirstHeaderRecived === headerHashRequested){
        return true;
      }
      else{
        return false;
      }
    }
    
    pool.on('peerheaders', function(peer, message) {
      let lastHeaderHashRequested;
      if (validHeaders[validHeaders.length -1]) {
        lastHeaderHashRequested = validHeaders[validHeaders.length -1].hash;
      }
      else {
        lastHeaderHashRequested = firsHeader;
      }
      if (isValidResponse(message.headers[0], lastHeaderHashRequested) && headers.length === 0) {
        headers.push({
          peer: peer,
          message: message,
        });
    
        console.log('Recived from: ', peer.host, message.headers.length, 'headers');
        console.log('The first block hash is', message.headers[0].hash);
        console.log('The last block hash is', message.headers[message.headers.length - 1].hash);
    
        syncronize.next();
        //console.log(syncronize)
      }
    });
    
    function* sync(lastHash) {
      let startHash = new Buffer(lastHash, 'hex');
      let message = new Messages({network: NetworksData[network]}).GetHeaders();
      // require as much as possible headers after startHash
      message.starts.push(startHash);
      pool.sendMessage(message);
      yield;
      validHeaders.push(...headers[0].message.headers);
      headers = [];
      let lastDownloadedHeader = validHeaders[validHeaders.length - 1];
      if (validHeaders.length % 2000 === 0) {
        yield * sync(reverseHash(lastDownloadedHeader.hash));
      }
    }
    
    syncronize = sync(reverseHash(firsHeader));
    
    // Wait for pool to connect
    setTimeout(function(){
      console.log(pool);
      syncronize.next();
    }, 5000);


  [1]: https://bitcoin.org/en/developer-reference#getheaders

