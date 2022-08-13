---
title: "Getting Started with QnA Services"
slug: "getting-started-with-qna-services"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

The **QnA Maker** is a free, easy-to-use, REST API- and web-based service that trains AI to respond to users’ questions in a more natural, conversational way. With optimized machine learning logic and the ability to integrate industry-leading language processing, QnA Maker distills semi-structured data like question and answer pairs into distinct, helpful answers.

## Creating our own QnA Service manually
Providing your microsoft account credentials you can authenticate and receive subscription keys to start with the services. This [document](https://qnamaker.ai/Documentation/Authentication) describes the various flows in the tool to create your own knowledge base. 

[![enter image description here][1]][1]

QnA Maker works in three steps: extraction, training and publishing. To start, feed it anything from existing FAQ URLs to documents and editorial content. I created my own question and answers manually.

[![enter image description here][2]][2]

QnA Maker extracts all possible pairs of questions and answers, and through the easy-to-use web interface you can edit, remove or add any pairs, as well as test and train the knowledge base. The relevance of the responses is the most important part of your QnA service. 

The train feature lets you evaluate the correctness of the responses and correct them and re-train the knowledge base.

[![enter image description here][3]][3]

There are two ways you can improve the relevance of the responses.

a. Chat with your KB:

Chat with your knowledge base, to see the relevance of the responses. You can add a variation to an existing question as well as choose a different answer for a question. Make sure you press Save and retrain, to reflect any changes/inputs you have provided.

b. Replay live chat logs:

A very useful feature is to see what responses the service returns for live traffic, and then train it appropriately. You can download the live chat traffic hitting your published end-point by clicking on Download chat logs. This downloads all the questions hitting your end-point in descending order of frequency. Looking at the chat logs, you can decide which questions you want to test and train your knowledge base on, as described in the above section.

[![enter image description here][4]][4]

Once you’re satisfied with the scope of responses, you can publish your knowledge base as an API endpoint.

We can review the changes made to the QnA Bot Service and click on "Publish" button.

[![enter image description here][5]][5]

Our QnA Bot Service will be deployed successfully. It will show the sample HTTP request with knowledge base id and subscription key. By using the HTTP request, we can build our own UI for this QnA Bot Service or consume it directly with [Azure Bot Service](https://azure.microsoft.com/en-us/services/bot-service/)

[![enter image description here][6]][6]

Even after publishing, you can review interactions in real time and refine responses as needed. QnA Maker integrates with other APIs and solutions seamlessly and at scale.

Through settings tab you can update the changes requires and make you save and train it every time.

By using other Cognitive Services with QnA Maker, you can create something as elegantly simple as a chat bot that answers FAQs, or as complex as an interactive virtual guide.

[![enter image description here][7]][7]

If you have feedback or questions about the service, share your comments by going [here](https://qnamaker.ai/) and clicking on “Feedback” in the top navigation.


  [1]: https://i.stack.imgur.com/o0SVc.png
  [2]: https://i.stack.imgur.com/IcuWI.png
  [3]: https://i.stack.imgur.com/KlZvU.png
  [4]: https://i.stack.imgur.com/lPold.png
  [5]: https://i.stack.imgur.com/3RCx2.png
  [6]: https://i.stack.imgur.com/TQl1F.png
  [7]: https://i.stack.imgur.com/4OmAO.png

