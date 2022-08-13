---
title: "Cloud Functions for Firebase"
slug: "cloud-functions-for-firebase"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

Firebase launched its beta release of Cloud Functions for Firebase which is similar to using of Cloud Functions on Google Cloud Platform.

Cloud Functions is a hosted, private, and scalable Node.js environment where you can run JavaScript code. Firebase SDK for Cloud Functions integrates the Firebase platform by letting you write code that responds to events and invokes functionality exposed by other Firebase features.

## Send welcome notification emails to the users for subscribing.
Use the GitHub repository to get the entire code:
https://github.com/firebase/functions-samples/blob/master/quickstarts/email-users

- Copy or clone the repository in your computer.

# Now go to your Firebase Console
- Create a Firebase Project using the Firebase Console.
- Enable the **Google** Provider in the **Auth** section.
- Paste the **Web initialization** snippet from: **Firebase Console > Overview > Add Firebase** to your web app in the **public/index.html** where the **TODO** is located.

    
     * TODO(DEVELOPER): Paste the initialization snippet from: Firebase Console > Overview > Add Firebase to your web app. *
     *********************************************************************************************************************** -->
     <script src="https://www.gstatic.com/firebasejs/3.7.3/firebase.js"></script>
     <script>
       // Initialize Firebase
       var config = {
         apiKey: "your apiKey",
         authDomain: "authDomain.firebaseapp.com",
         databaseURL: "https://databaseURL.firebaseio.com",
         storageBucket: "storageBucket.appspot.com",
         messagingSenderId: "messagingID"
       };
       firebase.initializeApp(config);
     </script>

# Install Firebase CLI in your computer
- If you don't have **NodeJS** installed already, install it from https://nodejs.org/en/ (Make sure to have the updated version of NodeJS  installed on your computer.) 
- Open command prompt/terminal and install it with **npm install -g firebase-tools** and then configure it with **firebase login**
- To choose your project you created now ==> Configure the CLI locally by using **firebase use --add** and select your project in the list.
- Install dependencies locally by running: **cd functions; npm install; cd -**

# Set Google Cloud environment variables

- Set the **gmail.email** and **gmail.password** Google Cloud environment variables to match the email and password of the Gmail account used to send emails. For this **open the command prompt or terminal** and type the following Firebase CLI command:

    firebase functions:config:set gmail.email="myusername@gmail.com" gmail.password="secretpassword"

# Deploy the project and test
- To deploy the project open the **cmd/terminal** and use the command **firebase deploy** to start the deployment.

[![successfull deployment snapshot][1]][1]
- Once it gets done, use the command to open the site in browser **firebase open hosting:site** or manually do it from the url displayed.


  [1]: https://i.stack.imgur.com/ChGX1.png

