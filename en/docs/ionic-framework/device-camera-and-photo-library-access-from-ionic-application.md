---
title: "Device camera and photo library access from Ionic application"
slug: "device-camera-and-photo-library-access-from-ionic-application"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

also refer this [link][1]


  [1]: https://www.tutorialspoint.com/ionic/ionic_camera.htm

## Open camara and photo gallery
index.html

    <!DOCTYPE html>
    <html>
      <head>
        <meta charset="utf-8">
        <meta http-equiv="Content-Security-Policy" content="default-src *; script-src 'self' 'unsafe-inline' 'unsafe-eval' *; style-src  'self' 'unsafe-inline' *"/>
        <meta name="viewport" content="initial-scale=1, maximum-scale=1, user-scalable=no, width=device-width">
        <title></title>
    
        <link href="lib/ionic/css/ionic.css" rel="stylesheet">
        <link href="css/style.css" rel="stylesheet">
    
        <!-- IF using Sass (run gulp sass first), then uncomment below and remove the CSS includes above
        <link href="css/ionic.app.css" rel="stylesheet">
        -->
    
        <!-- ionic/angularjs js -->
        <script src="lib/ionic/js/ionic.bundle.js"></script>
    
        <!-- cordova script (this will be a 404 during development) -->
        <script src="js/ng-cordova.min.js"></script>
        <script src="cordova.js"></script>
    
        <!-- your app's js -->
        <script src="js/app.js"></script>
        <script src="js/controllers.js"></script>
        <script src="js/services.js"></script>
      </head>
      <body ng-app="starter">
      <ion-content class="has-header padding" ng-controller="ImageController">
        <button class="button button-full button-energized" ng-click="addMedia()">
          Add image
        </button>
        <button class="button button-full button-positive" ng-click="sendEmail()">
          Send mail
        </button>
        <br><br>
        <ion-scroll direction="y" style="height:200px; min-height: 200px; overflow: scroll; white-space: nowrap;">
          <img ng-repeat="image in images track by $index" ng-src="data:image/jpeg;base64,{{image}}" style="height:200px; padding: 5px 5px 5px 5px;"/>
        </ion-scroll>
    
      </ion-content>
    </body>
    </html>

controller.js

    angular.module('starter')
     
    .controller('ImageController', function($scope, $cordovaDevice, $cordovaFile, $ionicPlatform, $cordovaEmailComposer, $ionicActionSheet, ImageService, FileService) {
     
      $ionicPlatform.ready(function() {
        $scope.images = FileService.images();
        $scope.$apply();
      });
     
      $scope.addMedia = function() {
        $scope.hideSheet = $ionicActionSheet.show({
          buttons: [
            { text: 'Take photo' },
            { text: 'Photo from library' }, 
              {  text: '<b ng-disabled="user.length<1">Delete</b>',
                  type: 'input type="file"'}
          ],
          titleText: 'Add images',
          cancelText: 'Cancel',
          buttonClicked: function(index) {
            $scope.addImage(index);
          }
        });
      }
     
      $scope.addImage = function(type) {
        $scope.hideSheet();
        ImageService.handleMediaDialog(type).then(function() {
          $scope.$apply();
        });
      }
      
      $scope.sendEmail = function() {
        if ($scope.images != null && $scope.images.length > 0) {
          var mailImages = [];
          var savedImages = $scope.images;
          for (var i = 0; i < savedImages.length; i++) {
              mailImages.push('base64:attachment'+i+'.jpg//' + savedImages[i]);
            }
          $scope.openMailComposer(mailImages);
        }
      }
     
      $scope.openMailComposer = function(attachments) {
        var bodyText = '<html><h2>My Images</h2></html>';
        var email = {
            to: '',
            attachments: attachments,
            subject: 'Devdactic Images',
            body: bodyText,
            isHtml: true
          };
     
        $cordovaEmailComposer.open(email, function(){
            console.log('email view dismissed');                                                      
        }, this);
      }
    });
service.js

    angular.module('starter')
     
    .factory('FileService', function() {
      var images;
      var IMAGE_STORAGE_KEY = 'dav-images';
     
      function getImages() {
        var img = window.localStorage.getItem(IMAGE_STORAGE_KEY);
        if (img) {
          images = JSON.parse(img);
        } else {
          images = [];
        }
        return images;
      };
     
      function addImage(img) {
        images.push(img);
        window.localStorage.setItem(IMAGE_STORAGE_KEY, JSON.stringify(images));
      };
     
      return {
        storeImage: addImage,
        images: getImages
      }
    })
    
    .factory('ImageService', function($cordovaCamera, FileService, $q, $cordovaFile) {
        
      function optionsForType(type) {
        var source;
        switch (type) {
          case 0:
            source = Camera.PictureSourceType.CAMERA;
            break;
          case 1:
            source = Camera.PictureSourceType.PHOTOLIBRARY;
            break;
        }
        return {
          quality: 90,
          destinationType: Camera.DestinationType.DATA_URL,
          sourceType: source,
          allowEdit: false,
          encodingType: Camera.EncodingType.JPEG,
          popoverOptions: CameraPopoverOptions,
          saveToPhotoAlbum: false,
          correctOrientation:true
        };
      }
     
      function saveMedia(type) {
        return $q(function(resolve, reject) {
          var options = optionsForType(type);
     
          $cordovaCamera.getPicture(options).then(function(imageBase64) {        
                FileService.storeImage(imageBase64);      
          });
        })
      }
      return {
        handleMediaDialog: saveMedia
      }
    });

## For Ionic 3 Example
Install the Cordova and Ionic Native plugins:

    $ ionic cordova plugin add cordova-plugin-camera
    $ npm install --save @ionic-native/camera

Your `app.module.ts` will need to inject camera:

    import { Camera } from '@ionic-native/camera';
    ..........
    @NgModule({
      declarations: [
        MyApp
      ],
      imports: [
        BrowserModule,
        IonicModule.forRoot(MyApp),
        ...........
      ],
      bootstrap: [IonicApp],
      entryComponents: [
        MyApp
      ],
      providers: [
        StatusBar,
        SplashScreen,
        Camera,
        {provide: ErrorHandler, useClass: IonicErrorHandler},
        ..........
      ]
    })
    export class AppModule {}

Camera can be used with Action sheet easily with Ionic 3, Your page.ts will be like below:

    import { Camera, CameraOptions } from '@ionic-native/camera';
    .........
    
    export class YourPage {
    
        private base64:any;
    
    constructor(private camera: Camera,private actionsheetCtrl: ActionSheetController) { }
    
    
    cameragalleryfun(){
    
        let actionSheet = this.actionsheetCtrl.create({
          title: 'Camera-Gallery',
          cssClass: 'action-sheets-basic-page',
          buttons: [
            {
              text: 'Camera',
              icon: 'camera',
              handler: () => {
                //console.log('Camera');
                const options: CameraOptions = {
                        quality: 60,
                        destinationType: this.camera.DestinationType.DATA_URL,
                        encodingType: this.camera.EncodingType.JPEG,
                        mediaType: this.camera.MediaType.PICTURE,
                        sourceType : this.camera.PictureSourceType.CAMERA,
                        targetWidth: 500,
                        saveToPhotoAlbum: false,
                        correctOrientation:true
                      }
    
                         this.camera.getPicture(options).then((imageData) => {
                         this.base64 = 'data:image/jpeg;base64,' + imageData;
                        }, (err) => {
                         // Handle error
                        });
              }
            },
            {
              text: 'Gallery',
              icon: 'images',
              handler: () => {
                //console.log('Gallery');
                    const options: CameraOptions = {
                        quality: 60,
                        destinationType: this.camera.DestinationType.DATA_URL,
                        encodingType: this.camera.EncodingType.JPEG,
                        mediaType: this.camera.MediaType.PICTURE,
                        sourceType : this.camera.PictureSourceType.PHOTOLIBRARY,
                        targetWidth: 500,
                        saveToPhotoAlbum: false,
                        correctOrientation:true
                      }
    
                          this.camera.getPicture(options).then((imageData) => {
                          this.base64 = 'data:image/jpeg;base64,' + imageData;
                        }, (err) => {
                         // Handle error
                        });
              }
            },
            {
              text: 'Cancel',
              role: 'cancel', // will always sort to be on the bottom
              icon: 'close',
              handler: () => {
                //console.log('Cancel clicked');
              }
            }
          ]
        });
        actionSheet.present();
          
      }
    }

Call `cameragalleryfun` function from any event like click on button, this will return base64 string for image. More option can be applied. for extra option see:https://github.com/apache/cordova-plugin-camera

