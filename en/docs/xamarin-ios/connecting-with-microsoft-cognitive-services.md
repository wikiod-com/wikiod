---
title: "Connecting with Microsoft Cognitive Services"
slug: "connecting-with-microsoft-cognitive-services"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

In this example we used Microsoft.ProjectOxford.Vision NuGet package:
https://www.nuget.org/packages/Microsoft.ProjectOxford.Vision/

To read more about Microsoft Cognitive Services please refer to the official documentation:
https://www.microsoft.com/cognitive-services/en-us/computer-vision-api

Please find uploaded sample on my GitHub:
https://github.com/Daniel-Krzyczkowski/XamarinIOS/tree/master/XamariniOS_CognitiveServices

I also attach link to my blog where I presented how to use Cognitive Services with Xamarin Forms application:
http://mobileprogrammer.pl

## Connecting with Microsoft Cognitive Services
In this example you will learn how to use Microsoft Cognitive Services with Xamarin iOS mobile application. We will use Computer Vision API to detect what is in the picture.

Once you create Xamarin.iOS project please add below NuGet package to the project:

https://www.nuget.org/packages/Microsoft.ProjectOxford.Vision/

With this library we will be able to utilize Cognitive Services in our iOS app.
I assume that you have Microsoft account already registered to use it and you have enabled Computer Vision Api like on the screen below:
[![enter image description here][1]][1]

Once you click "Subscribe" at the bottom Api Key will be generated:

[![enter image description here][2]][2]

Now we can start configuring access to Cognitive Services from the iOS app. Firstly we need to get some picture for the analysis. To do it we can use Xamarin Media Component available below:
https://components.xamarin.com/view/mediaplugin

Once it is successfully installed let's create simple UI with the image and button to select picture from the gallery. Size of the controls is up to you.

Open Main.storyboard and add UIImageView and UIButton controls do default ViewController. Add them names: "SelectedPictureImageView" and "SelectButton":

[![enter image description here][3]][3]

Now we should add "Touch Up Inside" event handler to handle image selection:

<!-- language-all: c# -->

    partial void SelectButtonClick(UIButton sender)
    {
        selectImage();
    }

    async void selectImage()
    {
        var selectedImage = await CrossMedia.Current.PickPhotoAsync();
        SelectedPictureImageView.Image =  new UIImage(NSData.FromStream(selectedImage.GetStream()));
    }

Now we would like to display analysis information once Cognitive Services returns the information. Add label below the button called "AnalysisLabel":
[![enter image description here][4]][4]

It is time to connect Computer Vision API! 

To get information about selected picture add below method. Remember to paste you API Key!

    async Task analyseImage(Stream imageStream)
    {
        try
        {
            VisionServiceClient visionClient = new VisionServiceClient("<<YOUR API KEY HERE>>");
            VisualFeature[] features = { VisualFeature.Tags, VisualFeature.Categories, VisualFeature.Description };
            var analysisResult = await visionClient.AnalyzeImageAsync(imageStream, features.ToList(), null);
            AnalysisLabel.Text = string.Empty;
            analysisResult.Description.Tags.ToList().ForEach(tag => AnalysisLabel.Text = AnalysisLabel.Text + tag + "\n");
        }
        catch (Microsoft.ProjectOxford.Vision.ClientException ex)
        {
            AnalysisLabel.Text = ex.Error.Message;
        }
    }

Now you can invoke it in "selectImage" method:

    async void selectImage()
    {
        var selectedImage = await CrossMedia.Current.PickPhotoAsync();
        SelectedPictureImageView.Image =  new UIImage(NSData.FromStream(selectedImage.GetStream()));
        await analyseImage(selectedImage.GetStream());
    }

Once you select image, Microsoft Cognitive Services will analyze it and return the result:

[![enter image description here][5]][5]

Remember that you image cannot be too large - in this case you will receive information like below:

[![enter image description here][6]][6]

There are many other services which you can try to use. Please refer to the official documentation (link attached) to discover more.


  [1]: http://i.stack.imgur.com/UBcMp.png
  [2]: http://i.stack.imgur.com/h9UV6.png
  [3]: http://i.stack.imgur.com/Qb2c5.png
  [4]: http://i.stack.imgur.com/8nCjz.png
  [5]: http://i.stack.imgur.com/c1RHw.png
  [6]: http://i.stack.imgur.com/7ReZi.png

