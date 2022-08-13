---
title: "Transfer Learning and Fine Tuning using Keras"
slug: "transfer-learning-and-fine-tuning-using-keras"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

This topic includes short, brief but comprehensive examples of loading pre-trained weights, inserting new layers on top or in the middle of pre-tained ones, and training a new network with partly pre-trained weights. An example for each of out-of-the-box pre-trained networks, available in *Keras* library (VGG, ResNet, Inception, Xception, MobileNet), is required.

## Transfer Learning using Keras and VGG
In this example, three brief and comprehensive sub-examples are presented:

 - Loading weights from available pre-trained models, included with *Keras* library
 - Stacking another network for training on top of any layers of VGG
 - Inserting a layer in the middle of other layers
 - Tips and general rule-of-thumbs for Fine-Tuning and transfer learning with VGG

----------------------


# Loading pre-trained weights

Pre-trained on *ImageNet* models, including *VGG-16* and *VGG-19*, are available in     *Keras*. Here and after in this example, *VGG-16* will be used. For more information, please visit [*Keras Applications documentation*][1].

<!-- language: lang-python -->

    from keras import applications

    # This will load the whole VGG16 network, including the top Dense layers.
    # Note: by specifying the shape of top layers, input tensor shape is forced
    # to be (224, 224, 3), therefore you can use it only on 224x224 images.
    vgg_model = applications.VGG16(weights='imagenet', include_top=True)

    # If you are only interested in convolution filters. Note that by not
    # specifying the shape of top layers, the input tensor shape is (None, None, 3),
    # so you can use them for any size of images.
    vgg_model = applications.VGG16(weights='imagenet', include_top=False)

    # If you want to specify input tensor
    from keras.layers import Input
    input_tensor = Input(shape=(160, 160, 3))
    vgg_model = applications.VGG16(weights='imagenet',
                                   include_top=False,
                                   input_tensor=input_tensor)

    # To see the models' architecture and layer names, run the following
    vgg_model.summary()
--------------------------------

# Create a new network with bottom layers taken from VGG

Assume that for some specific task for images with the size `(160, 160, 3)`, you want to use pre-trained bottom layers of VGG, up to layer with the name `block2_pool`.

<!-- language: lang-python -->

    vgg_model = applications.VGG16(weights='imagenet',
                                   include_top=False,
                                   input_shape=(160, 160, 3))

    # Creating dictionary that maps layer names to the layers
    layer_dict = dict([(layer.name, layer) for layer in vgg_model.layers])
    
    # Getting output tensor of the last VGG layer that we want to include
    x = layer_dict['block2_pool'].output

    # Stacking a new simple convolutional network on top of it    
    x = Conv2D(filters=64, kernel_size=(3, 3), activation='relu')(x)
    x = MaxPooling2D(pool_size=(2, 2))(x)
    x = Flatten()(x)
    x = Dense(256, activation='relu')(x)
    x = Dropout(0.5)(x)
    x = Dense(10, activation='softmax')(x)

    # Creating new model. Please note that this is NOT a Sequential() model.
    from keras.models import Model
    custom_model = Model(input=vgg_model.input, output=x)

    # Make sure that the pre-trained bottom layers are not trainable
    for layer in custom_model.layers[:7]:
        layer.trainable = False

    # Do not forget to compile it
    custom_model.compile(loss='categorical_crossentropy',
                         optimizer='rmsprop',
                         metrics=['accuracy'])

--------------------------------------

# Remove multiple layers and insert a new one in the middle

Assume that you need to speed up VGG16 by replacing `block1_conv1` and `block2_conv2` with a single convolutional layer, in such a way that the pre-trained weights are saved.
The idea is to disassemble the whole network to separate layers, then assemble it back. Here is the code specifically for your task:
    
<!-- language: lang-python -->

    vgg_model = applications.VGG16(include_top=True, weights='imagenet')

    # Disassemble layers
    layers = [l for l in vgg_model.layers]

    # Defining new convolutional layer.
    # Important: the number of filters should be the same!
    # Note: the receiptive field of two 3x3 convolutions is 5x5.
    new_conv = Conv2D(filters=64, 
                      kernel_size=(5, 5),
                      name='new_conv',
                      padding='same')(layers[0].output)

    # Now stack everything back
    # Note: If you are going to fine tune the model, do not forget to
    #       mark other layers as un-trainable
    x = new_conv
    for i in range(3, len(layers)):
        layers[i].trainable = False
        x = layers[i](x)

    # Final touch
    result_model = Model(input=layer[0].input, output=x)




  [1]: https://keras.io/applications/

