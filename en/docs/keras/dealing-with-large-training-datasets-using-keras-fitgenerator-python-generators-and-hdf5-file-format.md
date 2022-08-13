---
title: "Dealing with large training datasets using Keras fit_generator, Python generators, and HDF5 file format"
slug: "dealing-with-large-training-datasets-using-keras-fit_generator-python-generators-and-hdf5-file-format"
draft: false
images: []
weight: 9494
type: docs
toc: true
---

Machine learning problems often require dealing with large quantities of training data with limited computing resources, particularly memory. It is not always possible to load an entire training set into memory. Fortunately, this can be dealt with through the use of Keras' fit_generator method, Python generators, and HDF5 file format.

This example assumes keras, numpy (as np), and h5py have already been installed and imported. It also assumes that video inputs and labels have already been processed and saved to the specified HDF5 file, in the format mentioned, and a video classification model has already been built to work with the given input.

## Training a model to classify videos
For this example, let **model** be a Keras model for classifying video inputs, let **X** be a large data set of video inputs, with a shape of *(samples, frames, channels, rows, columns)*, and let **Y** be the corresponding data set of one-hot encoded labels, with a shape of *(samples, classes)*. Both datasets are stored within an HDF5 file called **video_data.h5**. The HDF5 file also has the attribute **sample_count** for the number of samples.

*Here is the function for training the model with fit_generator*

    def train_model(model, video_data_fn="video_data.h5", validation_ratio=0.3, batch_size=32):
        """ Train the video classification model
        """
        with h5py.File(video_data_fn, "r") as video_data:
             sample_count = int(video_data.attrs["sample_count"])
             sample_idxs = range(0, sample_count)
             sample_idxs = np.random.permutation(sample_idxs)
             training_sample_idxs = sample_idxs[0:int((1-validation_ratio)*sample_count)]
             validation_sample_idxs = sample_idxs[int((1-validation_ratio)*sample_count):]
             training_sequence_generator = generate_training_sequences(batch_size=batch_size,
                                                                       video_data=video_data,
                                                                       training_sample_idxs=training_sample_idxs)
             validation_sequence_generator = generate_validation_sequences(batch_size=batch_size,
                                                                           video_data=video_data,
                                                                           validation_sample_idxs=validation_sample_idxs)
             model.fit_generator(generator=training_sequence_generator,
                                 validation_data=validation_sequence_generator,
                                 samples_per_epoch=len(training_sample_idxs),
                                 nb_val_samples=len(validation_sample_idxs),
                                 nb_epoch=100,
                                 max_q_size=1,
                                 verbose=2,
                                 class_weight=None,
                                 nb_worker=1)

*Here are the training and validation sequence generators*

    def generate_training_sequences(batch_size, video_data, training_sample_idxs):
        """ Generates training sequences on demand
        """
        while True:
            # generate sequences for training
            training_sample_count = len(training_sample_idxs)
            batches = int(training_sample_count/batch_size)
            remainder_samples = training_sample_count%batch_size
            if remainder_samples:
                batches = batches + 1
            # generate batches of samples
            for idx in xrange(0, batches):
                if idx == batches - 1:
                    batch_idxs = training_sample_idxs[idx*batch_size:]
                else:
                    batch_idxs = training_sample_idxs[idx*batch_size:idx*batch_size+batch_size]
                batch_idxs = sorted(batch_idxs)
    
                X = video_data["X"][batch_idxs]
                Y = video_data["Y"][batch_idxs]
    
                yield (np.array(X), np.array(Y))
    
    def generate_validation_sequences(batch_size, video_data, validation_sample_idxs):
        """ Generates validation sequences on demand
        """
        while True:
            # generate sequences for validation
            validation_sample_count = len(validation_sample_idxs)
            batches = int(validation_sample_count/batch_size)
            remainder_samples = validation_sample_count%batch_size
            if remainder_samples:
                batches = batches + 1
            # generate batches of samples
            for idx in xrange(0, batches):
                if idx == batches - 1:
                    batch_idxs = validation_sample_idxs[idx*batch_size:]
                else:
                    batch_idxs = validation_sample_idxs[idx*batch_size:idx*batch_size+batch_size]
                batch_idxs = sorted(batch_idxs)
    
                X = video_data["X"][batch_idxs]
                Y = video_data["Y"][batch_idxs]
    
                yield (np.array(X), np.array(Y))

