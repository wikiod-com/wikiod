---
title: "Training a Caffe model with pycaffe"
slug: "training-a-caffe-model-with-pycaffe"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Training a network on the Iris dataset
Given below is a simple example to train a Caffe model on the Iris data set in Python, using PyCaffe. It also gives the predicted outputs given some user-defined inputs. 

`iris_tuto.py`

    import subprocess
    import platform
    import copy
    
    from sklearn.datasets import load_iris
    import sklearn.metrics 
    import numpy as np
    from sklearn.cross_validation import StratifiedShuffleSplit
    import matplotlib.pyplot as plt
    import h5py
    import caffe
    import caffe.draw
    
    
    def load_data():
        '''
        Load Iris Data set
        '''
        data = load_iris()
        print(data.data)
        print(data.target)
        targets = np.zeros((len(data.target), 3))
        for count, target in enumerate(data.target):
            targets[count][target]= 1    
        print(targets)
    
        new_data = {}
        #new_data['input'] = data.data
        new_data['input'] = np.reshape(data.data, (150,1,1,4))
        new_data['output'] = targets
        #print(new_data['input'].shape)
        #new_data['input'] = np.random.random((150, 1, 1, 4))
        #print(new_data['input'].shape)   
        #new_data['output'] = np.random.random_integers(0, 1, size=(150,3))    
        #print(new_data['input'])
    
        return new_data
    
    def save_data_as_hdf5(hdf5_data_filename, data):
        '''
        HDF5 is one of the data formats Caffe accepts
        '''
        with h5py.File(hdf5_data_filename, 'w') as f:
            f['data'] = data['input'].astype(np.float32)
            f['label'] = data['output'].astype(np.float32)
    
    
    def train(solver_prototxt_filename):
        '''
        Train the ANN
        '''
        caffe.set_mode_cpu()
        solver = caffe.get_solver(solver_prototxt_filename)
        solver.solve()
    
    
    def print_network_parameters(net):
        '''
        Print the parameters of the network
        '''
        print(net)
        print('net.inputs: {0}'.format(net.inputs))
        print('net.outputs: {0}'.format(net.outputs))
        print('net.blobs: {0}'.format(net.blobs))
        print('net.params: {0}'.format(net.params))    
    
    def get_predicted_output(deploy_prototxt_filename, caffemodel_filename, input, net = None):
        '''
        Get the predicted output, i.e. perform a forward pass
        '''
        if net is None:
            net = caffe.Net(deploy_prototxt_filename,caffemodel_filename, caffe.TEST)
    
        #input = np.array([[ 5.1,  3.5,  1.4,  0.2]])
        #input = np.random.random((1, 1, 1))
        #print(input)
        #print(input.shape)
        out = net.forward(data=input)
        #print('out: {0}'.format(out))
        return out[net.outputs[0]]
    
    
    import google.protobuf 
    def print_network(prototxt_filename, caffemodel_filename):
        '''
        Draw the ANN architecture
        '''
        _net = caffe.proto.caffe_pb2.NetParameter()
        f = open(prototxt_filename)
        google.protobuf.text_format.Merge(f.read(), _net)
        caffe.draw.draw_net_to_file(_net, prototxt_filename + '.png' )
        print('Draw ANN done!')
    
    
    def print_network_weights(prototxt_filename, caffemodel_filename):
        '''
        For each ANN layer, print weight heatmap and weight histogram 
        '''
        net = caffe.Net(prototxt_filename,caffemodel_filename, caffe.TEST)
        for layer_name in net.params: 
            # weights heatmap 
            arr = net.params[layer_name][0].data
            plt.clf()
            fig = plt.figure(figsize=(10,10))
            ax = fig.add_subplot(111)
            cax = ax.matshow(arr, interpolation='none')
            fig.colorbar(cax, orientation="horizontal")
            plt.savefig('{0}_weights_{1}.png'.format(caffemodel_filename, layer_name), dpi=100, format='png', bbox_inches='tight') # use format='svg' or 'pdf' for vectorial pictures
            plt.close()
    
            # weights histogram  
            plt.clf()
            plt.hist(arr.tolist(), bins=20)
            plt.savefig('{0}_weights_hist_{1}.png'.format(caffemodel_filename, layer_name), dpi=100, format='png', bbox_inches='tight') # use format='svg' or 'pdf' for vectorial pictures
            plt.close()
    
    
    def get_predicted_outputs(deploy_prototxt_filename, caffemodel_filename, inputs):
        '''
        Get several predicted outputs
        '''
        outputs = []
        net = caffe.Net(deploy_prototxt_filename,caffemodel_filename, caffe.TEST)
        for input in inputs:
            #print(input)
            outputs.append(copy.deepcopy(get_predicted_output(deploy_prototxt_filename, caffemodel_filename, input, net)))
        return outputs    
    
    
    def get_accuracy(true_outputs, predicted_outputs):
        '''
    
        '''
        number_of_samples = true_outputs.shape[0]
        number_of_outputs = true_outputs.shape[1]
        threshold = 0.0 # 0 if SigmoidCrossEntropyLoss ; 0.5 if EuclideanLoss
        for output_number in range(number_of_outputs):
            predicted_output_binary = []
            for sample_number in range(number_of_samples):
                #print(predicted_outputs)
                #print(predicted_outputs[sample_number][output_number])            
                if predicted_outputs[sample_number][0][output_number] < threshold:
                    predicted_output = 0
                else:
                    predicted_output = 1
                predicted_output_binary.append(predicted_output)
    
            print('accuracy: {0}'.format(sklearn.metrics.accuracy_score(true_outputs[:, output_number], predicted_output_binary)))
            print(sklearn.metrics.confusion_matrix(true_outputs[:, output_number], predicted_output_binary))
    
    
    def main():
        '''
        This is the main function
        '''
    
        # Set parameters
        solver_prototxt_filename = 'iris_solver.prototxt'
        train_test_prototxt_filename = 'iris_train_test.prototxt'
        deploy_prototxt_filename  = 'iris_deploy.prototxt'
        deploy_prototxt_filename  = 'iris_deploy.prototxt'
        deploy_prototxt_batch2_filename  = 'iris_deploy_batchsize2.prototxt'
        hdf5_train_data_filename = 'iris_train_data.hdf5' 
        hdf5_test_data_filename = 'iris_test_data.hdf5' 
        caffemodel_filename = 'iris__iter_5000.caffemodel' # generated by train()
    
        # Prepare data
        data = load_data()
        print(data)
        train_data = data
        test_data = data
        save_data_as_hdf5(hdf5_train_data_filename, data)
        save_data_as_hdf5(hdf5_test_data_filename, data)
    
        # Train network
        train(solver_prototxt_filename)
    
        # Print network
        print_network(deploy_prototxt_filename, caffemodel_filename)
        print_network(train_test_prototxt_filename, caffemodel_filename)
        print_network_weights(train_test_prototxt_filename, caffemodel_filename)
    
        # Compute performance metrics
        #inputs = input = np.array([[[[ 5.1,  3.5,  1.4,  0.2]]],[[[ 5.9,  3. ,  5.1,  1.8]]]])
        inputs = data['input']
        outputs = get_predicted_outputs(deploy_prototxt_filename, caffemodel_filename, inputs)
        get_accuracy(data['output'], outputs)
    
    
    if __name__ == "__main__":
        main()


It requires the two following `iris_train_test.prototxt` and `iris_deploy.prototxt` to be in the same folder.


`iris_train_test.prototxt`:

    name: "IrisNet"
    layer {
      name: "iris"
      type: "HDF5Data"
      top: "data"
      top: "label"
      include {
        phase: TRAIN
      }
      hdf5_data_param {
        source: "iris_train_data.txt"
        batch_size: 1
    
      }
    }
    
    layer {
      name: "iris"
      type: "HDF5Data"
      top: "data"
      top: "label"
      include {
        phase: TEST
      }
      hdf5_data_param {
        source: "iris_test_data.txt"
        batch_size: 1
    
      }
    }
    
    
    
    
    layer {
      name: "ip1"
      type: "InnerProduct"
      bottom: "data"
      top: "ip1"
      param {
        lr_mult: 1  # the learning rate multiplier for weights
      }
      param {
        lr_mult: 2  # the learning rate multiplier for biases
      }
      inner_product_param {
        num_output: 50
        weight_filler {
          type: "xavier"
        }
        bias_filler {
          type: "constant"
        }
      }
    }
    layer {
      name: "relu1"
      type: "ReLU"
      bottom: "ip1"
      top: "ip1"
    }
    layer {
      name: "drop1"
      type: "Dropout"
      bottom: "ip1"
      top: "ip1"
      dropout_param {
        dropout_ratio: 0.5
      }
    }
    
    
    layer {
      name: "ip2"
      type: "InnerProduct"
      bottom: "ip1"
      top: "ip2"
      param {
        lr_mult: 1
      }
      param {
        lr_mult: 2
      }
      inner_product_param {
        num_output: 50
        weight_filler {
          type: "xavier"
        }
        bias_filler {
          type: "constant"
        }
      }
    }
    layer {
      name: "drop2"
      type: "Dropout"
      bottom: "ip2"
      top: "ip2"
      dropout_param {
        dropout_ratio: 0.4
      }
    }
    
    
    
    layer {
      name: "ip3"
      type: "InnerProduct"
      bottom: "ip2"
      top: "ip3"
      param {
        lr_mult: 1
      }
      param {
        lr_mult: 2
      }
      inner_product_param {
        num_output: 3
        weight_filler {
          type: "xavier"
        }
        bias_filler {
          type: "constant"
        }
      }
    }
    
    layer {
      name: "drop3"
      type: "Dropout"
      bottom: "ip3"
      top: "ip3"
      dropout_param {
        dropout_ratio: 0.3
      }
    }
    
    layer {
      name: "loss"
      type: "SigmoidCrossEntropyLoss" 
      # type: "EuclideanLoss" 
      # type: "HingeLoss"  
      bottom: "ip3"
      bottom: "label"
      top: "loss"
    }

`iris_deploy.prototxt`:

    name: "IrisNet"
    input: "data"
    input_dim: 1 # batch size
    input_dim: 1
    input_dim: 1
    input_dim: 4
    
    
    layer {
      name: "ip1"
      type: "InnerProduct"
      bottom: "data"
      top: "ip1"
      param {
        lr_mult: 1
      }
      param {
        lr_mult: 2
      }
      inner_product_param {
        num_output: 50
        weight_filler {
          type: "xavier"
        }
        bias_filler {
          type: "constant"
        }
      }
    }
    layer {
      name: "relu1"
      type: "ReLU"
      bottom: "ip1"
      top: "ip1"
    }
    layer {
      name: "drop1"
      type: "Dropout"
      bottom: "ip1"
      top: "ip1"
      dropout_param {
        dropout_ratio: 0.5
      }
    }
    
    
    layer {
      name: "ip2"
      type: "InnerProduct"
      bottom: "ip1"
      top: "ip2"
      param {
        lr_mult: 1
      }
      param {
        lr_mult: 2
      }
      inner_product_param {
        num_output: 50
        weight_filler {
          type: "xavier"
        }
        bias_filler {
          type: "constant"
        }
      }
    }
    layer {
      name: "drop2"
      type: "Dropout"
      bottom: "ip2"
      top: "ip2"
      dropout_param {
        dropout_ratio: 0.4
      }
    }
    
    
    layer {
      name: "ip3"
      type: "InnerProduct"
      bottom: "ip2"
      top: "ip3"
      param {
        lr_mult: 1
      }
      param {
        lr_mult: 2
      }
      inner_product_param {
        num_output: 3
        weight_filler {
          type: "xavier"
        }
        bias_filler {
          type: "constant"
        }
      }
    }
    
    layer {
      name: "drop3"
      type: "Dropout"
      bottom: "ip3"
      top: "ip3"
      dropout_param {
        dropout_ratio: 0.3
      }
    }


`iris_solver.prototxt`:

    # The train/test net protocol buffer definition
    net: "iris_train_test.prototxt"
    # test_iter specifies how many forward passes the test should carry out.
    test_iter: 1
    # Carry out testing every test_interval training iterations.
    test_interval: 1000
    # The base learning rate, momentum and the weight decay of the network.
    base_lr: 0.0001
    momentum: 0.001
    weight_decay: 0.0005
    # The learning rate policy
    lr_policy: "inv"
    gamma: 0.0001
    power: 0.75
    # Display every 100 iterations
    display: 1000
    # The maximum number of iterations
    max_iter: 5000
    # snapshot intermediate results
    snapshot: 5000
    snapshot_prefix: "iris_"
    # solver mode: CPU or GPU
    solver_mode: CPU # GPU





