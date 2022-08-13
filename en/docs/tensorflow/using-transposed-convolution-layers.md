---
title: "Using transposed convolution layers"
slug: "using-transposed-convolution-layers"
draft: false
images: []
weight: 9919
type: docs
toc: true
---

## Using tf.nn.conv2d_transpose for arbitary batch sizes and with automatic output shape calculation.
Example of how to calculate the output shape and overcome the difficulties of using tf.nn.conv2d_transpose with unknown batch size (when input.get_shape() is (?, H, W, C) or (?, C, H, W)).

    def upconvolution (input, output_channel_size, filter_size_h, filter_size_w,
                       stride_h, stride_w, init_w, init_b, layer_name, 
                       dtype=tf.float32, data_format="NHWC", padding='VALID'):
        with tf.variable_scope(layer_name):
          #calculation of the output_shape:
          if data_format == "NHWC":
            input_channel_size = input.get_shape().as_list()[3]
            input_size_h = input.get_shape().as_list()[1]
            input_size_w = input.get_shape().as_list()[2]
            stride_shape = [1, stride_h, stride_w, 1]
            if padding == 'VALID':
              output_size_h = (input_size_h - 1)*stride_h + filter_size_h
              output_size_w = (input_size_w - 1)*stride_w + filter_size_w
            elif padding == 'SAME':
              output_size_h = (input_size_h - 1)*stride_h + 1
              output_size_w = (input_size_w - 1)*stride_w + 1
            else:
              raise ValueError("unknown padding")
            output_shape = tf.stack([tf.shape(input)[0], 
                                    output_size_h, output_size_w, 
                                    output_channel_size])
          elif data_format == "NCHW":
            input_channel_size = input.get_shape().as_list()[1]
            input_size_h = input.get_shape().as_list()[2]
            input_size_w = input.get_shape().as_list()[3]
            stride_shape = [1, 1, stride_h, stride_w]
            if padding == 'VALID':
              output_size_h = (input_size_h - 1)*stride_h + filter_size_h
              output_size_w = (input_size_w - 1)*stride_w + filter_size_w
            elif padding == 'SAME':
              output_size_h = (input_size_h - 1)*stride_h + 1
              output_size_w = (input_size_w - 1)*stride_w + 1
            else:
              raise ValueError("unknown padding")
            output_shape = tf.stack([tf.shape(input)[0], 
                                    output_channel_size, 
                                    output_size_h, output_size_w])
          else:
            raise ValueError("unknown data_format")
    
          #creating weights:
          shape = [filter_size_h, filter_size_w, 
                   output_channel_size, input_channel_size]
          W_upconv = tf.get_variable("w", shape=shape, dtype=dtype,
                                     initializer=init_w)
          
          shape=[output_channel_size]
          b_upconv = tf.get_variable("b", shape=shape, dtype=dtype, 
                                     initializer=init_b)
          
          upconv = tf.nn.conv2d_transpose(input, W_upconv, output_shape, stride_shape,
                                          padding=padding,
                                          data_format=data_format)
          output = tf.nn.bias_add(upconv, b_upconv, data_format=data_format)
          
          #Now output.get_shape() is equal (?,?,?,?) which can become a problem in the 
          #next layers. This can be repaired by reshaping the tensor to its shape:
          output = tf.reshape(output, output_shape)
          #now the shape is back to (?, H, W, C) or (?, C, H, W)
          
          return output

