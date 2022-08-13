---
title: "Creating RNN, LSTM and bidirectional RNNLSTMs with TensorFlow"
slug: "creating-rnn-lstm-and-bidirectional-rnnlstms-with-tensorflow"
draft: false
images: []
weight: 9253
type: docs
toc: true
---

## Creating a bidirectional LSTM
    import tensorflow as tf

    dims, layers = 32, 2
    # Creating the forward and backwards cells
    lstm_fw_cell = tf.nn.rnn_cell.BasicLSTMCell(dims, forget_bias=1.0)
    lstm_bw_cell = tf.nn.rnn_cell.BasicLSTMCell(dims, forget_bias=1.0)
    # Pass lstm_fw_cell / lstm_bw_cell directly to tf.nn.bidrectional_rnn
    # if only a single layer is needed
    lstm_fw_multicell = tf.nn.rnn_cell.MultiRNNCell([lstm_fw_cell]*layers)
    lstm_bw_multicell = tf.nn.rnn_cell.MultiRNNCell([lstm_bw_cell]*layers)

    # tf.nn.bidirectional_rnn takes a list of tensors with shape 
    # [batch_size x cell_fw.state_size], so separate the input into discrete
    # timesteps.
    _X = tf.unpack(state_below, axis=1)
    # state_fw and state_bw are the final states of the forwards/backwards LSTM, respectively
    outputs, state_fw, state_bw = tf.nn.bidirectional_rnn(lstm_fw_multicell, lstm_bw_multicell, _X, dtype='float32')

**Parameters**
* `state_below` is a 3D tensor of with the following dimensions: [`batch_size`, maximum sequence index, `dims`]. This comes from a previous operation, such as looking up a word embedding.
* `dims` is the number of hidden units.
* `layers` can be adjusted above 1 to create a _stacked LSTM network_.

**Notes**

* `tf.unpack` may not be able to determine the size of a given axis (use the `nums` argument if this is the case).
* It may be helpful to add an additional weight + bias multiplication beneath the LSTM (e.g. `tf.matmul(state_below, U) + b`.


