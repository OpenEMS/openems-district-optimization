##### config_c #####

import os
import tensorflow.keras.optimizers as optimizers
from tensorflow.keras.models import Model
from tensorflow.keras.layers import Flatten, Input, Conv1D, Concatenate, Dropout, Dense, AveragePooling1D
from tensorflow.keras.losses import MeanSquaredError
from typing import Optional, Tuple


def build_param(param: dict,
                optim_opt: Optional[dict] = None
                ) -> Tuple[dict, optimizers.Nadam]:
    """
    Completes the param input with all necessary entries. If entries do not exist default values are set.
    Default values: loss_fun=mse, activation_conv=relu, activation_dense=linear, optimizer=Nadam

    Parameters
    ----------
    param: dict
        dictionary with setup information of network and training
    optim_opt: dict
        with entries of dictionary the "Nadam" optimizer can be initialised with customized arguments. If none are given
        the default values are used.
        Necessary entries of optim_dict: lr, beta_1, beta_2, epsilon, schedule_decay

    Returns
    -------
    param: dict
    optimizer: optimizer
    """

    # name the model
    param.setdefault('name', 'default')

    # parameter for the net
    param.setdefault('loss_fun', 'mse')
    param.setdefault('activation_conv', 'relu')
    param.setdefault('activation_dense', 'linear')

    # put the optimizer-options into param to be able to access it later on for evaluation
    if not optim_opt:
        optim_opt = dict(type='keras.optimizers.Nadam',
                         lr=0.001,
                         beta_1=0.9,
                         beta_2=0.999,
                         epsilon=None,
                         schedule_decay=0.004)
    optimizer = optimizers.Nadam(lr=optim_opt['lr'], beta_1=optim_opt['beta_1'], beta_2=optim_opt['beta_2'],
                                 epsilon=optim_opt['epsilon'], schedule_decay=optim_opt['schedule_decay'])
    param.setdefault('optimizer', optim_opt)

    # set default parameters for all parameters that are not set by hand
    param = set_default_parameters(param)

    return param, optimizer


def build_model(param: dict,
                optimizer: optimizers.Nadam
                ) -> Model:
    """
    Functions builds and compiles the model, so it is ready for fitting. The given network consists of one CNN layer,
    followed by fully-connected layers. Additionally, the features are added to the network after the CNN layer.
    The network produces a one-point output. Hence forecasts are created iteratively.

    Parameters
    ----------
    param: dict
        inlcudes all the necessary parameters of the network: filters1, kernel_size1, dense_size1, dense_size2,
        dropout_rate, seq_len, features, activation_conv, loss_fun
    optimizer: optimizer
        Optimizer object for the neural network
    Returns
    -------
    model: keras.model
    """

    ## network parameters ##
    filters1 = param['filters1']
    kernel_size1 = param['kernel_size1']
    dense_size1 = param['dense_size1']
    dense_size2 = param['dense_size2']
    dropout_rate = param['dropout_rate']

    ## network construction ##
    # input layer
    input_main_shape = [param['seq_len'], 1]
    input_main = Input(shape=input_main_shape)
    if param['features'] is not None:
        input_feat_shape = [len(param['features']), ]
        input_feat = Input(shape=input_feat_shape)

    # hidden layer
    conv1 = Conv1D(filters=filters1, kernel_size=kernel_size1, strides=1, activation=param['activation_conv'])(input_main)
    conv1 = Flatten()(conv1)
    if dropout_rate is not None:
        conv1 = Dropout(rate=dropout_rate)(conv1)

    if param['features'] is not None:
        dense = Concatenate()([conv1, input_feat])
    else:
        dense = conv1

    # output layer
    dense = Dense(units=dense_size1, activation='softsign')(dense)
    dense = Dense(units=dense_size2, activation='softsign')(dense)
    dense = Dense(units=1, activation=param['activation_dense'])(dense)

    # model compilation
    if param['features'] is not None:
        model = Model(inputs=[input_main, input_feat], outputs=dense)
    else:
        model = Model(inputs=input_main, outputs=dense)
    model.compile(loss=param['loss_fun'], optimizer=optimizer)

    return model


def set_default_parameters(param: dict) -> dict:
    """
    Definition of Parameters:

    epochs: number of epochs in the training
    batch_size: amount of sequences in one batch in the training
    data_splits: defines the percentages of the data that are used for train, validation and test
                e.g. (0.8, 0.2, 0.0) means that the first 80% are used for training, the next 20% for validation and
                the last 0% of the data are reserved as test data
    cut_len: the distance in which the data is cutted into the training examples
    normalise_window: If True, data will be normalised between values -1 to 1. The largest and smallest value of the
                      whole dataset are used for the normalisation and returned in order to be able to renormalise the
                      data afterwards
    useCNN: If True, the data will be reshaped in a way that it according to the requirements of CNNs (and als LSTMs)
    number_of_predictions: Points that are predicted with the trained network in order to create the final plot
    name: Name of the net that is used part of the file name of the results
    data_file: Name of the data file that is used for training
    data_folder: folder where the data_file is located
    output_folder: folder where the output is to be stored; if None, name of the data_file is used
    """

    # default parameters
    param.setdefault('name', 'default')
    param.setdefault('data_splits', (0.8, 0.2, 0.0))
    param.setdefault('cut_len', 1)
    param.setdefault('normalise_window', True)
    param.setdefault('regression', False)
    param.setdefault('useCNN', True)
    param.setdefault('loss_fun', 'mse')
    param.setdefault('activation_conv', 'relu')
    param.setdefault('activation_dense', 'linear')
    param.setdefault('number_of_predictions', 5)
    param.setdefault('show_plot', False)
    param.setdefault('save_plot', True)
    param.setdefault('features', None)
    param.setdefault('output_folder', 'output_default')

    return param
