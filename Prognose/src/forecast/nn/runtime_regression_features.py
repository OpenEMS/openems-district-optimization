import os
import numpy as np
import pandas as pd
import time
import warnings
from typing import Optional, Tuple
from keras import backend as K
from tensorflow.keras import Model as Model_k

import src.forecast.nn.modules_regression_features as modules
import src.forecast.nn.config_regression_features as config


def main(param: dict, data: Optional[pd.Series] = None, model: Optional[Model_k] = None,
         temp: Optional[pd.Series] = None):
    """
    The main function that goes through the entire process of loading the data, pre-processing the data,
    building the model, fitting the model on the train data, saving the model and plotting the loss curves in the end.
    It also outputs a plot of the performance on the test_data.

    All of this is done with the parameters defined in the param dictionary.
    One may pass this by hand, or use the parameters of config.py.
    Same goes for the data. One can either pass a DataFrame with shape[1]=1 that contains a timeSeries
    or load the data from disk with the path defined in the param dictionary.
    Parameters
    ----------
    param: dict
        dictionary with all necessary information about the data, model, training, saving.
        Has to include: data_path, data_file, output_folder,
                        regression, win_size, seq_len, pred_len, epochs, batch_size,
                        and network parameters.
    data: pd.Series
        data as Series with DateTimeIndex; if None, data will be loaded from param['data_path'] and param['data_file']; Default=None
    model: keras.model
        a model can be passed and will be used for the training; if None, model will be created according to param; Default=None
    temp: pd.Series
        temperature time series

    Returns
    -------
    results: dict
        statistical evaluation of the model on the validation data
    history: keras.history
        training history
    """

    ###################
    # define settings #
    ###################

    ## get absolute path ##
    # goes back to project folder and gets absolute path.
    # That path is used afterwards for saving etc. . Only data in project folder can be accessed.
    relative_path = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

    ## create params and build model ##
    # create model if not given as input
    if model is None:
        # create optimiser
        param, optimizer = config.build_param(param)
        # auto fill non-existent parameters in `param` with default values
        param = config.set_default_parameters(param)
        # build model from parameters
        model = config.build_model(param, optimizer)

    ## define paths ##
    #  defines the path where data is saved in order to access it in case it was not given as input
    if data is None:
        data_location = os.path.join(relative_path, param['data_path'], param['data_file'])
    else:
        data_location = None
    # defines path were outputs are stored
    output_path = os.path.join(relative_path, 'output', 'cnn', param['output_folder'])

    ## check if regression should be executed ##
    try:
        param['regression']
    except KeyError:
        param['regression'] = False
        warnings.warn('Training is continued without regression!')

    ##################
    # Pre-processing #
    ##################

    ## load and split data ##
    # create training, validation, and test set from given data / data path
    train, val, test, param = modules.preproc_data(filename=data_location, param=param, data=data)
    # load and check corresponding temperature time series
    if temp is None:
        temp, param = modules.load_temp(param, freq=train.index.freq)
    else:
        if not isinstance(temp, pd.Series) or not isinstance(temp.index, pd.DatetimeIndex):
            raise Exception('temp must be pd.Series with index of pd.DatetimeIndex')

    ## make regression ##
    if param['regression']:
        # load regressor from given path (here it is the temperature)
        regressor, param = modules.load_regressor(param=param)
        # make linear regression on mean and amplitude of training and validation set
        reg1, reg2, param = modules.learn_linReg(regressor, train, param, win_size=param['win_size'])
        # apply linear regressions
        train = modules.make_linReg(regressor, train, reg1, reg2, win_size=param['win_size'])
        val = modules.make_linReg(regressor, val, reg1, reg2, win_size=param['win_size'])
        if test is not None:
            test = modules.make_linReg(regressor, test, reg1, reg2, win_size=param['win_size'])

    ## normalise data ##
    if param['normalise_window']:
        train, val, test, scaler, param = modules.normalise_data(train, val, test, param)

    ## create input with labels ##
    x_train, y_train, x_val, y_val, x_test, y_test = modules.create_labeled_data(train, val, test, param, temp)
    # possible features are created and input shape of input data is adapted for keras network
    if param['features']:
        x_train = modules.create_multi_input(data=x_train, param=param)
        x_val_for_train = modules.create_multi_input(data=x_val, param=param)
    else:
        x_train = x_train.values[:, :, np.newaxis]
        x_val_for_train = x_val.values[:, :, np.newaxis]

    #####################
    # Define model name #
    #####################

    filename = 'trained_nn'
    # # model name can also be put together from params:
    # filename = f"{param['name']}_regression_{param['regression']}_drop_{param['dropout_rate']}"

    #######################
    # training of network #
    #######################

    # print model summary
    print(model.summary())
    # start timer
    global_start_time = time.time()

    ## fit model ##
    history = model.fit(x=x_train, y=y_train.values,
                        validation_data=(x_val_for_train, y_val.values),
                        epochs=param['epochs'], batch_size=param['batch_size'],
                        shuffle=True)
    del x_val_for_train

    # end timer
    global_end_time = time.time()
    global_computation_time = global_end_time - global_start_time
    print('Training duration (s) : ', global_computation_time)

    #######################
    # evaluation of model #
    #######################

    # number of input sequences in data must (at least) equal number of predicted points as sequences are
    # created with cut_len = 1 and thereby the difference is only 1 point

    ## create prediction of validation set ##
    predictions = modules.predict_sequences_multiple(model=model, x_val=x_val, param=param)

    ## inverse pre-processing ##
    if param['normalise_window']:
        predictions = modules.renormalise_predictions(predictions=predictions, scaler=scaler)
        # train = modules.renormalise_data(train, scaler)
        val = modules.renormalise_data(val, scaler)
        # test = modules.renormalise_data(train, scaler)
    if param['regression']:
        predictions = modules.inverse_linReg_prediction(predictions, regressor, reg1, reg2, param,
                                                        win_size=param['win_size'])
        val = modules.inverse_linReg_data(val, regressor, reg1, reg2, win_size=param['win_size'])

    ##########################
    # compute model accuracy #
    ##########################

    # accuracy is computed with validation data
    result_total, result_col = modules.analyze(predictions, val)
    results = dict(total=result_total, column=result_col)
    print('\nTotal validation accuracy:\n' + str(result_total))

    #############################
    # save model and parameters #
    #############################

    modules.save_final(model, param, results, history, filename, output_path)
    print('Files and results saved in:  ' + str(output_path))
    # # plot predictions with final model and open history plot
    # plots.plot_results_multiple(predictions[:param['number_of_predictions']], val, param, path=output_path,
    #                             filename=filename, compute_time=global_computation_time)

    ## delete parameters ##
    # mainly for grid searches so memory is empty when new loops starts
    if param['regression'] is True:
        del regressor, reg1, reg2, param['reg1'], param['reg2']
    if param['normalise_window'] is True:
        del scaler, param['scaler']
    del param, model, train, val, test, predictions, x_train, y_train, x_val, y_val, x_test, y_test, data, temp, result_col, result_total, global_computation_time, global_end_time, global_start_time, optimizer
    K.clear_session()

    return results, history


