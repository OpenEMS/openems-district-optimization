import pandas as pd
import numpy as np

import src.forecast.nn.runtime_regression_features as runtime


"""Minimalbeispiel des Trainings eines CNNs """
# run script and training of neural network is executed

###############
# create data #
###############

# usually data is either loaded from files, or paths are given in param. See runtime_regression_features.main for
# detailed documentation

# load
data = pd.Series(np.random.rand(10000))
data.index = pd.date_range(start='01-01-2020 12:00', periods=10000, freq='30min')
# temperature
temp = pd.Series(np.random.rand(10000))
temp.index = data.index

#######################
# training parameters #
#######################

# see below for detailed documentation of parameters
param = dict(epochs=10,
             batch_size=128,
             seq_len=7*48, pred_len=60, pred_step=1,
             filters1=8, kernel_size1=6,
             dense_size1=8, dense_size2=4,
             dropout_rate=0.6,
             features=['weekend', 'weekend_inv']
             )

############
# run main #
############

# the model is automatically saved to the path given in param['output_folder'] with the filename defined in `runtime`
runtime.main(data=data, param=param, temp=temp, model=None)
# a pre-defined model can also be given to main. If not, model is compiled according to config_regression_features.py
# with default architecture

################
# reload model #
################

# relative path of project directory
import os
relative_path = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

from src.forecast.nn.modules_regression_features import reload_model
model_neu = reload_model(filename='trained_nn_model', path=os.path.join(relative_path, 'output', 'cnn', 'output_default'))

print('Finished!')

"""
#######################
How to make predictions
#######################

In order to make predictions with a trained or loaded model, the same steps as in `runtime` must be executed:
- load data and temperature as pd.Series with DatetimeIndex
- make regression on temperature and save regression models
- normalise data and save normalisation parameters
- create features
- bring data in the right input format (using `modules.create_multi_input`)
- predict forecast
- re-normalise with saved normalisation parameters
- make inverse regression with saved regression models
"""


"""
#######################
# documentation param #
########################

# necessary parameters #
########################
epochs: number of epochs in the training
batch_size: amount of sequences in one batch in the training
seq_len: number of data points the net receives as input
pred_len: the number of data points to be predicted
pred_step: the steps in which the data points of pred_len are predicted i.e. pred_step=1 means each point of pred_len
          is predicted individually and then added to the data and used to predict the next point etc. <-- recommended method
filters1: number of filters of the CNN layer
kernel_size1: size of filters / kernels of the CNN layer
dense_size1: size of first dense layer
dense_size2: size of second dense layer
dropout_rate: rate of neurons that are deactivated during training. The number of the second dense layer is adjusted in a way that always "dense_size2" number of neurons are active

# optional parameters with default values #
###########################################
name -> 'default': name of the model
cut_len -> 1: describes with which distance the training samples are created from `train`
normalise_window -> True: should data be normalised before training
useCNN -> True
loss_fun -> 'mse': used loss function
activation_conv -> 'relu': activation function of the neurons in the convolutional layer
activation_dense -> 'linear': activation function of the output neuron
number_of_predictions -> 5: number of predictions, which are plotted
show_plot -> False: should plot be shown after training
save_plot -> True: should plot be saved after training
features -> None: which features should be used during training
output_folder -> 'output_default': name of output folder. Data is saved in: "project folder"/output/cnn/"output_folder"

#############################################
# Empfohlene Parameter für Daten aus MAGGIE #
#############################################
# epochs= 40
# batch_size = 128
# seq_len = 7*48  # one week of data necessary; in this case the sample frequency is 30 minutes
# features = ['weekend', 'weekend_inv']
# filters1 = 8
# kernel_size1 = 6
# dense_size1 = 8
# dense_size2 = 4
# dropout_rate = 0.6
# pred_step = 1
# pred_len must be set according to the required forecast length"""
