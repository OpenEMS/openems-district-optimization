import os
import numpy as np
import pandas as pd
import src.forecast.xg.runtime as runtime

"""
This is an example file which shows the functionality of the xgboost forecaster.

The feature extraction during training may take some time as does the cross validation of the model.
However, they both enhance the performance of the model drastically.
When the model is applied real-world operation, the computational load for creating the necessary features to make
a one-day forecast are minimal. The cross-validation takes only place during training.
"""


######################
# load / create data #
######################

## training with random data ##
# load
data = pd.Series(np.random.rand(1000), name='load')
data.index = pd.date_range(start='01-01-2020 12:00', periods=1000, freq='30min', tz='UTC')
# temperature
temp = pd.Series(np.random.rand(1000), name='temp')
temp.index = data.index

# ## training with real data
# relative_path = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
# data = pd.read_csv(os.path.join(relative_path, 'data', 'ireland', 'file_residential_sum_350.csv'), index_col=0, header=None)
# data.index = pd.to_datetime(data.index, utc=True)
# data.index.freq = pd.infer_freq(data.index)
# data.columns = ['load']
# data = data['load']
#
# temp = pd.read_csv(os.path.join(relative_path, 'data', 'ireland', 'temp_phoenixpark_interpolated30.csv'), header=None, index_col=0)
# temp.index = pd.to_datetime(temp.index, utc=True)
# temp.index.freq = pd.infer_freq(temp.index)
# temp.columns = ['temp']
# temp = temp['temp']
#
# temp = temp.loc[data.index.intersection(temp.index)]
# data = data.loc[data.index.intersection(temp.index)]

##################
# model settings #
##################

# forecast horizon
fc_horizon = pd.Timedelta('36h')
# lookback horizon
lb_horizon = pd.Timedelta('36h')

# features to us:
cols_to_keep = [
    'hour', 'isweekend',
    'temp',
    '2d', '1w',
    'past_q25',    'past_q50',    'past_q75',    'past_diff',
    'hor_temp_q25',    'hor_temp_q50',    'hor_temp_q75',       'hor_temp_diff']

###################
# execute runtime #
###################

# in `runtime` all processes for training the model and predicting data with the model are described
# it is sufficient to use `runtime.main` to train the model when the necessary parameters are given
model = runtime.main(data, temp, fc_horizon, lb_horizon, cols_to_keep, evaluation=True)

#######################
# save and load model #
#######################

# save model #
import os
relative_path = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
print(os.path.join(relative_path, 'output', 'xg', 'xg_model.pkl'))
model.save_trained_model(os.path.join(relative_path, 'output', 'xg', 'xg_model.pkl')) # save trained model to given path
# load model #
from src.forecast.xg.models import XGBoostModel_CV
model_neu = XGBoostModel_CV()  # first initialise model
model_neu.load_existing_model('xg_model.pkl')  # load model from path