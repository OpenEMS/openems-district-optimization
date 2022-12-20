import numpy as np
import pandas as pd
from typing import List, Optional
from warnings import warn

from src.forecast.xg.models import XGBoostModel_CV
import src.forecast.xg.modules as modules


def main(data: pd.Series, temp: pd.Series, fc_horizon:pd.Timedelta, lb_horizon: pd.Timedelta, cols_to_keep: List[str],
         evaluation: Optional[bool] = False) -> XGBoostModel_CV:
    """
    Main function that creates the features from `data` and `temp`, keeps only desired features and fits model.

    Parameters
    ----------
    data: pd.Series
        load time series in kW. name of series must be "load"
    temp: pd.Series
        temperature time series in °C. name of series must be "temp"
    fc_horizon: pd.Timedelta
        time difference from the last know point in `load` to the first forecasted point
    lb_horizon: pd.Timedelta
        time range of the last points in `data` and `load` that are used for the statistical features
    cols_to_keep: list
        list of features that should kept for training of the model. Possible values:
        'dayofweek', 'hour', 'isweekend', 'temp', '36h', '2d', '1w', '2w', 'temp_36h', 'temp_2d', 'temp_1w', 'temp_2w',
        'past_min', 'past_q25', 'past_q50', 'past_q75', 'past_max', 'past_minmax_ratio', 'past_var', 'past_diff',
        'past_temp_min', 'past_temp_q25', 'past_temp_q50', 'past_temp_q75', 'past_temp_max', 'past_temp_minmax_ratio',
        'past_temp_var', 'past_temp_diff', 'hor_temp_min', 'hor_temp_q25', 'hor_temp_q50', 'hor_temp_q75',
        'hor_temp_max', 'hor_temp_minmax_ratio', 'hor_temp_var', 'hor_temp_diff'
    evaluation: bool
        if True, evaluation metrics of the trained model with training data is calculated

    Returns
    -------
    model:
        fitted model
    """

    ################
    # check inputs #
    ################

    if not isinstance(data, pd.Series):
        raise TypeError("`data` needs to be of type: <pd.Series>")
    if not isinstance(data.index, pd.DatetimeIndex):
        raise TypeError("`data.index` needs to be of type: <pd.DatetimeIndex>")
    if data.name is None:
        warn('"data" series was not named. Name was set to "load".')
        data.name = 'load'
    elif not data.name == 'load':
        raise TypeError('Name of data series must be "load"!')
    if not isinstance(temp, pd.Series):
        raise TypeError("`temp` needs to be of type: <pd.Series>")
    if not isinstance(temp.index, pd.DatetimeIndex):
        raise TypeError("`temp.index` needs to be of type: <pd.DatetimeIndex>")
    if temp.name is None:
        warn('"temp" series was not named. Name was set to "temp"')
    elif not temp.name == 'temp':
        raise TypeError('name of temp series must be "temp"!')

    ###################
    # create features #
    ###################

    data = pd.DataFrame(data)
    data = modules.create_features(data, temp, fc_horizon, lb_horizon, shifts=["36h", "2d", "1w", "2w"])

    ###################
    # train the model #
    ###################

    X = data.copy()
    y = X['load']  # target
    X = X.drop(['load'], axis=1)  # everything but the target

    # only keep selected features
    X = X[cols_to_keep]

    # initialise model
    model = XGBoostModel_CV(name='example')
    # fit model (with cross validation)
    model.fit(X, y, cross_val=True)

    ##################
    # evaluate model #
    ##################

    # test or validation data can be evaluated similarly
    if evaluation is True:
        prediction = model.predict(X=X)
        dev = y - prediction
        dev_np = np.array(dev, dtype=float)
        results_total = dict(info='average accuracy of all data points',
                             mae=np.mean(np.abs(dev_np)),
                             mse=np.mean(dev_np * dev_np),
                             std=np.std(dev_np),
                             var=np.var(dev_np),
                             max_dev=np.amax(dev_np),
                             min_dev=np.amin(dev_np)
                             )
        print(results_total)

    return model






