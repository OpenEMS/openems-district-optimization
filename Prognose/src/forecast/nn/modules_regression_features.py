##### modules #####

import os
import warnings
import json
import pandas as pd
import numpy as np
from scipy import stats
import src.forecast.nn.plots_regression as plots
from sklearn.preprocessing import MinMaxScaler
from sklearn.linear_model import LinearRegression
from tensorflow.keras.models import Model
from typing import Tuple, Optional

os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'  # Hide messy TensorFlow warnings
warnings.filterwarnings("ignore")  # Hide messy Numpy warnings


def series_loader(filename: str,
                  param: dict,
                  freq: Optional[str] = None,
                  index_col: Optional[int] = 0,
                  delimiter: Optional[str] = ','
                  ) -> Tuple[pd.Series, dict]:
    """
    loads the Series from a csv-file
    Parameters
    ----------
    filename: str
        path of csv-file
    param: dict
        includes all necessary parameters
    freq: str
        frequency of the data, if None, frequency is automatically assigned, default=None
    index_col: int
        column of file that is index
    delimiter: str
        default=','

    Returns
    -------
    data: TimeSeries
    param: dict
    """

    df = pd.read_csv(filename, index_col=index_col, header=None, delimiter=delimiter)
    df.index = pd.to_datetime(df.index)
    if not df.index.freq:
        if freq is None:
            df = df.asfreq(pd.infer_freq(df.index))
        else:
            df = df.asfreq(freq)
    param['freq_data'] = df.index.freq.freqstr
    df.index.name = 'index'
    df.columns = ['load']
    s = df['load']
    return s, param


def load_regressor(param: dict,
                   regressor_path: Optional[str] = None,
                   regressor_name: Optional[str] = None,
                   freq: Optional[str] = None,
                   index_col: Optional[int] = 0,
                   delimiter: Optional[str] = ','
                   ) -> Tuple[pd.Series, dict]:
    """
    loads an regressor from a csv-file; data location can be given directly or in param
    Parameters
    ----------
    param: dict
        includes all necessary parameters
    regressor_path: str
        local path of regressor file
    regressor_name: str
        name of regressor file
    freq: str
        frequency of data; if None, frequency is automatically assigned, Default=None
    index_col: int
        column in which index is located, Default=0
    delimiter: str
        delimiter of csv-file, Default=','

    Returns
    -------
    s: pd.TimeSeries
    param: dict()
    """

    if regressor_path:
        param['regressor_path'] = regressor_path
    if regressor_name:
        param['regressor_name'] = regressor_name

    df = pd.read_csv(os.path.join(param['regressor_path'], param['regressor_name']),
                     index_col=index_col, header=None, delimiter=delimiter)
    df.index = pd.to_datetime(df.index)
    if not df.index.freq:
        if freq is None:
            df = df.asfreq(pd.infer_freq(df.index))
        else:
            df = df.asfreq(freq)
    df.index.name = 'index'
    df.columns = ['load']
    s = df['load']
    return s, param


def preproc_data(filename: str,
                 param: dict,
                 data: Optional[pd.Series] = None
                 ) -> Tuple[pd.Series, pd.Series, pd.Series, dict]:
    """
    loads data if none is passed, splits the data in train, val, and test according to param.

    Parameters
    ----------
    filename: str
        file to read the data from if data is not passed explicitly
    param: dict
    data: pd.Series
        TimeSeries that contains the load

    Returns
    -------
    train: pd.Series
    val: pd.Series
    test: pd.Series
    param: dict
    """

    # initialise needed parameters
    splits = param['data_splits']

    # load data if it is not passed by hand
    if data is None:
        data, param = series_loader(filename, param, index_col=0)

    else:
        if not isinstance(data, pd.Series):
            raise Exception('Data has to be a pandas.TimeSeries object!')
        if not isinstance(data.index, pd.DatetimeIndex):
            raise Exception('Series needs to have a pd.DateTimeIndex')
        if not data.index.freq:
            raise Exception('TimeSeries has no frequency!')

    # splits into training, validation and test
    train_split, val_split, test_split = splits
    train_end = round(train_split * data.shape[0])
    val_end = round((train_split + val_split) * data.shape[0])

    train = data[:train_end].copy()
    val = data[train_end:val_end].copy()
    if test_split == 0.0:
        test = None
    else:
        test = data[val_end:].copy()

    return train, val, test, param


def normalise_data(train: pd.Series,
                   val: pd.Series,
                   test: pd.Series,
                   param: dict
                   ) -> Tuple[pd.Series, pd.Series, pd.Series, MinMaxScaler, dict]:
    """
    Normalisation of data in feature_range=(-1,1). Scaler is trained on training data and applied on train, val, and test.
    Thus, values in val and test can be out of feature_range. Saves parameter of MinMaxScaler in params.
    Parameters
    ----------
    train: pd.Series
    val: pd.Series
    test: pd.Series
    param: dict

    Returns
    -------
    train: pd.Series
        normalised training data
    val: pd.Series
        normalised validation data
    test: pd.Series
        normalised testing data
    scaler: sklearn.MinMaxScaler
        scaler object trained on the training data
    param: dict
        updated param with scaler parameters
    """

    # Normalisation. Fit scaler only on train. Then transform all three parts
    scaler = MinMaxScaler(feature_range=(-1, 1))
    # check if normalisation is necessary
    if param['normalise_window']:
        train[:] = scaler.fit_transform(train.values.reshape(-1, 1)).reshape(-1, )
        val[:] = scaler.transform(val.values.reshape(-1, 1)).reshape(-1, )
        if test is not None:
            test[:] = scaler.transform(test.values.reshape(-1, 1)).reshape(-1, )
        # save parameters of scaler in param
        param['scaler'] = dict(min_=scaler.min_[0],
                               scale_=scaler.scale_[0],
                               data_min_=scaler.data_min_[0],
                               data_max_=scaler.data_max_[0],
                               data_range_=scaler.data_range_[0],
                               feature_range=scaler.feature_range)

    return train, val, test, scaler, param


def renormalise_data(data: pd.Series,
                     scaler: Optional[MinMaxScaler] = None,
                     param: Optional[dict] = None
                     ) -> pd.Series:
    """
    Inverse normalisation of a time series. Either a MinMaxScaler object or a dictionary must be given.
    Parameters
    ----------
    data: pd.Series
        data to be renormalised
    scaler: sklearn.preprocessing.MinMaxScaler
    param: dict
        if scaler is not given as input, param must include all the necessary parameters to create a scaler

    Returns
    -------
    data_renorm: pd.Series
        renormalised time series
    """

    if not scaler:
        from sklearn.preprocessing import MinMaxScaler
        scaler = MinMaxScaler()
        scaler.min_ = np.array([param['scaler']['min_']])
        scaler.scale_ = np.array([param['scaler']['scale_']])
        scaler.data_min_ = np.array([param['scaler']['data_min_']])
        scaler.data_max_ = np.array([param['scaler']['data_max_']])
        scaler.data_range_ = np.array([param['scaler']['data_range_']])
        scaler.feature_range = param['scaler']['feature_range']

    data_renorm = pd.Series(index=data.index)
    data_renorm[:] = scaler.inverse_transform(data.values.reshape(-1, 1)).reshape(-1, )
    return data_renorm


def renormalise_predictions(predictions: pd.DataFrame,
                            scaler: Optional[MinMaxScaler] = None,
                            param: Optional[dict] = None
                            ) -> pd.DataFrame:
    """
    Inverse transformation of predictions. Either a MinMaxScaler object or a dictionary must be given.
    Parameters
    ----------
    predictions: pd.DataFrame
        DataFrame with one prediction in each row; index is the timestamp of first point of the prediction
    scaler: sklearn.preprocessing.MinMaxScaler
    param: dict
        if scaler is not given, param must include all the necessary parameters for a scaler

    Returns
    -------
    predictions_renorm: pd.DataFrame
        renormalised predictions; indices and columns are the same as input
    """

    if scaler is None and param is None:
        raise Exception('Either Scaler oder param must be given as input.')
    if not scaler:
        from sklearn.preprocessing import MinMaxScaler
        scaler = MinMaxScaler()
        scaler.min_ = np.array([param['scaler']['min_']])
        scaler.scale_ = np.array([param['scaler']['scale_']])
        scaler.data_min_ = np.array([param['scaler']['data_min_']])
        scaler.data_max_ = np.array([param['scaler']['data_max_']])
        scaler.data_range_ = np.array([param['scaler']['data_range_']])
        scaler.feature_range = param['scaler']['feature_range']

    predictions_renorm = pd.DataFrame(index=predictions.index, columns=predictions.columns)
    for index in predictions_renorm.index:
        predictions_renorm.loc[index] = scaler.inverse_transform(predictions.loc[index].values.reshape(-1, 1)).reshape(-1, )
    return predictions_renorm


def create_labeled_data(train: pd.Series,
                        val: pd.Series,
                        test: pd.Series,
                        param: dict,
                        temp: Optional[pd.Series] = None
                        ) -> Tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame, pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    """
    Splits the data into pieces of specified length and creates labels to it.

    Parameters
    ----------
    train: pd.Series
    val: pd.Series
    test: pd.Series
    param: dict
        needed entries are: 'seq_len' - the sequence length of the data; 'pred_len' - the number of points to be
        predicted; 'cut_len' - the distance in which the data is cut into sequences
    temp: pd.Series
        temperature time series

    Returns
    -------
    x_train: pd.DataFrame
        the DateTimeIndex of the DataFrame refers to the first point that is predicted with the sequence
    y_train: pd.DataFrame
        the DateTimeIndex of the DataFrame refers to the first point in row (that is the first predicted point)
    x_val: pd.DataFrame
    y_val: pd.DataFrame
    x_test: pd.DataFrame
    y_test: pd.DataFrame
    """

    # initialise
    seq_len = param['seq_len']
    pred_step = param['pred_step']
    cut_len = param['cut_len']

    train_np = train.values
    val_np = val.values
    if test is not None:
        test_np = test.values

    # CREATE LABELED DATA #
    # creates an array consisting of parts of the data with length seq_len+pred_step
    # cut_len cuts complete data into examples with the starting points of each example cut_len points apart
    sequence_length = seq_len + pred_step
    train_labeled, val_labeled, test_labeled = [], [], []
    # training
    for i in range(0, len(train) - sequence_length + 1, cut_len):
        train_labeled.append(train_np[i: i + sequence_length])
    train_labeled = np.array(train_labeled)
    x_train = train_labeled[:, :-pred_step]
    y_train = train_labeled[:, -pred_step:]
    # validation
    for i in range(0, len(val) - sequence_length + 1, cut_len):
        val_labeled.append(val_np[i: i + sequence_length])
    val_labeled = np.array(val_labeled)
    x_val = val_labeled[:, :-pred_step]
    y_val = val_labeled[:, -pred_step:]
    # test
    if test is not None:
        for i in range(0, len(test) - sequence_length + 1, cut_len):
            test_labeled.append(test_np[i: i + sequence_length])
        test_labeled = np.array(test_labeled)
        x_test = test_labeled[:, :-pred_step]
        y_test = test_labeled[:, -pred_step:]
    else:
        x_test, y_test = None, None

    del train_np, val_np
    if test is not None:
        del test_np

    # create DataFrames
    if pred_step == 1:
        index_train = train.index[seq_len::cut_len]
        index_val = val.index[seq_len::cut_len]
        if test is not None:
            index_test = test.index[seq_len::cut_len]
    else:
        index_train = train.index[seq_len:-(pred_step-1):cut_len]
        index_val = val.index[seq_len:-(pred_step-1):cut_len]
        if test is not None:
            index_test = test.index[seq_len:-(pred_step-1):cut_len]
    x_train = pd.DataFrame(x_train, index=index_train)
    y_train = pd.DataFrame(y_train, index=index_train)
    x_val = pd.DataFrame(x_val, index=index_val)
    y_val = pd.DataFrame(y_val, index=index_val)
    if test is not None:
        x_test = pd.DataFrame(x_test, index=index_test)
        y_test = pd.DataFrame(y_test, index=index_test)

    # create features and add them to the dataframes
    if param['features']:
        features_train = create_features(data=x_train, param=param, temp=temp)
        features_val = create_features(data=x_val, param=param, temp=temp)
        if test is not None:
            features_test = create_features(data=x_test, param=param, temp=temp)
        x_train = x_train.merge(features_train, how='left', left_index=True, right_index=True)
        x_val = x_val.merge(features_val, how='left', left_index=True, right_index=True)
        if test is not None:
            x_test = x_test.merge(features_test, how='left', left_index=True, right_index=True)

        del features_train, features_val
        if test is not None:
            del features_test

    return x_train, y_train, x_val, y_val, x_test, y_test


def learn_linReg(regressor: pd.Series,
                 regressand: pd.Series,
                 param: dict,
                 win_size: Optional[int] = 1,
                 two_regressions: Optional[bool] = True
                 ) -> Tuple[LinearRegression, LinearRegression, dict]:
    """
    learns two linear regressions. The first one the data, the second on the squared data
    Parameters. All indices of the regressand must also be in the regressor
    ----------
    regressor: pd.Series
        TimeSeries of the Regressor with DateTimeIndex
    regressand: pd.Series
        TimeSeries of the Regressand with DateTimeIndex
    param: dict
    win_size: int
        size of sliding window; Default=None
    two_regressions: boolean
        if False, only the first regression is trained

    Returns
    -------
    reg1: sklearn.linear_model.LinearRegression
    reg2: sklearn.linear_model.LinearRegression
    param: dict
    """

    if not regressor.index.freq == regressand.index.freq:
        raise Exception('Frequency of the regressor and regressand do not match!')
    reg1 = LinearRegression()
    reg2 = LinearRegression()

    reg1.fit(X=regressor[regressand.index].rolling(window=win_size, center=True).mean().interpolate(limit_direction='both').values.reshape(-1, 1),
             y=regressand.rolling(window=win_size, center=True).mean().interpolate(limit_direction='both').values.reshape(-1, 1))

    if not two_regressions:
        reg2 = None
    else:
        data_reg = regressand - reg1.predict(X=regressor[regressand.index].rolling(window=win_size, center=True).mean().interpolate(limit_direction='both').values.reshape(-1, 1)).reshape(-1,)
        reg2.fit(X=regressor[regressand.index].rolling(window=win_size, center=True).mean().interpolate(limit_direction='both').values.reshape(-1, 1),
                 y=(data_reg ** 2).rolling(window=win_size, center=True).mean().interpolate(limit_direction='both').values.reshape(-1, 1))

    # add regressor parameters to param
    param['reg1'] = dict(coef_=float(reg1.coef_), intercept_=float(reg1.intercept_), win_size=win_size)
    param['reg2'] = dict(coef_=float(reg2.coef_), intercept_=float(reg2.intercept_), win_size=win_size)

    return reg1, reg2, param


def make_linReg(regressor: pd.Series,
                regressand: pd.Series,
                reg1: LinearRegression,
                reg2: LinearRegression,
                win_size: Optional[int] = None,
                two_regressions: Optional[bool] = True
                ) -> pd.Series:
    """
    Uses the given regressors and applies the regressions.

    Parameters
    ----------
    regressor: pd.Series
        TimeSeries of the regressor
    regressand: pd.Series
        TimeSeries of the regressand; all indices of regressand must be found in regressor
    reg1: sklearn.linear_model.LinearRegression
        trained regression for the first regression
    reg2: sklearn.linear_model.LinearRegression
        trained regression for the second regression
    win_size: int
        size of rolling mean window; no rolling mean if None; default=None
    two_regressions: bool
        whether the regression on the variance of regressand is carried out; default=True

    Returns
    -------
    data_reg: pd.Series
        regressand after regressin of type TimeSeries
    """

    if not regressor.index.freq == regressand.index.freq:
        raise Exception('Frequency of the regressor and regressand do not match!')
    if not win_size:
        win_size = 1

    data_reg1 = regressand - reg1.predict(X=regressor[regressand.index].rolling(window=win_size, center=True).mean().interpolate(limit_direction='both').values.reshape(-1, 1)).reshape(-1,)
    if not two_regressions:
        return data_reg1
    else:
        data_reg2 = data_reg1 / np.sqrt(reg2.predict(X=regressor[regressand.index].rolling(window=win_size, center=True).mean().interpolate(limit_direction='both').values.reshape(-1, 1)).reshape(-1,))
        return data_reg2


def load_linReg(reg_parameters: dict,
                param: dict
                ) -> Tuple[LinearRegression, dict]:
    """
    creates a LinearRegression from given/saved parameters
    Parameters and adds 'win_size' to param
    ----------
    reg_parameters: dict
        Dictionary with coef_, intercept_, and win_size
    param: dict

    Returns
    -------
    reg: sklearn.linear_model.LinearRegression
    param: dict

    """
    reg = LinearRegression()
    reg.coef_ = np.array([[reg_parameters['coef_']]])
    reg.intercept_ = np.array([[reg_parameters['intercept_']]])
    param['win_size'] = reg_parameters['win_size']
    return reg, param


def inverse_linReg_data(data: pd.Series,
                        regressor: pd.Series,
                        reg1: LinearRegression,
                        reg2: LinearRegression,
                        win_size: Optional[int] = 1,
                        two_regressions: Optional[bool] = True
                        ) -> pd.Series:
    """
    Makes inverse linear Regression for a TimeSeries using the regressor and the two handed regressor objects.

    Parameters
    ----------
    data: pd.Series
        Series with DateTimeIndex on which the inverse linear Regression is applied
    regressor: pd.Series
        Regressor Series with DateTimeIndex; all indices from data have to exist in regressor
    reg1: sklearn.linear_model.LinearRegression
    reg2: sklearn.linear_model.LinearRegression
    win_size: int
        size of the rolling window for regressor; Default=1
    two_regressions: boolean
        Whether one or two inverse regressions are applied; Default=True

    Returns
    -------
    data_orig: pd.Series
        data after inverese regresseion
    """

    data_reg2 = data.copy()
    if win_size > data_reg2.shape[0]:
        reg_index = pd.date_range(start=data_reg2.index[0] - int((win_size - len(data_reg2.index)) / 2) * data_reg2.index.freq, periods=win_size, freq=data_reg2.index.freq)
    else:
        reg_index = data_reg2.index

    if two_regressions:
        data_reg1 = data_reg2 * np.sqrt(reg2.predict(
            X=regressor[reg_index].rolling(window=win_size, center=True).mean().interpolate(
                limit_direction='both').values.reshape(-1, 1)).reshape(-1, )[:data_reg2.shape[0]])
    else:
        data_reg1 = data_reg2
    data_orig = data_reg1 + reg1.predict(
        X=regressor[reg_index].rolling(window=win_size, center=True).mean().interpolate(
            limit_direction='both').values.reshape(-1, 1)).reshape(-1, )[:data_reg2.shape[0]]
    return data_orig


def inverse_linReg_prediction(prediction: pd.DataFrame,
                              regressor: pd.Series,
                              reg1: LinearRegression,
                              reg2: LinearRegression,
                              param: dict,
                              two_regressions: Optional[bool] = True,
                              win_size: Optional[int] = 1
                              ) -> pd.DataFrame:
    """
    Makes inverse linear regression of the predictions.

    Parameters
    ----------
    prediction: pd.DataFrame
        index is TimeStamp of first point in prediction
    regressor: pd.Series
        Regressor Series with DateTimeIndex; all indices from data have to exist in regressor
    reg1: sklearn.linear_model.LinearRegression
    reg2: sklearn.linear_model.LinearRegression
    param: dict
        must include 'pred_len' and 'freq_data'
    two_regressions: boolean
        whether to do one or two regressions; Default=True
    win_size: int
        size of the rolling window for regressor, Default=1

    Returns
    -------
    pred: pd.DataFrame
        predictions after inverse linear regression; index and columns are unchanged
    """

    pred = prediction.copy()
    for index in pred.index:
        s = prediction.loc[index]
        s.index = pd.date_range(start=index, periods=param['pred_len'], freq=param['freq_data'])
        pred.loc[index] = inverse_linReg_data(s, regressor, reg1, reg2, two_regressions=two_regressions, win_size=win_size).values
    return pred


def predict_sequences_multiple(model: Model,
                               x_val: pd.DataFrame,
                               param: dict
                               ) -> pd.DataFrame:
    """
    Makes prediction for all handed sequences in x_val.
    Parameters
    ----------
    model: keras.Model
        trained Keras model
    x_val: pd.DataFrame
        DataFrame with sequences that should be predicted
    param: dict

    Returns
    -------
    prediction: pd.DataFrame
        predictions to x_val; index corresponds to first value in each row
    """

    # initialise
    ind_to_drop = x_val.shape[0] % param['pred_len']  # last indices to drop
    x_val = x_val.copy().drop(x_val.index[-ind_to_drop:])

    # make predictions
    if param['features']:
        x_val_cons = make_consecutive(x_val, param)
        prediction = x_val_cons.copy().drop(param['features'], axis=1)
        for i in range(param['pred_len']):
            input_pred = [prediction.values[:, -param['seq_len']:, np.newaxis], x_val[param['features']].values[i::param['pred_len']]]  # creates multi-input and gets updated feature value
            prediction_new = pd.DataFrame(model.predict(input_pred), index=prediction.index, columns=[param['seq_len'] + i])  # one-point prediction is calculated
            prediction = pd.concat((prediction, prediction_new), axis=1)  # prediction is added to known values
    else:
        x_val_cons = make_consecutive(x_val, param)
        prediction = x_val_cons.copy()
        for i in range(param['pred_len']):
            prediction = pd.concat(
                (prediction, pd.DataFrame(model.predict(prediction.values[:, -param['seq_len']:, np.newaxis]), index=x_val_cons.index)),
                axis=1, ignore_index=True)

    # drop input data from DataFrame
    prediction = prediction.drop(axis=1, labels=range(param['seq_len']))

    return prediction


def analyze(predictions: pd.DataFrame, truth: pd.Series) -> Tuple[dict, dict]:
    """
    Statistical analysis of the accuracy of a model by comparing predictions of validation data and true data.

    Parameters
    ----------
    predictions: pd.DataFrame
        predictions in a DataFrame; index corresponds to first value in row
    truth: pd.Series
        Series with DateTimeIndex of true data; all indices of predictions must be present in truth

    Returns
    -------
    results_total: dict
        dictionary with statistical measures averaged over all predictions
    results_col: dict
        dictionary with statistical measures averaged over all columns (meaining average over all fist values, all second values etc.)
    """

    truth_df = pd.DataFrame(index=predictions.index, columns=predictions.columns)
    for index in truth_df.index:
        truth_df.loc[index] = truth[pd.date_range(start=index, periods=truth_df.shape[1], freq=truth.index.freq)].values
    dev = truth_df - predictions
    dev_np = np.array(dev, dtype=float)
    results_total = dict(info='average accuracy of all data points',
                         mae=np.mean(np.abs(dev_np)),
                         mse=np.mean(dev_np*dev_np),
                         std=np.std(dev_np),
                         var=np.var(dev_np),
                         max_dev=np.amax(dev_np),
                         min_dev=np.amin(dev_np),
                         norm_test_ks=stats.kstest(dev_np.flatten(), 'norm'),
                         norm_test_shapiro=stats.shapiro(dev_np),
                         # Shapiro-Wilk-Test auf Normalverteilung. Gibt Teststatistik und p-Wert aus.
                         # p-Wert nur für n<5000 aussagekräftig
                         mape=mape(np.array(truth_df), np.array(predictions))
                         )
    if dev_np.size >= 5000:  # chooses 5000 random values from dev for shapiro statistics
        results_total['norm_test_shapiro'] = stats.shapiro(np.random.choice(dev_np.flatten(), 5000, replace=False))

    results_col = dict(info='shows the average accuracy in dependency of the distance to the last known data point',
                       mae=list(np.mean(dev_np, axis=0)),
                       mse=list(np.mean(dev_np*dev_np, axis=0)),
                       std=list(np.std(dev_np, axis=0)),
                       var=list(np.var(dev_np, axis=0)),
                       max_dev=list(np.amax(dev_np, axis=0)),
                       min_dev=list(np.amin(dev_np, axis=0))
                       )
    # calculate norm tests for colummns
    norm_test_ks = []
    norm_test_shapiro = []
    if dev_np.shape[0] < 3:
        print('no norm tests in column possible for less than 3 columns')
    else:
        for i in range(dev_np.shape[1]):
            if dev_np.shape[1] < 5000:
                norm_test_ks.append(stats.kstest(dev_np[:, i], 'norm'))
                norm_test_shapiro.append(stats.shapiro(dev_np[:, i]))
            else:
                norm_test_ks.append(stats.kstest(dev_np[:, i], 'norm'))
                norm_test_shapiro.append(stats.shapiro(np.random.choice(dev_np[:, i], 5000, replace=False)))
    results_col['norm_test_ks'] = norm_test_ks
    results_col['norm_test_shapiro'] = norm_test_shapiro
    # calculate mape for columns
    mape_list = []
    for i in range(predictions.shape[1]):
        mape_list.append(mape(np.array(truth_df.iloc[:, i]), np.array(predictions.iloc[:, i])))

    return results_total, results_col


def make_consecutive(data: pd.DataFrame, param: Optional[dict]) -> pd.DataFrame:
    """
    Changes a standard x_val into a DataFrame of consecutive periods, meaning the upcoming predictions of x_val won't
    overlap so plotting is easier and no unnecessary predictions will be calculated
    Parameters
    ----------
    data: pd.DataFrame
        data in form of x_val, meaning each row is of seq_len and the index indicates the first point to be predicted
    param: dict

    Returns
    -------
    cons: pd.DataFrame
        data in form of x_val, now consecutive
    """
    cons = data.loc[::param['pred_len']]
    return cons


def create_features(data: pd.DataFrame,
                    param: dict,
                    temp: Optional[pd.Series] = None
                    ) -> pd.DataFrame:
    """
    Creates and returns features according to param['features'] to every DateTimeindex in data.
    Parameters
    ----------
    data: pd.DataFrame
    param: dict
    temp: pd.Series

    Returns
    -------
    features: DataFrame
    """
    if not isinstance(data, pd.DataFrame):
        raise TypeError('data must be pd.DataFrame')
    if not isinstance(param, dict):
        raise TypeError('param must be dictionary')

    if param['features']:
        feat = param['features']
        # create features and add them to DataFrame
        features = pd.DataFrame(index=data.index)
        if 'weekend' in feat:
            features = features.assign(weekend=np.floor(features.index.dayofweek/5))
            if 'weekend_inv' in feat:
                features = features.assign(weekend_inv=np.absolute(features['weekend']-1))
        if 'weekday' in feat:
            features = features.assign(weekday=features.index.dayofweek)
        # one-hot encoding for weekday; assuming that 'mon' always comes with all the other weekddays
        if 'mon' in feat:
            days = pd.DataFrame(0, index=features.index, columns=range(7))
            for index in days.index:
                days.loc[index][index.dayofweek] = 1
            days.columns = ['mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun']
            features = pd.concat([features, days], axis=1)
            del days
        if 'temp' in feat:
            if temp is None:
                raise ValueError('Temperature was not given to create_features()')
            features = features.assign(temp=temp[features.index])
    else:
        features = None
        warnings.warn('No features are specified in param!')
    return features


def create_multi_input(data: pd.DataFrame, param: Optional[dict]) -> list:
    """
    Creates list with multi input for NN.

    Parameters
    ----------
    data: pd.DataFrame
    param: dict

    Returns
    -------
    list
    """
    out = list()
    # input main
    out.append(data.drop(param['features'], axis=1).values[:, :, np.newaxis])
    # input features
    out.append(data[param['features']].values)
    return out


def load_temp(param: dict,
              temp_path: Optional[str] = None,
              temp_name: Optional[str] = None,
              freq: Optional[str] = None,
              index_col: Optional[int] = 0,
              delimiter: Optional[str] = ','
              ) -> Tuple[pd.Series, dict]:
    """
    Load temperature time series.

    Parameters
    ----------
    param: dict
    temp_path: str
    temp_name: str
    freq: str
    index_col: int
    delimiter: str

    Returns
    -------
    temp: pd.Series
    param: dict
    """

    if temp_path:
        param['temp_path'] = temp_path
    if temp_name:
        param['temp_name'] = temp_name

    df = pd.read_csv(os.path.join(param['temp_path'], param['temp_name']), index_col=index_col, header=None, delimiter=delimiter)
    df.index = pd.to_datetime(df.index)
    if not df.index.freq:
        if freq is None:
            df = df.asfreq(pd.infer_freq(df.index))
        else:
            df = df.asfreq(freq)
    df.index.name = 'index'
    df.columns = ['temp']

    return df['temp'], param


def save_model(model: Model, filename: Optional[str] = 'default', path: Optional[str] = ''):
    """Saves model to given path."""
    model.save(os.path.join(path, filename + '_model.h5'))


def reload_model(filename: str, path: Optional[str] = '') -> Model:
    """Reloads model from given path."""
    from tensorflow.keras.models import load_model
    model = load_model(os.path.join(path, filename + '.h5'))
    return model


def save_history(history, filename: Optional[str] = 'default', path: Optional[str] = ''):
    """Saves history file to csv"""
    pd.DataFrame(history.history).to_csv(os.path.join(path, filename + '_history.csv'))


def save_attributes(data, filename, path: Optional[str] = ''):
    """Save attributes in the order: max_data, min_data"""
    df = pd.DataFrame(data, index=['max_data', 'min_data'], columns=None)
    name = os.path.join(path, filename + '_attributes.csv')
    df.to_csv(name, header=False)


def save_param(param, filename: Optional[str] = 'default', path: Optional[str] = ''):
    """Save params used for training"""
    with open(os.path.join(path, filename + '_params.json'), 'w') as fp:
        json.dump(param, fp)


def save_results(results, filename: Optional[str] = 'default', path: Optional[str] = ''):
    """Save the accuracy of the results in json-file"""
    with open(os.path.join(path, filename + '_results.json'), 'w') as fp:
        json.dump(results, fp)


def save_final(model: Model,
               param: dict,
               results: dict,
               history,
               filename: Optional[str] = 'default',
               path: Optional[str] = ''):
    """Saves final model, param, results, and history."""
    if not os.path.exists(path):
        os.makedirs(path, exist_ok=True)
    save_model(model, filename, path)
    save_param(param, filename, path)
    save_results(results, filename, path)
    save_history(history, filename, path)
    plots.history_plot(history, show_plot=False, save_plot=True, filename=filename, path=path)


def load_json(path: str, filename: Optional[str] = None):
    """Loads json file from given path"""
    if not os.path.exists(path):
        raise ValueError('Path or file does not exist!')
    if filename is None:
        with open(os.path.join(path)) as f:
            data = json.load(f)
    else:
        with open(os.path.join(path, filename)) as f:
            data = json.load(f)
    return data


def mape(truth: np.array, prediction: np.array) -> np.array:
    """
    mean absolute procentage error; calculates the mape of the two arrays after flattening them;
    note that it makes a difference which array is declared as truth
    Parameters
    ----------
    truth: np.array
        array that is used as the true data for the calculation of the mape
    prediction: np.array

    Returns
    -------
    mape: float
        MAPE of the two arrays
    """
    mask = truth != 0
    mape_res = np.abs((truth[mask] - prediction[mask]) / truth[mask]).mean()
    return mape_res


def running_mean(x: np.array, N: int):
    """
    Calculates the running mean with window length N; the first and last (N-1)/2 values are set to the first / last value so
    the resulting array has the same lenght as the input
    Parameters
    ----------
    x: np.array
        data
    N: int

    Returns
    -------
    np.array
    """
    cumsum = np.cumsum(np.insert(x, 0, 0))
    runmean = (cumsum[N:] - cumsum[:-N]) / float(N)
    return np.append(np.insert(runmean, 0, int((N-1)/2)*[runmean[0]]), int(np.ceil((N-1)/2))*[runmean[-1]])


def sunshine_duration(start: str, end: str, lon: float, lat: float, elev: int = 0):
    """
    Calculates length of day for between given dates. Works only for Europe right now.
    Parameters
    ----------
    start: str
        start of period to be calculated in UTC
    end: str
        end of period to be calculated in UTC
    lon: float
        longtitude of location in degree
    lat: float
        lattitude of location in degree
    elev: int
        elevation of location

    Returns
    -------
    pd.Series
    """
    import ephem

    obs = ephem.Observer()
    obs.lon = str(lon)
    obs.lat = str(lat)
    obs.elev = elev
    obs.pressure = 0
    obs.horizon = '-0:34'

    dates = pd.date_range(start=start + ' 12:00:00', end=end + ' 12:00:00', freq='D')
    deltas = pd.Series(index=pd.to_datetime(dates.date))

    for date in dates:
        obs.date = date.to_pydatetime()
        deltas[date.date()] = (obs.next_setting(ephem.Sun()).datetime() - obs.previous_rising(
            ephem.Sun()).datetime()).total_seconds() / 3600
        # deltas.append((obs.next_setting(ephem.Sun()).datetime() - obs.previous_rising(ephem.Sun()).datetime()).total_seconds()/3600)

    return deltas
