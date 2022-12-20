import numpy as np
import pandas as pd
from typing import List, Optional, Union

from german_holidays import get_german_holiday_calendar


def create_features(
    data: pd.DataFrame,
    temp: pd.Series,
    fc_horizon: pd.Timedelta,
    lb_horizon: pd.Timedelta,
    verbose: Optional[bool] = True,
    shift_params: Optional[dict] = dict(),
    shifts: Optional[List[str]] = ["1d", "2d", "1w", "2w", "3w", "4w", "5w"],
    use_temp: Optional[bool] = False,
    use_weekend: Optional[bool] = False,
    use_weekday: Optional[bool] = False,
    use_hol: Optional[bool] = False,
    use_bridge: Optional[bool] = False,
    temp_shifts: Optional[bool] = False,
    shuffle: Optional[bool] = False,
) -> pd.DataFrame:
    """
    creates shift and statistical features

    Parameters
    ----------
    data: pd.DataFrame
        load in kW
    temp: pd.Series
        temperature in °C
    fc_horizon: pd.Timedelta
    lb_horizon: pd.Timedelta
    verbose: bool
        decides whether progress information is printed
    shift_params: dict
        parameters for feature extraction. All entries in  `shift_params` supersede the given inputs.
        Possible parameters: 'shifts', 'use_temp', 'use_weekend', 'use_hol', 'use_bridge', 'temp_shifts', 'shuffle'
    shifts: list
        load shifts that are created as features
    use_temp: bool
        should temperature be used as features
    use_weekend: bool
        should weekend information be used as feature
    use_weekday: bool
        should day of the week be used as feature
    use_hol: bool
        should holiday information be used as feature
    use_bridge: bool
        should bridge days information be used as feature
    temp_shifts: bool
        should corresponding temperature shift to `shifts` be created and used as feature
    shuffle: bool
        should samples be shuffled before training

    Returns
    -------
    data: pd.DataFrame
        dataframe with features as columns
    """

    ################
    # check inputs #
    ################

    if not isinstance(data, pd.DataFrame):
        raise TypeError("`data` must be of type pd.DataFrame!")
    if not isinstance(temp, pd.Series):
        raise TypeError("`temp` must be of type pd.Series!")
    if not isinstance(fc_horizon, pd.Timedelta) or not isinstance(
        lb_horizon, pd.Timedelta
    ):
        raise TypeError("horizons must be of type pd.Timedelta!")

    ##############
    # set params #
    ##############

    shift_params.setdefault("shifts", shifts)
    shift_params.setdefault("use_temp", use_temp)
    shift_params.setdefault("use_weekend", use_weekend)
    shift_params.setdefault("use_weekday", use_weekday)
    shift_params.setdefault("use_hol", use_hol)
    shift_params.setdefault("use_bridge", use_bridge)
    shift_params.setdefault("temp_shifts", temp_shifts)
    shift_params.setdefault("shuffle", shuffle)

    ##################################
    # explanation of used timeranges #
    ##################################

    # for a forecast of up to 40h we have to be careful to not use any data that is unavailable at runtime
    # that includes all the heat data inside of the 40h horizon
    # but as the temperature is available 5days in advance as a forecast, we may use this.
    # as the index in the DataFrame is corresponding to the target we have the following situation
    #
    #               |                  |                    |                  |
    #               |     past 36h     |    36h horizon     |    future 36h    |
    #               |                  |                    |                  |
    # ---------------------------------now------------------idx---------------------> time
    #
    #
    # the target for the forecast always is the load at idx
    #
    # to indicate where statistical features are calculated we use the suffixes 'past_', 'hor_', 'fut_'

    #####################
    # temporal features #
    #####################
    data = data.assign(
        dayofweek=data.index.dayofweek,
        hour=data.index.hour,
        isweekend=(data.index.dayofweek >= 5).astype(int),
    )
    if verbose is True:
        print("done with temporal features")

    #####################
    # external features #
    #####################
    # temperature
    data = data.merge(temp, how="left", left_index=True, right_index=True)
    if verbose is True:
        print("done with external features")

    ###########################
    # features from past time #
    ###########################
    ### shifts ###
    # (only allow shifts >= fc_horizon)
    shifts = [s for s in shifts if pd.Timedelta(s) >= fc_horizon]
    shifted = shift_past_events(data, data["load"], shift_params)
    ### corresponding temps ###
    shifted = shifted.join(
        [
            pd.Series(
                [temp.loc[ind - pd.Timedelta(t)] for ind in shifted.index],
                index=shifted.index,
                name=f"temp_{t}",
            )
            for t in shifts
        ]
    )

    data = data.merge(shifted, how="left", left_index=True, right_index=True)
    if verbose is True:
        print("done with shift-features")
    del shifted

    ### statistical features ###
    # use the range from 72h before the idx (exclusive) up to 36h before the idx (inclusive).
    # Example for the reasoning: It's noon and I want to forecast the load for the full next day. At the time of the
    # calculation of the forecast the 12h before the start of the forecast are not available.
    lookback_start = (
        lb_horizon + fc_horizon - pd.Timedelta("15min")
    )  # as endpoints are included when slicing a DatetimeIndex
    lookback_end = fc_horizon  # include the "now" (idx - 40h)
    ind_for_past = data.loc[
        data.index[0] + lookback_start + pd.Timedelta("15min") :
    ].index  # do not use the first 80h but add 15min so the "now" is included in past

    ### statistical features of past heatload
    # min/max and quartiles of last hours
    past_min = pd.Series(
        [
            np.min(data.loc[ind - lookback_start : ind - lookback_end, "load"])
            for ind in ind_for_past
        ],
        index=ind_for_past,
        name="past_min",
    )
    past_q25 = pd.Series(
        [
            np.nanquantile(
                a=data.loc[ind - lookback_start : ind - lookback_end, "load"], q=0.25
            )
            for ind in ind_for_past
        ],
        index=ind_for_past,
        name="past_q25",
    )
    past_q50 = pd.Series(
        [
            np.nanquantile(
                a=data.loc[ind - lookback_start : ind - lookback_end, "load"], q=0.50
            )
            for ind in ind_for_past
        ],
        index=ind_for_past,
        name="past_q50",
    )
    past_q75 = pd.Series(
        [
            np.nanquantile(
                a=data.loc[ind - lookback_start : ind - lookback_end, "load"], q=0.75
            )
            for ind in ind_for_past
        ],
        index=ind_for_past,
        name="past_q75",
    )
    past_max = pd.Series(
        [
            np.max(data.loc[ind - lookback_start : ind - lookback_end, "load"])
            for ind in ind_for_past
        ],
        index=ind_for_past,
        name="past_max",
    )
    # max divided by min of last hours
    past_ratio = past_max / past_min
    past_ratio.name = "past_minmax_ratio"
    # variance and spread of last hours
    past_var = pd.Series(
        [
            np.var(data.loc[ind - lookback_start : ind - lookback_end, "load"])
            for ind in ind_for_past
        ],
        index=ind_for_past,
        name="past_var",
    )
    past_diff = past_max - past_min
    past_diff.name = "past_diff"

    data = data.join(
        [
            past_min,
            past_q25,
            past_q50,
            past_q75,
            past_max,
            past_ratio,
            past_var,
            past_diff,
        ]
    )
    if verbose is True:
        print("done with statistical features of past load")
    del (
        past_min,
        past_q25,
        past_q50,
        past_q75,
        past_max,
        past_ratio,
        past_var,
        past_diff,
    )

    ## statistical features of past temperature
    # min/max and quartiles of last hours
    past_temp_min = pd.Series(
        [
            np.min(temp.loc[ind - lookback_start : ind - lookback_end])
            for ind in ind_for_past
        ],
        index=ind_for_past,
        name="past_temp_min",
    )
    past_temp_q25 = pd.Series(
        [
            np.nanquantile(
                a=temp.loc[ind - lookback_start : ind - lookback_end], q=0.25
            )
            for ind in ind_for_past
        ],
        index=ind_for_past,
        name="past_temp_q25",
    )
    past_temp_q50 = pd.Series(
        [
            np.nanquantile(
                a=temp.loc[ind - lookback_start : ind - lookback_end], q=0.50
            )
            for ind in ind_for_past
        ],
        index=ind_for_past,
        name="past_temp_q50",
    )
    past_temp_q75 = pd.Series(
        [
            np.nanquantile(
                a=temp.loc[ind - lookback_start : ind - lookback_end], q=0.75
            )
            for ind in ind_for_past
        ],
        index=ind_for_past,
        name="past_temp_q75",
    )
    past_temp_max = pd.Series(
        [
            np.max(temp.loc[ind - lookback_start : ind - lookback_end])
            for ind in ind_for_past
        ],
        index=ind_for_past,
        name="past_temp_max",
    )
    # max divided by min of last hours
    past_temp_ratio = (past_temp_max + 273.15) / (past_temp_min + 273.15)
    past_temp_ratio.name = "past_temp_minmax_ratio"
    # variance and spread of last hours
    past_temp_var = pd.Series(
        [
            np.var(temp.loc[ind - lookback_start : ind - lookback_end])
            for ind in ind_for_past
        ],
        index=ind_for_past,
        name="past_temp_var",
    )
    past_temp_diff = past_temp_max - past_temp_min
    past_temp_diff.name = "past_temp_diff"

    data = data.join(
        [
            past_temp_min,
            past_temp_q25,
            past_temp_q50,
            past_temp_q75,
            past_temp_max,
            past_temp_ratio,
            past_temp_var,
            past_temp_diff,
        ]
    )
    if verbose is True:
        print("done with statistical features of past temperature")
    del (
        past_temp_min,
        past_temp_q25,
        past_temp_q50,
        past_temp_q75,
        past_temp_max,
        past_temp_ratio,
        past_temp_var,
        past_temp_diff,
    )

    ####################################
    # features from within the horizon #
    ####################################
    # use the range from 36h before the idx (exclusive) up to the idx (inclusive)
    # within the forecast horizon no measurements are available, but forecasted temperatures that can be used.
    # Here it is assumed that the weather forecast is perfect.
    lookback = fc_horizon - pd.Timedelta(
        "15min"
    )  # as endpoints are included when slicing a DatetimeIndex and "now \notin hor"
    ind_for_hor = data.loc[
        data.index[0] + fc_horizon :
    ].index  # do not use the first 36h, as you can't look back 36h from these

    ### statistical features ###
    # min/max and quartiles of temp in last hours
    hor_temp_min = pd.Series(
        [np.min(temp.loc[ind - lookback : ind]) for ind in ind_for_hor],
        index=ind_for_hor,
        name="hor_temp_min",
    )
    hor_temp_q25 = pd.Series(
        [
            np.nanquantile(a=temp.loc[ind - lookback : ind], q=0.25)
            for ind in ind_for_hor
        ],
        index=ind_for_hor,
        name="hor_temp_q25",
    )
    hor_temp_q50 = pd.Series(
        [
            np.nanquantile(a=temp.loc[ind - lookback : ind], q=0.50)
            for ind in ind_for_hor
        ],
        index=ind_for_hor,
        name="hor_temp_q50",
    )
    hor_temp_q75 = pd.Series(
        [
            np.nanquantile(a=temp.loc[ind - lookback : ind], q=0.75)
            for ind in ind_for_hor
        ],
        index=ind_for_hor,
        name="hor_temp_q75",
    )
    hor_temp_max = pd.Series(
        [np.max(temp.loc[ind - lookback : ind]) for ind in ind_for_hor],
        index=ind_for_hor,
        name="hor_temp_max",
    )
    # max divided by min of temp in last 40h (in Kelvin)
    hor_temp_ratio = (hor_temp_max + 273.15) / (hor_temp_min + 273.15)
    hor_temp_ratio.name = "hor_temp_minmax_ratio"
    # variance and spread of temp in last hours
    hor_temp_var = pd.Series(
        [np.var(temp.loc[ind - lookback : ind]) for ind in ind_for_hor],
        index=ind_for_hor,
        name="hor_temp_var",
    )
    hor_temp_diff = hor_temp_max - hor_temp_min
    hor_temp_diff.name = "hor_temp_diff"

    data = data.join(
        [
            hor_temp_min,
            hor_temp_q25,
            hor_temp_q50,
            hor_temp_q75,
            hor_temp_max,
            hor_temp_ratio,
            hor_temp_var,
            hor_temp_diff,
        ]
    )
    if verbose is True:
        print("done with statistical features of horizon temperature")
    del (
        hor_temp_min,
        hor_temp_q25,
        hor_temp_q50,
        hor_temp_q75,
        hor_temp_max,
        hor_temp_ratio,
        hor_temp_var,
        hor_temp_diff,
    )

    return data


def shift_past_events(
    X: pd.DataFrame, y: pd.Series, params: dict, fillna: Optional[bool] = True
) -> pd.DataFrame:
    """
    Creates a DataFrame with past events based on params['shifts'] and with features.

    Parameters
    ----------
    X: DataFrame
        includes the temperature if temperature is used
    y: Series
        heat load
    params: dict
        includes the information which shifts and features are to be created
    fillna: bool
        whether NaNs in df should be replaced with mean of columns

    Returns
    -------
    df: DataFrame
        includes all features; has same index as y (only cut at the beginning by shifts)
    """

    if y.index.freq is None:
        raise Exception("Freq not defined")

    shifts_ = list(map(lambda i: pd.Timedelta(i) / y.index.freq, params["shifts"]))
    shifts_.sort()
    delta1 = int(shifts_[-1] - shifts_[0])
    delta2 = int(shifts_[-1])
    if any(map(lambda x: x - int(x) != 0.0, shifts_)):
        raise Exception("Freq. mismatch. Perform a resample inorder to align")
    df = pd.concat(
        [y.shift(int(s), freq=y.index.freq) for s in shifts_],
        axis=1,
        keys=params["shifts"],
    )
    if params["use_temp"]:
        df["temp"] = X["temp"][X.index.intersection(df.index)]
    if params["use_hol"]:
        hol = get_bank_holidays(
            start=df.index[0],
            end=df.index[-1],
            freq=df.index.freq,
            state="BY",
            return_mask=True,
            tz="UTC",
        )
        df["holiday"] = hol.astype(float)
        # df['non-holiday'] = (~hol).astype(float)

    if params["use_bridge"]:
        brd = get_bridgedays(
            start=df.index[0],
            end=df.index[-1],
            freq=df.index.freq,
            state="BY",
            return_mask=True,
            tz="UTC",
        )
        df["bridgedays"] = brd.astype(float)
    if params["use_weekday"]:
        df["weekday"] = df.index.dayofweek
    if params["use_weekend"]:
        df["weekend"] = np.floor(df.index.dayofweek / 5)
    if params["temp_shifts"]:
        shifts_ = list(
            map(lambda i: pd.Timedelta(i) / y.index.freq, params["temp_shifts"])
        )
        if any(map(lambda x: x - int(x) != 0.0, shifts_)):
            raise Exception("Freq. mismatch. Perform a resample inorder to align")
        df_ = pd.concat(
            [X["temp"].shift(int(s), freq=y.index.freq) for s in shifts_],
            axis=1,
            keys=["temp-" + s for s in params["temp_shifts"]],
        )
        df = pd.concat([df, df_.loc[df.index]], axis=1)
        del df_
    df = df.loc[df.index[delta1:-delta2]]
    if fillna is True:
        df = df.fillna(df.mean())

    return df


def get_bank_holidays(
    start: Union[str, pd.Timestamp],
    end: Union[str, pd.Timestamp],
    freq: Optional[str] = "D",
    state: Optional[str] = "BY",
    return_mask: Optional[bool] = True,
    tz: Optional[str] = "UTC",
    silent: Optional[bool] = False,
) -> pd.Series:
    """Generates holidays of given interval as mask or DatetimeIndex

    Parameters
    ----------
    start: datetime-like
        start of the timeseries including
    end: datetime-like
        end of the timeseries including
    freq: str
        frequency of the returned pd.Series; default='D'
    state: str
        Bundesland; default='BY'
    return_mask: bool
        If False, the function returns all bridge days as pandas.DatetimeIndex; default=True
    tz: str
        timezone; default=None
    silent: bool
        decides whether progress information is printed

    Returns
    -------
    holidays: pd.Series
        Returns a mask for the holidays; index is a pd.DateTimeIndex; values = True or False
    """

    start = pd.Timestamp(start)
    end = pd.Timestamp(end)
    if end < start:
        raise Exception("start has to be before end!")
    if not isinstance(return_mask, bool):
        raise TypeError("return_mask must be boolean!")

    cal_cls = get_german_holiday_calendar(state)
    cal = cal_cls()
    hld_ind = cal.holidays(start.date(), end.date())
    if return_mask:
        holidays = pd.Series(
            False, index=pd.date_range(start=start, end=end, freq=freq)
        )
        for index in hld_ind:
            holidays[index.strftime("%Y-%m-%d")] = True
    else:
        holidays = hld_ind
    # add timezone
    if isinstance(holidays, pd.Series):
        if str(holidays.index.tz) is not tz:
            if holidays.index.tz:
                if silent is False:
                    print(
                        "Converted " + str(holidays.index.tz) + "timezone to " + str(tz)
                    )
                holidays = holidays.tz_convert(tz)
            else:
                if silent is False:
                    print("Added timezone " + str(tz))
                holidays = holidays.tz_localize(tz)
    else:
        if str(holidays.tz) is not tz:
            if holidays.tz:
                if silent is False:
                    print("Converted " + str(holidays.tz) + "timezone to " + str(tz))
                holidays = holidays.tz_convert(tz)
            else:
                if silent is False:
                    print("Added timezone " + str(tz))
                holidays = holidays.tz_localize(tz)

    return holidays


def get_bridgedays(
    start: Union[str, pd.Timestamp],
    end: Union[str, pd.Timestamp],
    freq: Optional[str] = "D",
    state: Optional[str] = "BY",
    return_mask: Optional[bool] = True,
    tz: Optional[str] = "UTC",
    silent: Optional[bool] = False,
) -> pd.Series:
    """
    Generates bridge days of given interval as mask or DatetimeIndex

    Parameters
    ----------
    start: datetime-like
        start of the time series including
    end: datetime-like
        end of the time series including
    freq: str
        frequency of the returned pd.Series; default='D'
    state: str
        Bundesland; default='BY'
    return_mask: bool
        If False, the function returns all bridge days as pandas.DatetimeIndex; default=True
    tz: str
        timezone; default=None
    silent: bool
        decides whether progress information is printed

    Returns
    -------
    holidays: pd.Series
        Returns a mask for the holidays; index is a pd.DatetimeIndex; values = True or False
    """

    # initialise start with timezone
    start = pd.Timestamp(start)
    end = pd.Timestamp(end)
    if start.tz:
        start = start.tz_convert(tz)
    else:
        start = start.tz_localize(tz)
    if end.tz:
        end = end.tz_convert(tz)
    else:
        end = end.tz_localize(tz)

    if end < start:
        raise Exception("start has to be before end!")
    if not isinstance(return_mask, bool):
        raise TypeError("return_mask must be boolean!")

    brd_ind = pd.DatetimeIndex([])
    holidays = get_bank_holidays(
        start=start,
        end=end,
        freq=freq,
        return_mask=False,
        state=state,
        tz=tz,
        silent=silent,
    )
    for hld in holidays:
        # holiday is Thursday
        if hld.dayofweek == 3:
            # check if bridge day possible and if still in interval
            if (
                hld + pd.Timedelta("1D") not in holidays
                and hld + pd.Timedelta("1D") <= end
            ):
                brd_ind = brd_ind.append(pd.DatetimeIndex([hld + pd.Timedelta("1D")]))
        # holiday is Tuesday
        if hld.dayofweek == 1:
            # check if bridge day possible and if still in interval
            if hld - pd.Timedelta("1D") not in holidays and start <= (
                hld - pd.Timedelta("1D")
            ):
                brd_ind = brd_ind.append(pd.DatetimeIndex([hld - pd.Timedelta("1D")]))
    if return_mask:
        bridges = pd.Series(False, index=pd.date_range(start=start, end=end, freq=freq))
        for index in brd_ind:
            bridges[index.strftime("%Y-%m-%d")] = True
    else:
        bridges = brd_ind

    return bridges
