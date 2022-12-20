import numpy as np
import pandas as pd
from warnings import warn
import pickle
import xgboost as xgb


class XGBoostModel_CV:
    """
    Model that uses the XGBoost algorithm for predicting. Cross-validation during training is possible.
    """
    def __init__(self, name: str = "", nfolds: int = 4):
        self.name = "XGB_" + name
        self.xg = None
        self.nfolds = nfolds

    def fit(
        self,
        X: pd.DataFrame,
        y: pd.Series,
        verbose: bool = False,
        max_no_rounds: int = 50,
        cross_val: bool = True,
        depth_range: range = range(1, 20),
        subsample_range: np.ndarray = np.arange(0.1, 1.1, 0.1),
        eta_range: np.ndarray = np.arange(0.05, 0.7, 0.05)
    ):
        """
        Fits the XGBoost model with X and y.

        Parameters
        ----------
        X: pd.DataFrame
            includes all features
        y: pd.Series
            load in kW
        verbose: bool
            if True, the final parameters are printed
        max_no_rounds: int
            Maximum number of training rounds
        cross_val: bool
            if True, cross validation is applied during fitting
        depth_range: range
            range of possible tree depths in cross validation
        subsample_range: np.ndarray
            tested subsample ratios in cross validation. Values must fulfill 0 < value <= 1
        eta_range: np.ndarray
            tested learning rates in cross validation. Values must fulfill 0 < value <= 1

        Returns
        -------

        """

        ########################
        # test input variables #
        ########################

        if not isinstance(X, pd.DataFrame):
            raise TypeError("`X` needs to be of type: <pd.DataFrame>")
        if not isinstance(X.index, pd.DatetimeIndex):
            raise TypeError("`X.index` needs to be of type: <pd.DatetimeIndex>")
        if not isinstance(y, pd.Series):
            raise TypeError("`y` needs to be of type <pd.Series>")
        if not isinstance(y.index, pd.DatetimeIndex):
            raise TypeError("`y.index` needs to be of type: <pd.DatetimeIndex>")
        if X.index.tz.tzname(str) != "UTC":
            raise ValueError("Data `X.index` has to be UTC")
        if y.index.tz.tzname(str) != "UTC":
            raise ValueError("Data `y.index` has to be UTC")
        if not (X.index == y.index).all():
            raise ValueError("Indices of X and y must be the same")

        ###############
        # format data #
        ###############
        dtrain = xgb.DMatrix(X, label=y)

        ##########################
        # model cross validation #
        ##########################
        if cross_val is True:
            ## max_depth ##
            grid_results = pd.DataFrame(np.NaN, index=depth_range, columns=["no_rounds", "error"])
            for max_depth_it in grid_results.index:
                params = dict(max_depth=max_depth_it, objective="reg:squarederror")
                cv_results = xgb.cv(
                    params,
                    dtrain,
                    max_no_rounds,
                    nfold=4,
                    early_stopping_rounds=3,
                    shuffle=True,
                    metrics="mae",
                )
                grid_results["no_rounds"][max_depth_it] = cv_results["test-mae-mean"].idxmin()
                grid_results["error"][max_depth_it] = cv_results["test-mae-mean"].min()
            max_depth = grid_results.idxmin()["error"]
            del grid_results, params, cv_results

            ## subsample ##
            grid_results = pd.DataFrame(np.NaN, index=subsample_range, columns=["no_rounds", "error"])
            for subsample_it in grid_results.index:
                params = dict(
                    max_depth=max_depth,
                    subsample=subsample_it,
                    objective="reg:squarederror",
                )
                cv_results = xgb.cv(
                    params,
                    dtrain,
                    max_no_rounds,
                    nfold=4,
                    early_stopping_rounds=3,
                    shuffle=True,
                    metrics="mae",
                )
                grid_results["no_rounds"][subsample_it] = cv_results["test-mae-mean"].idxmin()
                grid_results["error"][subsample_it] = cv_results["test-mae-mean"].min()
            subsample = grid_results.idxmin()["error"]
            del grid_results, params, cv_results

            ## learning rate ##
            grid_results = pd.DataFrame(np.NaN, index=eta_range, columns=["no_rounds", "error"])
            for eta_it in grid_results.index:
                params = dict(
                    max_depth=max_depth,
                    subsample=subsample,
                    eta=eta_it,
                    objective="reg:squarederror",
                )
                cv_results = xgb.cv(
                    params,
                    dtrain,
                    max_no_rounds,
                    nfold=4,
                    early_stopping_rounds=3,
                    shuffle=True,
                    metrics="mae",
                )
                grid_results["no_rounds"][eta_it] = cv_results["test-mae-mean"].idxmin()
                grid_results["error"][eta_it] = cv_results["test-mae-mean"].min()
            eta = grid_results.idxmin()["error"]
            del grid_results, params, cv_results

            ## max_depth 2nd time ##
            grid_results = pd.DataFrame(np.NaN, index=depth_range, columns=["no_rounds", "error"])
            for max_depth_it in grid_results.index:
                params = dict(
                    max_depth=max_depth_it,
                    subsample=subsample,
                    eta=eta,
                    objective="reg:squarederror",
                )
                cv_results = xgb.cv(
                    params,
                    dtrain,
                    max_no_rounds,
                    nfold=4,
                    early_stopping_rounds=3,
                    shuffle=True,
                    metrics="mae",
                )
                grid_results["no_rounds"][max_depth_it] = cv_results["test-mae-mean"].idxmin()
                grid_results["error"][max_depth_it] = cv_results["test-mae-mean"].min()
            max_depth = grid_results.idxmin()["error"]
            del grid_results, params, cv_results

            ## no_rounds ##
            params = dict(
                max_depth=max_depth,
                subsample=subsample,
                eta=eta,
                objective="reg:squarederror",
            )
            if verbose is True:
                print("Final parameters after grid search:")
                print(params)
            cv_results = xgb.cv(
                params,
                dtrain,
                max_no_rounds,
                nfold=4,
                early_stopping_rounds=3,
                shuffle=True,
                metrics="mae",
            )
            no_rounds = cv_results.index[-1] + 1
            if max_no_rounds == no_rounds:
                warn("Cross validation for model " + self.name + " stopped before minimum was reached")
            del cv_results

        else:
            ## default values if no cross validation ##
            params = dict(
                eta=0.3,
                subsample=1,
                max_depth=6,
                objective="reg:squarederror"
            )
            no_rounds = max_no_rounds

        #####################################
        # train model with found parameters #
        #####################################
        self.xg = xgb.train(params, dtrain, num_boost_round=no_rounds)

    def predict(self, X: pd.DataFrame) -> pd.Series:
        """

        Parameters
        ----------
        X: pd.DataFrame
            dataframe that includes all features

        Returns
        -------
        pd.Series
        """

        # check inputs
        if not isinstance(X, pd.DataFrame):
            raise TypeError("`X` needs to be of type: <pd.DataFrame>")
        if not isinstance(X.index, pd.DatetimeIndex):
            raise TypeError("`X.index` needs to be of type: <pd.DatetimeIndex>")
        if X.index[0].tzname() != "UTC":
            raise ValueError("Data `X.index` has to be UTC")
        if self.xg is None:
            raise RuntimeError("The model has not been fitted yet")

        # format data
        ddata = xgb.DMatrix(X)
        # make prediction
        pred = self.xg.predict(ddata)
        return pd.Series(pred, index=X.index).clip(0)

    def save_trained_model(self, save_path: str):
        """
        Saves trained model to pickle file

        Parameters
        ----------
        save_path: str
            path where .pkl is saved

        Returns
        -------

        """
        if not save_path.endswith('.pkl'):
            warn('File name should end with ".pkl"!')
        pickle.dump(self, open(save_path, 'wb'))

    def load_existing_model(self, load_path: str):
        """
        Loads existing model from pickle file

        Parameters
        ----------
        load_path: str
            path where model is saved

        Returns
        -------

        """
        _model = pickle.load(open(load_path, 'rb'))
        self.xg = _model.xg
        self.name = _model.name
        self.nfolds = _model.nfolds


