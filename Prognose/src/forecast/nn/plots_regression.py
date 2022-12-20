
##### plots #####


import os
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd


# plot results multiple
def plot_results_multiple(predictions, truth, param, path=None, filename=None, compute_time=999999):
    if not param['show_plot'] and not param['save_plot']:
        pass
    else:
        fig = plt.figure(facecolor='white')
        ax = fig.add_subplot(111)
        ax.plot(truth[param['seq_len']:len(predictions)*param['pred_len']+param['seq_len']], label='true data')
        for index in predictions.index:
            s = predictions.loc[index].copy()
            s.index = pd.date_range(start=index, periods=param['pred_len'], freq=truth.index.freq)
            plt.plot(s, label='prediction')
            del s
        plt.legend()

        title = param['name'] + ', CNN: ' + str(param['useCNN']) + '\nbatch size: ' + str(
            param['batch_size']) + ', seq_len:' + str(param['seq_len']) + ', pred_len: ' + str(param['pred_len'])
        if param['callback_period']:
            title = title + '\nepochs: {}/{}'.format(curr_epoch, param['epochs']) + ', computation time: ' + str(
                int(compute_time)) + 's'
        else:
            title = title + '\nepochs: {}'.format(param['epochs']) + ', computation time: ' + str(
                int(compute_time)) + 's'
        plt.title(title)

        if param['save_plot']:
            if not path or not filename:
                raise Exception('Path or filename is not defined!')
            plt.savefig(os.path.join(path, filename + '_pred.png'), dpi=300)
        if param['callback_period']:
            plt.close()
        else:
            if param['show_plot']:
                plt.show()
            else:
                plt.close()
        del fig, ax, title


def history_plot(history, show_plot=True, save_plot=False, filename=None, path=''):
    plt.plot(history.history['loss'])
    plt.plot(history.history['val_loss'])
    plt.legend(['loss', 'val_loss'])
    if save_plot:
        plt.savefig(os.path.join(path, filename + '_history.png'), dpi=300)
    if show_plot:
        plt.show()
    else:
        plt.close()

