#!/bin/python3

import pandas as pd
import datetime
import subprocess
from subprocess import check_output
from dateutil import rrule
import csv
from io import StringIO
import matplotlib.pyplot as plt
from matplotlib import gridspec
import numpy as np
from collections import defaultdict
import locale
import dateutil.parser
locale.setlocale(locale.LC_TIME, 'ru_RU.UTF-8')

VERBOSE=True
CSV_BAL_FORMAT = '%D,%(account),%(quantity(scrub(total)))\n'

##################################################
# FNs
##################################################


def to_ledger_date(date):
    """convert date to ledger date format"""
    if isinstance(date, str):
        return date
    return date.strftime('%Y/%m/%d')


def clean_ledger_wallet(wallet_name):
    return wallet_name.strip('"')


def clean_ledger_date(date_str) -> datetime.datetime:
    if isinstance(date_str, str):
        return dateutil.parser.isoparse(date_str)
    return date_str


def clean_float(fl: str):
    if not fl:
        return 0
    
    return float(fl.strip('"'))


def parse_label_and_total(string: str) -> str:
    date = datetime.datetime.now().strftime('%Y/%m/%d')
    
    try:
        label, data = string.split(',')
    except ValueError:
        label = string
        data = 0

    return (date, label, data)


def parse_ballance_tree(string: str) -> (str, str):
    date = datetime.datetime.now().strftime('%Y/%m/%d')

    try:
        data, label = string.strip(' ').split('RUB')
    except ValueError:
        label = string
        data = '0'

    label = label.strip()
    data = data.strip()

    return date, label, data


def parse_csv_bal(string: str) -> (str, str):
    try:
        date, label, total = string.split(',')
    except ValueError:
        date = datetime.datetime.now()
        label = string
        total = '0'

    if  isinstance(date, str):
        date = date.strip()
    
    label = label.strip()
    total = total.strip()

    return date, label, total


##################################################
#  MODELS
##################################################

class LedgerModel:
    ledger_file = 'ledger-2020.sm'

    ledger_cmd = 'ledger {mode} -f "{ledger_file}" {params} {query}'

    def __init__(self, ledger_file=None):
        if ledger_file:
            self.ledger_file = ledger_file

    def execute(self, queries=['Assets'], params={}):
        results = []

        for q in queries:
            r = self.execute_command('bal', q, params, parse_ballance_tree)

            results.append(r)

        return results
            
    def prepare_cmd(self, cmd, mode: str, query: str, params: dict) -> str:
        param_string = self.dict_to_ledger_param_string(params)
        
        return cmd.format(
            ledger_file=self.ledger_file,
            mode=mode,
            params=param_string,
            query=query
        )

    def dict_to_ledger_param_string(self, params: dict):
        result = []

        for key, item in params.items():
            result.append(f'--{key} "{item}"')

        return ' '.join(result)

    def execute_command(self, mode, query, params, data_parse_fn=None):
        cmd = self.prepare_cmd(self.ledger_cmd, mode, query, params)

        try:
            data_raw = check_output(cmd, shell=True)
        except subprocess.CalledProcessError as e:
            print('Error when executing cmd: ' + str(e))
            return None

        if not data_parse_fn:
            data_parse_fn = parse_label_and_total

        data_raw = data_raw.decode('utf-8')

        return self.parse_cmd_output_to_dataframe(data_raw, data_parse_fn)

    def parse_cmd_output_to_dataframe(self, data_raw, data_parse_fn):
        dates = []
        labels = []
        totals = []
        for line in data_raw.split('\n'):
            if len(line) == 0:
                continue

            date, label, total = data_parse_fn(line)

            dates.append(
                clean_ledger_date(date)
            )
            
            labels.append(
                clean_ledger_wallet(label)
            )
            
            totals.append(
                clean_float(total)
            )
        
        return pd.DataFrame({'dates': dates, 'labels': labels, 'totals': totals})


class LedgerBalModel(LedgerModel):

    def execute(self, queries=['Assets'], params={}):

        params['format'] = CSV_BAL_FORMAT

        results = []

        for q in queries:
            r = self.execute_command('bal', q, params, parse_csv_bal)

            results.append(r)

        return results


class LedgerCSVModel(LedgerModel):

    def execute(self, queries=['Assets'], params={}):

        results = []

        for q in queries:
            r = self.execute_command('csv', q, params)

            results.append(r)

        return results

    def parse_cmd_output_to_dataframe(self, data_raw, data_parse_fn):
        rows = csv.reader(StringIO(data_raw), delimiter=',')

        dates = []
        labels = []
        totals = []
        comments = []

        for row in rows:
            date = clean_ledger_date(row[0])
            comment = row[2]
            label = clean_ledger_wallet(row[3])
            total = clean_float(row[5])

            dates.append(date)
            labels.append(label)
            totals.append(total)
            comments.append(comment)

        return pd.DataFrame({'dates': dates, 'labels': labels, 'totals': totals, 'comments': comments})

####################################################################################################
## graphs
####################################################################################################


def autolabel(rects, xpos='center', ax=None):
    """
    Attach a text label above each bar in *rects*, displaying its height.

    ,*xpos* indicates which side to place the text w.r.t. the center of
    the bar. It can be one of the following {'center', 'right', 'left'}.
    """

    xpos = xpos.lower()  # normalize the case of the parameter
    ha = {'center': 'center', 'right': 'left', 'left': 'right'}
    offset = {'center': 0.5, 'right': 0.57, 'left': 0.43}  # x_txt = x + w*off

    for rect in rects:
        height = rect.get_height()
        ax.text(rect.get_x() + rect.get_width()*offset[xpos], 1.01*height,
                '{0:.2f}'.format(height), ha=ha[xpos], va='bottom')


def init_plot(**kwargs):
    global VERBOSE
    
    fig_size = plt.rcParams["figure.figsize"]
    fig_size[0] = 15
    fig_size[1] = 5
    plt.rcParams["figure.figsize"] = fig_size
    plt.rcParams['figure.facecolor'] = 'white'

    fig, ax = plt.subplots()

    for key, val in kwargs.items():
        if key == 'title':
            plt.title(val)
        elif key == 'verbose':
            VERBOSE = val
        elif key == 'ax':
            fig = None
            ax = val
        
    return fig, ax


def post_init_plot(**kwargs):
    for key, val in kwargs.items():
        if key == 'legend':
            plt.legend(val)


def date_group_stacked_plot(df, **kwargs):
    gs = gridspec.GridSpec(1, 2, width_ratios=[4, 1])

    ##################################################
    # plot
    ##################################################
    
    ax = plt.subplot(gs[0])

    plot_data = defaultdict(list)

    wallets = df['labels'].unique()

    group = df.groupby(['dates'])

    bottom = []
    plot_labels = []
    grand_total = 0

    for name, gr in group:
        bottom.append(0)
        total = 0
        for label in wallets:
            val_row = gr[gr['labels'] == label]

            if len(val_row) == 0:
                plot_data[label].append(0)
            else:
                t = val_row.iloc[0]['totals'] * -1
                plot_data[label].append(t)
                total += t

        grand_total += total

        plot_labels.append(
            name.strftime('%b') + f'\n{total:0.2f}'
        )

    total_count = len(bottom)
    if total_count != 0:
        grand_mean = grand_total / len(bottom)
    else:
        grand_mean = grand_total

    idx = np.arange(1, len(bottom)+1)

    cmap=plt.get_cmap('tab20c')
    i = 0
    for k, yvals in plot_data.items():
        ax.bar(idx, yvals, 0.4, bottom=bottom, color=cmap(i))

        bottom = np.add(bottom, yvals)

        i += 1

    plt.xticks(idx, plot_labels)

    plt.legend(wallets)

    ##################################################
    # text data
    ##################################################
    fill = max(map(lambda x: len(x), plot_data.keys()))

    means = []
    for k, yvals in plot_data.items():
        avg = np.average(yvals)
        means.append(f' - {k: <{fill}} {avg:.2f}')
    means = '\n'.join(means)

    ax2 = plt.subplot(gs[1])

    ax2.axis('off')

    ax2.text(0, 1, f'''
Итого
  {grand_total:.2f}

Общая средняя
  {grand_mean:.2f}

Индивидуальные средние:

{means}
''', va='top')


def wallet_stacked_plot(df, **kwargs):
    fig, ax = init_plot(**kwargs)

    labels = form_second_level_wallet_stacked_labels(df)

    idx = 1
    used_labels = []
    for label in labels.keys():
        if len(label) == 0:
            continue
        
        if ':' in label:
            cols = df[df['labels'].str.startswith(label)]
        else:
            cols = df[df['labels'] == label]

        max_level = labels[label]
        
        bottom = 0
        for index, row in cols.iterrows():

            row_label = row['labels']
            total = row['totals']

            if max_level != 1:
                prefix, name = split_by_level(row_label, 2)

                if len(name.split(':')) != max_level:
                    continue

            used_labels.append(row_label + "(" + str(total) + " RUB)")
            ax.bar(idx, total, 1, bottom=bottom)

            bottom = bottom + total

        idx += 1

    plt.xticks(np.arange(1, idx), list(labels.keys()))

    plt.legend(used_labels)


def form_second_level_wallet_stacked_labels(df):
    result = {}
    
    for label in list(df['labels']):

        prefix, name = split_by_level(label, 2)

        if prefix not in result.keys():
            result[prefix] = len(name.split(':'))
        else:
            level = result[prefix]

            result[prefix] = max(
                len(name.split(':')),
                level
            )

    return result


def split_by_level(label, level):
    sublevels = label.split(':')

    return (
        ':'.join(sublevels[:level]),
        ':'.join(sublevels[level:])
    )


def plot_accumulation_graph(dfs, **kwargs):
    fig, ax = init_plot(**kwargs)

    for df in dfs:
    
        dates = list(df['dates'])

        accumulation = []
        current_total = 0
        for total in list(df['totals']):
            current_total += total
            accumulation.append(current_total)

        ax.plot(
            dates,
            accumulation
        )

        current_total = 0
        for idx, row in df.iterrows():
            current_total += row['totals']

            if not VERBOSE:
                continue

            color='green'
            if row['totals'] < 0:
                color='red'
        
            plt.text(
                row['dates'], current_total,
                row['comments'] + "(" + str(row['totals']) + ")",
                size=8, color=color
            )

    post_init_plot(**kwargs)

    return ax



if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Ledger cli')
    parser.add_argument('file', help='ledger file name')
    parser.add_argument('mode', help='some mode')

    args = parser.parse_args()

    model = LedgerBalModel(args.file)
    params = {
        'period': 'last month'
    }

    df = model.execute(
        ['Expenses'],
        params
    )[0]

    wallet_stacked_plot(df)
    plt.show()

    
