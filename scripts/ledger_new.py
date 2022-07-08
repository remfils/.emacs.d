import subprocess
from subprocess import check_output
import csv
import pandas as pd
from datetime import datetime
from datetime import date
import matplotlib.pyplot as plt
import numpy as np

DEBUG=True

CSV_BAL_FORMAT = '%D,%(account),%(scrub(total))\\n'
CSV_REG_FORMAT = '%D,%(account),%(scrub(amount))\\n'

def dict_to_ledger_param_string(params):
    result = []

    for key, item in params.items():
        if isinstance(item, bool):
            if item:
                result.append(f'--{key}')
        else:
            result.append(f'--{key} "{item}"')
        
    return ' '.join(result)

def execute_ledger_command(mode, ledger_file, query, params):
    ledger_cmd = 'ledger {mode} -f "{ledger_file}" {params} {query}'

    param_string = dict_to_ledger_param_string(params)

    cmd = ledger_cmd.format(
        mode=mode,
        ledger_file=ledger_file,
        params=param_string,
        query=query
    )

    try:
        if DEBUG: print('[DEBUG] executing cmd: ' + cmd)

        data_raw = check_output(cmd, shell=True)
        data_raw = data_raw.decode('utf-8')
    except subprocess.CalledProcessError as e:
        print('[ERROR] Error when executing cmd: ' + str(e))
        data_raw = False

    result = [s.strip() for s in data_raw.split('\n')]
    result = list(filter(len, result))

    if DEBUG: print('[DEBUG] result of cmd:\n ' + str(result))

    return result


####################################################################################################
## dataframes
####################################################################################################

def create_date_label_total_dataframe(lines):
    dates = []
    labels = []
    totals = []

    for line in lines:
        if len(line) == 0:
            continue

        date, label, total = line.split(',')

        date = datetime.strptime(date, '%Y/%m/%d') # datetime format: 2022/05/15

        total = float(total.split(' ')[0])

        dates.append(date)
        labels.append(label)
        totals.append(total)

    return pd.DataFrame({'date': dates, 'label': labels, 'total': totals})


colorCounter = 0
def color_fn(key):
    global colorCounter

    if key == 'Coffee':
        return plt.cm.tab20c.colors[7]
    if key == 'Self':
        return plt.cm.tab20c.colors[5]
    if key == 'Junk food':
        return plt.cm.tab20c.colors[4]
    if key == 'Work':
        return plt.cm.tab20c.colors[1]
    if key == 'Expenses:Groceries':
        return plt.cm.tab20c.colors[0]
    if key == 'Rest':
        return plt.cm.tab20c.colors[19]
    if key.startswith('Invest'):
        return plt.cm.tab20.colors[5]
    if key.startswith('Black'):
        return plt.cm.tab20.colors[4]
    if key.startswith('Expenses:Home'):
        return plt.cm.tab20c.colors[14]
    if key.startswith('Nastya'):
        return plt.cm.tab20c.colors[13]
    if key == 'Trip':
        return plt.cm.tab20c.colors[12]
    print(colorCounter)
    if colorCounter >= len(plt.cm.Pastel1.colors):
        colorCounter = 0

    col = plt.cm.Pastel1.colors[colorCounter]
    colorCounter += 1
    return col


####################################################################################################
## mode functions
####################################################################################################

def expenses_plot(ledger_file, period):
    params = {
        'format': CSV_BAL_FORMAT,
        'period': period,
        'flat': True,
        'no-total': True
    }

    lines = execute_ledger_command('bal', ledger_file, '^Expenses', params)

    dt = create_date_label_total_dataframe(lines)

    dt = dt.sort_values(by=['label'])
    fig, (text_ax, ax) = plt.subplots(figsize =(16, 9), ncols=2, gridspec_kw={'width_ratios': [4, 10]})
    
    ax.barh(dt['label'], dt['total'], color='tab:red')

    # Add x, y gridlines
    ax.grid(b = True, color ='grey', linestyle ='-.', linewidth = 0.5, alpha = 0.2)
    ax.invert_yaxis()
    ax.set_xlabel("RUB")
    ax.set_title("Expenses in last month")

    # Add annotation to bars
    bar_x_offset = dt['total'].max() * 0.01
    for i in ax.patches:
        plt.text(i.get_width()+bar_x_offset, i.get_y()+0.5, str(round((i.get_width()), 2)), fontsize = 10, fontweight ='normal', color ='gray')

    # text_ax
    text_ax.set_yticklabels([])
    text_ax.set_xticklabels([])
    text_ax.xaxis.set_ticks_position('none')
    text_ax.yaxis.set_ticks_position('none')

    text_ax.set_title('Plot values')
    for s in ['top', 'bottom', 'left', 'right']:
        text_ax.spines[s].set_visible(False)

    total_expenses = dt['total'].sum()

    dt2 = dt.sort_values(by=['total', 'label'], ascending=[False, True])    
    top_5_expenses_label = dt2.head(5).to_string(header=False, columns=['label', 'total'], index=False)
    lines = [line.strip().replace('Expenses:', '').split() for line in top_5_expenses_label.split('\n')]
    max_length = max([len(l[0]) for l in lines])

    top_5_expenses_label = '\n'.join(['  ' + label.ljust(max_length, ' ') +' ' + str(total) for (label, total) in lines])
    
    current_date = date.today().strftime('%Y/%m/%d')
    msg = f'''Plot date: {current_date}

Total expenses:
    {total_expenses}

Top 5 expenses this month
{top_5_expenses_label}'''
    text_ax.text(0.05, 0.95, msg, fontsize=10, color='black', horizontalalignment='left', verticalalignment='top', family='monospace')

    fig.tight_layout()


def weekly_query(ledger_file, period, query):
    params = {
        'format': CSV_REG_FORMAT,
        'period': period,
        'flat': True,
        'no-total': True,
        'weekly': True
    }

    lines = execute_ledger_command('reg', ledger_file, query, params)

    dt = create_date_label_total_dataframe(lines)

    dt.groupby(['date', 'label']).sum().unstack().plot(kind='bar')

    # fig, ax = plt.subplots(figsize =(16, 9))

    # X = np.arange(len(dt['date'].unique()))

    # for label in :
    #     bar_values = {x:0 for x in keys}
    #     f = dt['label'] == label

    print(dt)


def master_pie_chart(ledger_file, period):
    params = {
        'format': CSV_BAL_FORMAT,
        'flat': True,
        'period': period,
        'no-total': True
    }

    if period == 'full-year':
        del params['period']
        params['begin'] = '01/02'


    lines = execute_ledger_command('bal', ledger_file, '^Expenses or ^Invest or ^Black', params)
    dt = create_date_label_total_dataframe(lines)
    del dt['date']

    dt = dt[~dt['label'].str.startswith(('Expenses:Crypt', 'EUR', 'USD'))]
    dt = dt[dt['total'] > 0]

    dt.loc[dt['label'].str.startswith('Expenses:Work'), 'label'] = 'Work'

    dt.loc[dt['label'] == 'Expenses:Self:Coffee', 'label'] = 'Coffee'
    dt.loc[dt['label'].str.startswith('Expenses:FastFood'), 'label'] = 'Junk food'
    dt.loc[dt['label'].str.startswith('Expenses:Deserts'), 'label'] = 'Junk food'
    dt.loc[dt['label'].str.startswith('Expenses:Self'), 'label'] = 'Self'
    dt.loc[dt['label'].str.startswith('Expenses:Trip'), 'label'] = 'Trip'
    dt.loc[dt['label'].str.startswith('Expenses:Nastya'), 'label'] = 'Nastya'

    dt.loc[(dt['label'].str.startswith('Expenses')) & (dt['total'] < 10000), 'label'] = 'Rest'

    dt = dt.groupby('label')['total'].sum().reset_index()

    dt.sort_values(by=['total'], inplace=True)

    fig, (text_ax, ax) = plt.subplots(figsize =(16, 9), ncols=2, gridspec_kw={'width_ratios': [4, 10]})
    ax.pie(dt['total'], labels=dt['label'], startangle=90, colors=[color_fn(key) for key in dt['label']])


    text_ax.set_yticklabels([])
    text_ax.set_xticklabels([])
    text_ax.xaxis.set_ticks_position('none')
    text_ax.yaxis.set_ticks_position('none')

    text_ax.set_title('Plot values')
    for s in ['top', 'bottom', 'left', 'right']:
        text_ax.spines[s].set_visible(False)

    total = dt['total'].sum()
    current_date = date.today().strftime('%Y/%m/%d')

    msg = f'''Plot date: {current_date}

Total:
    {total}

'''
    text_ax.text(0.05, 0.95, msg, fontsize=10, color='black', horizontalalignment='left', verticalalignment='top', family='monospace')

    fig.tight_layout()

####################################################################################################
## main
####################################################################################################

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Ledger cli')
    parser.add_argument('--file', help='ledger file name')
    parser.add_argument('--mode', help='some modes')
    parser.add_argument('--period', help='period string')
    args = parser.parse_args()

    if args.mode == 'expenses-plot':
        expenses_plot(args.file, args.period)
        plt.show()
    elif args.mode == 'weekly-groceries-plot':
        weekly_query(args.file, 'this year', 'Groceries')
        plt.show()
    elif args.mode == 'master-pie':
        master_pie_chart(args.file, args.period)
        plt.show()
    else:
        print('command not found')
