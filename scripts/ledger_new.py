import subprocess
from subprocess import check_output
import csv
import pandas as pd
from datetime import datetime
from datetime import date
import matplotlib.pyplot as plt
import numpy as np
import pickle
import os
from PIL import Image, ImageDraw, ImageFont

DEBUG = True

CSV_BAL_FORMAT = '%D,%(account),%(scrub(total))\\n'
CSV_REG_FORMAT = '%D,%(account),%(scrub(amount))\\n'
CSV_REG_FORMAT_WITH_COMMENT = '%D,%(account),%(scrub(amount)),%(payee)\\n'
CSV_BAL_EXPENSES_PERIOD_FORMAT = '%(partial_account),%(depth),%(total),%(account)\\n'

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

    if isinstance(query, (list, tuple)):
        query = '"' + '" or "'.join(query) + '"'
    else:
        query = '"' +str(query) + '"'
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

def add_figure_standart_footer(fig, ledger_file, **kargs):
    current_date = date.today().strftime('%Y/%m/%d')

    p = ', '.join([ '[' + str(k) + ':' + str(v) + ']' for k, v in kargs.items()])
    fig.text(0, 0.02, f'[дата: {current_date}] [файл: "{ledger_file}"] параметры: {p}', fontsize=8, color='gray', ha='left', va='top', family='monospace')


def appacc_show_tight_plot():
    plt.tight_layout()
    plt.show()
    
####################################################################################################
## dataframes
####################################################################################################

def create_date_label_total_dataframe(lines, include_comments=False, only_positive=False):
    dates = []
    labels = []
    totals = []
    comments = []

    for line in lines:
        if len(line) == 0:
            continue

        items = line.split(',')
        date = items[0]
        label = items[1]
        total = items[2]

        if include_comments:
            comment = items[3]
            comments.append(comment)

        date = datetime.strptime(date, '%Y/%m/%d') # datetime format: 2022/05/15

        total = float(total.split(' ')[0])

        dates.append(date)
        labels.append(label)
        totals.append(total)

    if len(totals) == 0:
        return None

    if not include_comments:
        result = pd.DataFrame({'date': dates, 'label': labels, 'total': totals})
    else:
        result = pd.DataFrame({'date': dates, 'label': labels, 'total': totals, 'comment': comments})

    if only_positive:
        if not (result['total'] > 0).any():
            result['total'] = abs(result['total'])
        else:
            result = result[result['total'] > 0]

    return result

colorCounter = 0
income_color_counter = 0.9
def color_fn(key):
    global colorCounter, income_color_counter

    if key.startswith('Income'):
        if income_color_counter <= 0:
           income_color_counter = 0.9
        col = plt.cm.YlGn(income_color_counter)
        income_color_counter -= 0.1
        return col

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
    if colorCounter >= len(plt.cm.Pastel1.colors):
        colorCounter = 0

    col = plt.cm.Pastel1.colors[colorCounter]
    colorCounter += 1
    return col


####################################################################################################
## mode functions
####################################################################################################

def full_year_monthly_table(ledger_file, period, query):
    pfile = 'full_year_monthly_table-3.pkl'
    # if os.path.exists(pfile):
    if False:
        with open(pfile, 'rb') as f:
            df = pickle.load(f)
        print('df from pickle')
    else:
        df = full_year_monthly_table__get_dataframe(ledger_file, period, query)
        with open(pfile, 'wb') as f:
            pickle.dump(df, f)

    full_year_monthly_table__render(df)

def full_year_monthly_table__render(df):
    monospace_font = ImageFont.truetype('fonts/Courier Prime.ttf', 14)

    top_padding = 20
    bottom_padding = 30
    row_height = 25
    row_padding = 1
    text_in_row_x_offset = 5
    text_in_row_y_offset = 7

    row_color_main = (240,240,240)
    row_color_sub = (230,230,230)
    
    period_column_width = 130

    label_column_width = 200

    image_width = 1920
    image_height = top_padding + (1 + len(df.index) + 1 + 1) * (row_height + 2 * row_padding) + bottom_padding
    
    img = Image.new('RGBA', (image_width, image_height), (255, 255, 255, 255))
    d = ImageDraw.Draw(img)

    # render cols
    list_non_period_cols = ['depth', 'account_name', 'order_index']

    bottom_position = top_padding + (1 + len(df.index) + 1 + 1) * (row_height + 2 * row_padding)

    x_pos = 10
    for i, (idx, row) in enumerate(df.iterrows()):
        y_pos = top_padding + (i + 1) * (row_height + 2 * row_padding)

        bgc = row_color_main if i % 2 != 0 else row_color_sub
        d.rectangle([(x_pos, y_pos), (image_width, y_pos + row_height)], fill=bgc)
        d.text((x_pos + text_in_row_x_offset, y_pos + text_in_row_y_offset), str(row['account_name']), font=monospace_font, fill=(0,0,0,255))

        col_idx = 0
        for column_name in df.columns:
            if column_name in list_non_period_cols:
                continue

            column_x = label_column_width + row_padding + col_idx * (period_column_width + row_padding)
            val = str(round(row[column_name], 2))
            (text_w, text_h) = monospace_font.getsize(val)
            d.text((column_x + (period_column_width - text_w) / 2, y_pos + text_in_row_y_offset), val, font=monospace_font, fill=(0,0,0))

            col_idx += 1
            

    
    column_y = top_padding + row_padding
    i = 0
    for column_name in df.columns:
        if column_name in list_non_period_cols:
            continue

        column_x = label_column_width + row_padding + i * (period_column_width + row_padding)

        (text_w, text_h) = monospace_font.getsize(column_name)
        d.text((column_x + (period_column_width - text_w) / 2, column_y + text_in_row_y_offset), column_name, font=monospace_font, fill=(0,0,0))

        d.line([(column_x, column_y),(column_x, bottom_position)], fill=(0,0,0))

        i += 1

    d.line([(column_x + period_column_width, column_y),(column_x + period_column_width, bottom_position)], fill=(0,0,0))
    
    
    
    img.save('image.png', 'PNG')

def full_year_monthly_table__get_dataframe(ledger_file, period, query):
    params = {
        'format': CSV_BAL_EXPENSES_PERIOD_FORMAT,
        'period': None,
        'no-total': True
    }

    periods = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 'July', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']

    top_level_categories = []
    category_children = {}
    category_data = {}
    
    for p in periods:
        params['period'] = '2022 ' + p # DEBUG
        lines = execute_ledger_command('bal', ledger_file, ['^Expenses:Nastya'], params)
        current_hierarchy = []
        current_parent = None
        for l in lines:
            (account, depth, total, full_account) = l.split(',')
            depth = int(depth)
            if 'RUB' not in total:
                continue
            total = float(total.replace('RUB', '').strip())

            accs = account.split(':')

            initial_depth = depth - len(accs) + 1
            for i, a in enumerate(accs):
                d = initial_depth + i
                if d == 1:
                    if account not in top_level_categories:
                        top_level_categories.append(a)
                    current_hierarchy = []
                else:
                    current_hierarchy = current_hierarchy[0:d-1]
                    current_parent = current_hierarchy[-1]
                    if current_parent not in category_children:
                        category_children[current_parent] = []
                
                    if a not in category_children[current_parent]:
                        category_children[current_parent].append(a)
                
                if a not in category_data:
                    category_data[a] = {
                        'depth': d,
                        'account_name': (' ' * ((d - 1) * 2)) + a
                    }
                category_data[a][p] = total
                
                current_hierarchy.append(a)

    def fill_order_index_recursive(order_index, children):
        idx = order_index
        for child_acc in children:
            category_data[child_acc]['order_index'] = idx
            print(child_acc, idx)
            idx = idx + 1

            for p in periods:
                if p not in category_data[child_acc]:
                    category_data[child_acc][p] = 0

            if child_acc in category_children:
                print('----------')
                print(child_acc, idx, category_children[child_acc])
                idx = fill_order_index_recursive(idx, category_children[child_acc])
                print('out: ', idx)

        return idx

    order_index = 0
    for top_level in top_level_categories:
        category_data[top_level]['order_index'] = order_index
        order_index = order_index + 1

        for p in periods:
            if p not in category_data[top_level]:
                category_data[p] = 0
        
        if top_level in category_children:
            print('fill first leve children', category_children[top_level], '\n++++++++++')
            order_index = fill_order_index_recursive(order_index, category_children[top_level])

    df = pd.DataFrame(category_data).T
    
    df.sort_values(['order_index'], inplace=True)

    return df


def monthly_bar(ledger_file, period, query):
    limit_data_count = 5
    
    params = {
        'format': CSV_REG_FORMAT,
        'period': period,
        'flat': True,
        'no-total': True,
        'monthly': True
    }

    lines = execute_ledger_command('reg', ledger_file, query, params)
    
    df = create_date_label_total_dataframe(lines, only_positive=True)

    df_wide = df.pivot(index='date', columns='label', values='total')
    df_wide.fillna(0, inplace=True)
    df_wide.sort_index(inplace=True)

    totals = df_wide.apply(sum).sort_values(ascending=False)
    if len(totals) > limit_data_count:
        significant_totals = totals[:limit_data_count]
        not_significant_totals = totals[limit_data_count+1:]
    else:
        significant_totals = totals
        not_significant_totals = []    
    
    accounts = significant_totals.index.tolist()

    fig, [ax, ax_txt] = plt.subplots(2, 1, gridspec_kw={'height_ratios': [12, 2]})

    ax.grid(b = True, color ='grey', linestyle ='-.', linewidth = 0.5, alpha = 0.2)
    
    ax.set_title(f'Доходы по месяцам [{query}]')
    
    bar_width = 0.5 / len(accounts);
    index = np.arange(len(df_wide.index))
    
    cmap = plt.get_cmap("tab20c")
    for idx, account in enumerate(accounts):
        account_amounts = df_wide[account].tolist()
        ax.bar(index + idx * bar_width, account_amounts, bar_width, color=cmap(idx), label=account)

    ax.legend()
    
    ax.set_xticks(index, map(lambda x: x.strftime('%B'), df_wide.index))
    
    print_text = ''
    print_text += 'Счет'.rjust(10, ' ') + ' | '
    print_text += ' | '.join(map(lambda x: x.center(25, ' '), accounts))
    print_text += ' |\n'
    
    print_text += 'total'.rjust(10, ' ') + ' | '
    print_text += ' | '.join(map(lambda x: str(x).center(25, ' '), significant_totals))
    print_text += ' |\n'

    print_text += 'max'.rjust(10, ' ') + ' | '
    print_text += ' | '.join(map(lambda x: str(x).center(25, ' '), df_wide[accounts].max()))
    print_text += ' |\n'
    
    print_text += 'min'.rjust(10, ' ') + ' | '
    print_text += ' | '.join(map(lambda x: str(x).center(25, ' '), df_wide[accounts][df_wide > 0].min()))
    print_text += ' |\n'
    
    print_text += 'mean'.rjust(10, ' ') + ' | '
    print_text += ' | '.join(map(lambda x: str(x).center(25, ' '), df_wide[accounts].mean()))
    print_text += ' |\n'
    
    print_text += 'median'.rjust(10, ' ') + ' | '
    print_text += ' | '.join(map(lambda x: str(x).center(25, ' '), df_wide[accounts].median()))
    print_text += ' |\n'
    
    ax_txt.text(0.5, 1, print_text, family='monospace', ha='center', va='top', size=10)
    ax_txt.axis('off')

    add_figure_standart_footer(fig, ledger_file, period=period, query=query)

def ballance_plot(ledger_file, period, query):
    params = {
        'format': CSV_BAL_FORMAT,
        'period': period,
        'flat': True,
        'no-total': True
    }

    if query is None:
        query = '^Expenses'

    lines = execute_ledger_command('bal', ledger_file, query, params)

    dt = create_date_label_total_dataframe(lines, only_positive=True)
    dt = dt.sort_values(by=['total', 'label'], ascending=[False, True])
    
    fig, (ax, ax_txt) = plt.subplots(figsize =(16, 9), ncols=2, gridspec_kw={'width_ratios': [10, 4]})
    
    ax.barh(dt['label'], dt['total'], color='tab:red')

    # Add x, y gridlines
    ax.grid(b = True, color ='grey', linestyle ='-.', linewidth = 0.5, alpha = 0.2)
    ax.invert_yaxis()
    ax.set_xlabel("RUB")

    ax.set_title(f'Баланс [{query}] на период [{period}]')
    
    # Add annotation to bars
    bar_x_offset = dt['total'].max() * 0.01
    for i in ax.patches:
        barWidth = i.get_width()
        barWidth = barWidth if barWidth > 0 else 0
        plt.text(barWidth + bar_x_offset, i.get_y()+0.5, str(round((i.get_width()), 2)), fontsize = 10, fontweight ='normal', color ='gray')

    # ax_txt
    ax_txt.axis('off')

    total_expenses = dt['total'].sum()

    dt['percent'] = round(dt['total'] / total_expenses, 2)
    max_label_length = max(dt['label'].str.len()) + 10

    expenses_label = '\n'.join(['    ' + r['label'].rjust(max_label_length, ' ') +' ' + str(r['total']).rjust(10, ' ') for idx, r in dt.iterrows()])

    msg = f'''Plot date
Сумма:
    {total_expenses}

Расходы
{expenses_label}
'''
    ax_txt.text(1, 0.95, msg, fontsize=10, color='black', horizontalalignment='right', verticalalignment='top', family='monospace')

    plt.subplots_adjust(left=0.2, bottom= 0.1, top=0.95)

    add_figure_standart_footer(fig, ledger_file, period=period, query=query)


# def weekly_query(ledger_file, period, query):
#     params = {
#         'format': CSV_REG_FORMAT,
#         'period': period,
#         'flat': True,
#         'no-total': True,
#         'weekly': True
#     }

#     lines = execute_ledger_command('reg', ledger_file, query, params)

#     dt = create_date_label_total_dataframe(lines)

#     dt.groupby(['date', 'label']).sum().unstack().plot(kind='bar')

#     # fig, ax = plt.subplots(figsize =(16, 9))

#     # X = np.arange(len(dt['date'].unique()))

#     # for label in :
#     #     bar_values = {x:0 for x in keys}
#     #     f = dt['label'] == label

#     print(dt)


def bal_pie_chart(ledger_file, period, query):
    params = {
        'format': CSV_BAL_FORMAT,
        'flat': True,
        'period': period,
        'no-total': True
    }

    if period == 'full-year':
        del params['period']
        params['begin'] = '01/02'

    if query is None:
        query = ["^Expenses", "^Invest", "^Black"]

    lines = execute_ledger_command('bal', ledger_file, query, params)
    dt = create_date_label_total_dataframe(lines, only_positive=True)
    del dt['date']

    dt = dt[~dt['label'].str.startswith(('Expenses:Crypt', 'EUR', 'USD'))]

    dt.loc[dt['label'].str.startswith('Expenses:Work'), 'label'] = 'Work'

    dt.loc[dt['label'] == 'Expenses:Self:Coffee', 'label'] = 'Coffee'
    dt.loc[dt['label'].str.startswith('Expenses:FastFood'), 'label'] = 'Junk food'
    dt.loc[dt['label'].str.startswith('Expenses:Deserts'), 'label'] = 'Junk food'
    dt.loc[dt['label'].str.startswith('Expenses:Self'), 'label'] = 'Self'
    dt.loc[dt['label'].str.startswith('Expenses:Trip'), 'label'] = 'Trip'
    dt.loc[dt['label'].str.startswith('Expenses:Nastya'), 'label'] = 'Nastya'

    dt = dt.groupby('label')['total'].sum().reset_index()

    dt.sort_values(by=['total'], inplace=True, ascending=False)

    total = dt['total'].sum()
    dt['percents'] = round(dt['total'] * 100 / total, 2)
    dt['total'] = round(dt['total'], 2)

    split_percent = 1

    is_detailed_plot_needed = (dt['percents'] < split_percent).any()

    if is_detailed_plot_needed:
        fig, (full_ax, small_ax, ax_txt) = plt.subplots(figsize =(16, 9), ncols=3, gridspec_kw={'width_ratios': [5, 5, 2]})

        full_dt = dt.copy()
        full_dt.loc[full_dt['percents'] < split_percent, 'label'] = 'Rest'
        full_dt = full_dt.pivot_table(['label', 'total'], 'label', aggfunc=sum)
        full_dt.sort_values(by=['total'], inplace=True, ascending=False)
        full_ax.pie(full_dt['total'], labels=full_dt.index, startangle=90, colors=[color_fn(key) for key in full_dt.index])

        full_ax.set_title('Основные счета')

        small_dt = dt[dt['percents'] < split_percent]
        small_dt.sort_values(by=['total'], inplace=True, ascending=False)
        small_ax.pie(small_dt['total'], labels=small_dt['label'], startangle=90, colors=[color_fn(key) for key in small_dt['label']])

        small_ax.set_title('Мелкие счета')
    else:
        fig, (full_ax, ax_txt) = plt.subplots(figsize =(16, 9), ncols=2, gridspec_kw={'width_ratios': [10, 2]})
        full_ax.pie(dt['total'], labels=dt['label'], startangle=90, colors=[color_fn(key) for key in dt['label']])
    
    ax_txt.axis('off')

    add_figure_standart_footer(fig, ledger_file, period=period, query=query)

    msg = ''
    for idx, row in dt.iterrows():
        msg += row['label'].ljust(30, ' ') + '\n' + str(row['total']).rjust(20, ' ') + '' +  ('(' + str(row['percents']) + '%)').rjust(8, ' ') + '\n'
    ax_txt.text(0, 0.5, msg, fontsize=8, color='black', ha='left', va='center', family='monospace')


def wallet_change_time_track(ledger_file, query, period):
    params = {
        'format': CSV_REG_FORMAT_WITH_COMMENT,
        'period': period,
        'flat': True,
        'no-total': True
    }

    lines = execute_ledger_command('reg', ledger_file, query, params)

    dt = create_date_label_total_dataframe(lines, include_comments=True);

    fig, (ax, ax_txt) = plt.subplots(figsize =(16, 9), nrows=2, ncols=1, gridspec_kw={'height_ratios': [9, 1]})

    dt.loc[0, 'result'] = dt.loc[0, 'total']
    for i in range(1, len(dt)):
        dt.loc[i, 'result'] = dt.loc[i - 1, 'result'] + dt.loc[i, 'total']
    
    ax.set_title(f'Динамика данных [{query}]')
    ax.set_xlabel('Дата')
    ax.set_ylabel('Валюта, руб.')
    
    ax.plot(dt['date'], dt['result'], linestyle='--', marker='o', color=color_fn(query))

    ax.grid(b = True, color ='grey', linestyle ='-.', linewidth = 0.5, alpha = 0.2)

    for i in range(0, len(dt)):
        ax.text(
            dt.loc[i, 'date'], dt.loc[i, 'result'] + 2000, dt.loc[i, 'comment'],
            fontsize=8,
            rotation=45, ha='left', va='bottom'
        )

    
    plt.ylim(ymin=0)

    ax_txt.axis('off')

    current_date = date.today().strftime('%Y/%m/%d')

    total_earned = dt.loc[len(dt)-1, 'result'] - dt.loc[0, 'result']

    msg = f'''
Оборот за период:
    {total_earned}'''
    ax_txt.text(0, 1, msg, fontsize=10, color='black', ha='left', va='top', family='monospace')

    add_figure_standart_footer(fig, ledger_file, period=period, query=query)

####################################################################################################
## main
####################################################################################################

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Ledger cli')
    parser.add_argument('--file', help='ledger file name')
    parser.add_argument('--mode', help='some modes')
    parser.add_argument('--period', help='period string')
    parser.add_argument('--account', help='account')
    args = parser.parse_args()

    show_plot = True

    if args.mode == 'expenses-bal':
        ballance_plot(args.file, args.period, args.account)
    elif args.mode == 'master-pie':
        bal_pie_chart(args.file, args.period, None)
    elif args.mode == 'income-pie':
        bal_pie_chart(args.file, args.period, '^Income')
    elif args.mode == 'income-monthly':
        monthly_bar(args.file, args.period, '^Income')
    elif args.mode == 'savings-monthly':
        monthly_bar(args.file, args.period, ['^Black Day', '^Expenses:Investments'])
    elif args.mode == 'change-track':
        wallet_change_time_track(args.file, args.account, args.period)
    elif args.mode == 'full-year-expenses-bal':
        target_dir = r'C:\Users\Vlad Pereskokov\Sync\\'
        year = '2022'
        month_names = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December']

        for m in month_names:
            period = year + ' ' + m
            ballance_plot(args.file, period, '^Expenses')
            show_plot = False
            plt.savefig(target_dir + period + '.jpg')
            plt.cla()
            plt.clf()
    elif args.mode == 'full-year-monthly-expenses-table':
        full_year_monthly_table(args.file, args.period, args.account)
        show_plot = False
    else:
        show_plot = False
        print('command not found')

    if show_plot:
        appacc_show_tight_plot()
