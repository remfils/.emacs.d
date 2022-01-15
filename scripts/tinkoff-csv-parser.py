import sys
import csv
from datetime import datetime

csv_file = sys.argv[1]

def get_amount(amount):
    return float(amount.replace(',', '.'))

def get_expense_from_comment(comment):
    return 'Expenses'

def get_currency(currency):
    if currency == 'RUB':
        return ''
    return currency

with open(csv_file) as f:
    tinkreader = csv.reader(f, delimiter=';', quotechar='"')

    data = {}

    next(tinkreader)
    for row in tinkreader:
        # '01.10.2021 11:35:51', '01.10.2021', '*6364', 'OK', '-70,00', 'RUB', '-70,00', 'RUB', '', 'Рестораны', '5812', 'Kofe S Sovoi', '0,00', '0,00', '70,00'
        _, date, _, status, amount_1, cur_1, amount_2, cur_2, _, category, _, comment, _,_,_ = row

        is_money_transfer = False

        if cur_1 != cur_2:
            is_money_transfer = True

        if not is_money_transfer and amount_1 != amount_2:
            raise Exception('amounts dont match ' + amount_1 + ' != ' + amount_2)

        if status != 'OK':
            continue
            # raise Exception('status is ' + status)

        datetime = datetime.strptime(date, '%d.%m.%Y')

        if not datetime in data.keys():
            data[datetime] = []

        if is_money_transfer:
            comment = comment + f' / convert [{amount_1} {cur_1} -> {amount_2} {cur_2}]'
            data[datetime].append((amount_2, cur_2, category + ' / ' + comment))
        else:
            data[datetime].append((amount_1, cur_1, category + ' / ' + comment))

    for date in sorted(data):
        lines = data[date]
        ledger_msg = date.strftime('%Y/%m/%d') + ' * \n'
        
        for amount, currency, comment in lines:
            ledger_dt_amount = get_amount(amount)
            ledger_kt_amount = -1 * ledger_dt_amount
            ledger_expense = get_expense_from_comment(comment)
            ledger_currency = get_currency(currency)

            ledger_msg = ledger_msg + f'    {ledger_expense}    {ledger_kt_amount} {ledger_currency} ;; {comment}\n' + f'    Assets:TINK    {ledger_dt_amount} {ledger_currency}\n'
        print(ledger_msg)
