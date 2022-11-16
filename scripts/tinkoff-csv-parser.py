import sys
import csv
from datetime import datetime

csv_file = sys.argv[1]

def get_amount(amount):
    return float(amount.replace(',', '.'))

def get_expense_from_comment(comment, currency):
    if 'Пополнение брокерского счета' in comment: return 'Investments:' + currency

    if 'Перекрёсток' in comment: return 'Expenses:Groceries'
    if 'АТАК Супермаркеты' in comment: return 'Expenses:Groceries'
    if 'Provolstvennyj' in comment: return 'Expenses:Groceries'
    if 'Пятерочка' in comment: return 'Expenses:Groceries'
    if 'Дикси' in comment: return 'Expenses:Groceries'
    if 'ВкусВилл' in comment: return 'Expenses:Groceries'
    
    if 'Вкусно — и точка' in comment: return 'Expenses:FastFood'
    if 'Burger King' in comment: return 'Expenses:FastFood'
    if 'McDonald\'s' in comment: return 'Expenses:FastFood'
    if 'KFC' in comment: return 'Expenses:FastFood'
    if 'Додо Пицца' in comment: return 'Expenses:FastFood'

    if 'Kofejnya Dankin Donats' in comment: return 'Expenses:Deserts:Donuts'

    if 'пополнение счета' in comment: return 'Trade'

    return 'Expenses:UNK'

def get_currency(currency):
    if currency == 'RUB':
        return 'RUB'
    return currency

with open(csv_file, encoding="cp1251") as f:
    tinkreader = csv.reader(f, delimiter=';', quotechar='"')

    data = {}

    next(tinkreader)
    for row in tinkreader:
        # '01.10.2021 11:35:51', '01.10.2021', '*6364', 'OK', '-70,00', 'RUB', '-70,00', 'RUB', '', 'Рестораны', '5812', 'Kofe S Sovoi', '0,00', '0,00', '70,00'
        operation_date, date, _, status, amount_1, cur_1, amount_2, cur_2, _, category, _, comment, _,_,_ = row

        is_money_transfer = False

        if cur_1 != cur_2:
            is_money_transfer = True

        if not is_money_transfer and amount_1 != amount_2:
            raise Exception('amounts dont match ' + amount_1 + ' != ' + amount_2)

        if not (status == 'OK' or status == 'WAITING'):
            continue

        if status == 'WAITING':
            comment = ' [WAITING] ' + comment

        if len(date) < 3:
            datetime = datetime.strptime(operation_date.split()[0], '%d.%m.%Y')
        else:
            datetime = datetime.strptime(date, '%d.%m.%Y')

        if not datetime in data.keys():
            data[datetime] = []

        if is_money_transfer:
            comment = comment + f' / convert ({amount_1} {cur_1} -> {amount_2} {cur_2})'
            data[datetime].append((amount_2, cur_2, category + ' / ' + comment))
        else:
            data[datetime].append((amount_1, cur_1, category + ' / ' + comment))

    for date in sorted(data):
        lines = data[date]
        ledger_msg = date.strftime('%Y/%m/%d') + ' * \n'
        
        for amount, currency, comment in lines:
            ledger_asset_amount = get_amount(amount)
            ledger_expense_amount = -1 * ledger_asset_amount
            ledger_currency = get_currency(currency)
            ledger_expense = get_expense_from_comment(comment, ledger_currency)
            
            if ledger_asset_amount > 0:
                ledger_msg = ledger_msg + f'    {ledger_expense}    {ledger_expense_amount} {ledger_currency} ;; {comment}\n' + f'    Assets:TINK:{ledger_currency}    {ledger_asset_amount} {ledger_currency}\n'
            else:
                ledger_msg = ledger_msg + f'    Assets:TINK:{ledger_currency}    {ledger_asset_amount} {ledger_currency}\n' + f'    {ledger_expense}    {ledger_expense_amount} {ledger_currency} ;; {comment}\n'
        print(ledger_msg)
