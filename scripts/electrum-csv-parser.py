import sys
import csv
from datetime import datetime

csv_file = sys.argv[1]
currency = sys.argv[2]

# fields
#   - transaction_hash
#   - label
#   - confirmations
#   - value
#   - fiat_value
#   - fee
#   - fiat_fee
#   - timestamp



with open(csv_file, encoding="utf8") as f:
    csvReader = csv.reader(f, delimiter=',', quotechar='"')

    next(csvReader)
    for row in csvReader:
        transaction_hash, label, confirmations, value, fiat_value, fee, fiat_fee, timestamp = row
        # date format: 2020-02-14 00:18:40
        date = datetime.strptime(timestamp.split(' ')[0], '%Y-%m-%d')

        value = float(value)
        fee = float(fee) if len(fee) > 0 else 0
        fiat_value = float(fiat_value) if len(fiat_value) > 0 else 0

        isSell = value < 0
        transTitle = "sell" if isSell else "buy"

        if fiat_value > 0:
            print('what is fiat value? ' . fiat_value)
            exit

        if isSell:
            ledgerMsg = date.strftime('%Y/%m/%d') + f''' * {currency} {transTitle}
    Crypt:{currency}      {value} {currency}
    Trade                 {value * -1} {currency}
    Crypt:{currency}     -{fee:.6f} {currency}
    Commission            {fee:.6f} {currency}
    Trade                -0 RUB
    Assets                0 RUB'''
        else:
            ledgerMsg = date.strftime('%Y/%m/%d') + f''' * {currency} {transTitle}
    Assets            -0 RUB
    Trade              0 RUB
    Trade              {value * -1} {currency}
    Crypt:{currency}   {value} {currency}'''

        ledgerMsg = ledgerMsg + '\n\n'

        print(ledgerMsg)
