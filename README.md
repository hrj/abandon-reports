# Abandon reports

This tool can be used to generate PDF reports for the XML data exported from [abandon](https://github.com/hrj/abandon).

# Screenshots

## Ledger report
Shows detailed transactions for each account.
![Ledger report](https://i.imgur.com/EXtvcjd.png)

## Balance report
Shows summary (opening balance, changes, closing balance) of each account.
![Balance report](https://i.imgur.com/FlPwvtx.png)

## Flow report
Shows the flow for each account.
![Flow report](https://i.imgur.com/G9flhod.png)


## CLI Interface

```
java -jar abandon-reports-0.9.jar <path for transactions.xml> <report start date> <regular expression>

 <path for transactions.xml>                     - Path to the file exported from abandon
 <report start date in dd-mmm-yyyy format>       - Transactions before this date are used to form the opening balance in the report
 <regular expression for account names to hide>  - Can be used to hide details of personal accounts, such as "Drawings"

```

The tool emits the following files in the current working directory: 
  * `detailed_balance_report.pdf` which contains the balance report
  * `report.pdf` which contains the detailed ledger report


## LICENSE

This source code is distributed under Apache License (See LICENSE file).
