---
title: Transaction Add Screen (COTRN02)
---

The Transaction Add screen (COTRN02) allows users to input and confirm details for adding a new transaction to the system. Users can enter account or card numbers, transaction type, category, source, description, amount, dates, and merchant information, ensuring all necessary data is captured for the transaction.

## Screen Preview

```
Tran: CT02                      AWS Mainframe Modernization          Date: mm/dd/yy
Prog: COTRN02C                            CardDemo                   Time: hh:mm:ss

                                   Add Transaction

     Enter Acct #: ___________ (or) Card #: ________________

     ------------------------------------------------------

     Type CD: __  Category CD: ____  Source: __________

     Description: ____________________________________________________________

     Amount: ____________  Orig Date: __________  Proc Date: __________

     (-99999999.99)       (YYYY-MM-DD)           (YYYY-MM-DD)

     Merchant ID: ________  Merchant Name: ________________________________

     Merchant City: ___________________________  Merchant Zip: __________

     You are about to add this transaction. Please confirm: _ (Y/N)

[Error/Status Message Area]

ENTER=Continue  F3=Back  F4=Clear  F5=Copy Last Tran.
```

## Fields

### Transaction Name (TRNNAME)

This field is used to display the transaction name. It is a fixed-length field with a length of 4 characters. The field is non-editable and is displayed in blue color.

### Current Date (CURDATE)

This field displays the current date in the format 'mm/dd/yy'. It is a non-editable field with a length of 8 characters and is displayed in blue color.

### Program Name (PGMNAME)

This field is used to display the program name. It is a fixed-length field with a length of 8 characters. The field is non-editable and is displayed in blue color.

Associated document: <SwmLink doc-title="Adding a new transaction (COTRN02C)" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo" path="/.swm/adding-a-new-transaction-cotrn02c.i090jig3.sw.md">[Adding a new transaction (COTRN02C)](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v/docs/i090jig3)</SwmLink>

### Current Time (CURTIME)

This field displays the current time in the format 'hh:mm:ss'. It is a non-editable field with a length of 8 characters and is displayed in blue color.

### Account ID Input (ACTIDIN)

This field is used to input the account ID. It is an editable field with a length of 11 characters. The field is underlined, displayed in green color, and is initially blank.

### Card Number Input (CARDNIN)

This field is used to input the card number. It is an editable field with a length of 16 characters. The field is underlined, displayed in green color, and is initially blank.

### Transaction Type Code (TTYPCD)

This field is used to input the transaction type code. It is an editable field with a length of 2 characters. The field is underlined, displayed in green color, and is initially blank.

### Transaction Category Code (TCATCD)

This field is used to input the transaction category code. It is an editable field with a length of 4 characters. The field is underlined, displayed in green color, and is initially blank.

### Transaction Source (TRNSRC)

This field is used to input the transaction source. It is an editable field with a length of 10 characters. The field is underlined, displayed in green color, and is initially blank.

### Transaction Description (TDESC)

This field is used to input the transaction description. It is an editable field with a length of 60 characters. The field is underlined, displayed in green color, and is initially blank.

### Transaction Amount (TRNAMT)

This field is used to input the transaction amount. It is an editable field with a length of 12 characters. The field is underlined, displayed in green color, and is initially blank. The format for the amount is specified as '(-99999999.99)'.

### Original Date (TORIGDT)

This field is used to input the original date of the transaction. It is an editable field with a length of 10 characters. The field is underlined, displayed in green color, and is initially blank. The format for the date is specified as '(YYYY-MM-DD)'.

### Processing Date (TPROCDT)

This field is used to input the processing date of the transaction. It is an editable field with a length of 10 characters. The field is underlined, displayed in green color, and is initially blank. The format for the date is specified as '(YYYY-MM-DD)'.

### Merchant ID (MID)

This field is used to input the merchant ID. It is an editable field with a length of 9 characters. The field is underlined, displayed in green color, and is initially blank.

### Merchant Name (MNAME)

This field is used to input the merchant name. It is an editable field with a length of 30 characters. The field is underlined, displayed in green color, and is initially blank.

### Merchant City (MCITY)

This field is used to input the merchant city. It is an editable field with a length of 25 characters. The field is underlined, displayed in green color, and is initially blank.

### Merchant Zip (MZIP)

This field is used to input the merchant zip code. It is an editable field with a length of 10 characters. The field is underlined, displayed in green color, and is initially blank.

### Confirmation (CONFIRM)

This field is used to confirm the transaction. It is an editable field with a length of 1 character. The field is underlined, displayed in green color, and the user must input 'Y' for yes or 'N' for no.

&nbsp;

_This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human_

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
