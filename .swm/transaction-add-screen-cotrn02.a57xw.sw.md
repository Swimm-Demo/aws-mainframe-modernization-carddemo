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

### Current Date (CURDATE)

### Transaction Name (TRNNAME)

The Transaction Name field is a 4-character field displayed in blue color. It is a non-editable field, as indicated by the `ASKIP` attribute.

### Program Name (PGMNAME)

The Current Date field is an 8-character field initialized with the format 'mm/dd/yy'. It is displayed in blue color and is non-editable.

### Current Time (CURTIME)

The Program Name field is an 8-character field displayed in blue color. It is a non-editable field, as indicated by the `ASKIP` attribute.

### Account ID Input (ACTIDIN)

The Current Time field is an 8-character field initialized with the format 'hh:mm:ss'. It is displayed in blue color and is non-editable.

### Card Number Input (CARDNIN)

The Account ID Input field is an 11-character field where users can enter the account number. It is displayed in green color with an underline highlight. The field is validated to ensure it is numeric.

### Transaction Type Code (TTYPCD)

The Card Number Input field is a 16-character field where users can enter the card number. It is displayed in green color with an underline highlight. The field is validated to ensure it is numeric.

### Transaction Category Code (TCATCD)

The Transaction Type Code field is a 2-character field where users can enter the transaction type code. It is displayed in green color with an underline highlight. The field is validated to ensure it is numeric and not empty.

### Transaction Source (TRNSRC)

The Transaction Category Code field is a 4-character field where users can enter the transaction category code. It is displayed in green color with an underline highlight. The field is validated to ensure it is numeric and not empty.

### Transaction Description (TDESC)

The Transaction Source field is a 10-character field where users can enter the transaction source. It is displayed in green color with an underline highlight. The field is validated to ensure it is not empty.

### Transaction Amount (TRNAMT)

The Transaction Description field is a 60-character field where users can enter the transaction description. It is displayed in green color with an underline highlight. The field is validated to ensure it is not empty.

### Original Date (TORIGDT)

The Transaction Amount field is a 12-character field where users can enter the transaction amount. It is displayed in green color with an underline highlight. The field is validated to ensure it follows the format '-99999999.99' and is not empty.

### Processing Date (TPROCDT)

The Original Date field is a 10-character field where users can enter the original date of the transaction. It is displayed in green color with an underline highlight. The field is validated to ensure it follows the format 'YYYY-MM-DD' and is not empty.

### Merchant ID (MID)

The Processing Date field is a 10-character field where users can enter the processing date of the transaction. It is displayed in green color with an underline highlight. The field is validated to ensure it follows the format 'YYYY-MM-DD' and is not empty.

### Merchant Name (MNAME)

The Merchant ID field is a 9-character field where users can enter the merchant ID. It is displayed in green color with an underline highlight. The field is validated to ensure it is numeric and not empty.

### Merchant City (MCITY)

The Merchant Name field is a 30-character field where users can enter the merchant name. It is displayed in green color with an underline highlight. The field is validated to ensure it is not empty.

### Merchant Zip (MZIP)

The Merchant City field is a 25-character field where users can enter the merchant city. It is displayed in green color with an underline highlight. The field is validated to ensure it is not empty.

### Confirmation (CONFIRM)

The Merchant Zip field is a 10-character field where users can enter the merchant zip code. It is displayed in green color with an underline highlight. The field is validated to ensure it is not empty.

### Error/Status Message (ERRMSG)

The Confirmation field is a single-character field where users can confirm the transaction by entering 'Y' or 'N'. It is displayed in green color with an underline highlight. The field is validated to ensure it contains a valid value (Y/N).

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://staging.swimm.cloud/)</sup></SwmMeta>
