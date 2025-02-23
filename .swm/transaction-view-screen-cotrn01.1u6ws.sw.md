---
title: Transaction View Screen (COTRN01)
---
The Transaction View screen (COTRN01) allows users to view detailed information about a specific transaction by entering the transaction ID. It displays various transaction details such as card number, transaction type, category, source, description, amount, and merchant information.

## Screen Preview

```
Tran: CT01                AWS Mainframe Modernization             Date: mm/dd/yy
Prog: COTRN01C                     CardDemo                       Time: hh:mm:ss

                              View Transaction

     Enter Tran ID: ________________

     -----------------------------------------------------------------

     Transaction ID: ________________      Card Number: ________________

     Type CD: __      Category CD: ____      Source: __________

     Description: ____________________________________________________________

     Amount: ____________      Orig Date: __________      Proc Date: __________

     Merchant ID: _________      Merchant Name: ______________________

     Merchant City: _________________________      Merchant Zip: __________


ENTER=Fetch  F3=Back  F4=Clear  F5=Browse Tran.
```

## Fields

### Transaction ID Display (TRNID)

### Transaction ID Input Field (TRNIDIN)

- Length: 16 characters
- Required field
- Underlined, green color
- Initial value is a blank space
- Validations: Cannot be empty or contain low-values

### Card Number Display (CARDNUM)

- Length: 16 characters
- Read-only field
- Color: Blue
- Initial value is a blank space

### Type Code Display (TTYPCD)

- Length: 16 characters
- Read-only field
- Color: Blue
- Initial value is a blank space

### Category Code Display (TCATCD)

- Length: 2 characters
- Read-only field
- Color: Blue
- Initial value is a blank space

### Source Display (TRNSRC)

- Length: 4 characters
- Read-only field
- Color: Blue
- Initial value is a blank space

### Description Display (TDESC)

- Length: 10 characters
- Read-only field
- Color: Blue
- Initial value is a blank space

### Amount Display (TRNAMT)

- Length: 60 characters
- Read-only field
- Color: Blue
- Initial value is a blank space

### Original Date Display (TORIGDT)

- Length: 12 characters
- Read-only field
- Color: Blue
- Initial value is a blank space

### Processing Date Display (TPROCDT)

- Length: 10 characters
- Read-only field
- Color: Blue
- Initial value is a blank space

### Merchant ID Display (MID)

- Length: 10 characters
- Read-only field
- Color: Blue
- Initial value is a blank space

### Merchant Name Display (MNAME)

- Length: 9 characters
- Read-only field
- Color: Blue
- Initial value is a blank space

### Merchant City Display (MCITY)

- Length: 30 characters
- Read-only field
- Color: Blue
- Initial value is a blank space

### Merchant Zip Display (MZIP)

- Length: 25 characters
- Read-only field
- Color: Blue
- Initial value is a blank space

### Error Message Display (ERRMSG)

- Length: 10 characters
- Read-only field
- Color: Blue
- Initial value is a blank space

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
