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

### Transaction Name (TRNNAME)

- Length: 4 characters
- Display-only field
- Color: Blue

### Current Date (CURDATE)

- Length: 8 characters
- Display-only field
- Color: Blue
- Format: mm/dd/yy

### Program Name (PGMNAME)

- Length: 8 characters
- Display-only field
- Color: Blue
- Associated document: <SwmLink doc-title="View Transaction (COTRN01C)" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo" path="/.swm/view-transaction-cotrn01c.7an4bkg7.sw.md">[View Transaction (COTRN01C)](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v/docs/7an4bkg7)</SwmLink>

### Current Time (CURTIME)

- Length: 8 characters
- Display-only field
- Color: Blue
- Format: hh:mm:ss

### Transaction ID Input (TRNIDIN)

- Length: 16 characters
- Input field
- Color: Green
- Underlined

### Transaction ID (TRNID)

- Length: 16 characters
- Display-only field
- Color: Blue

### Card Number (CARDNUM)

- Length: 16 characters
- Display-only field
- Color: Blue

### Type Code (TTYPCD)

- Length: 2 characters
- Display-only field
- Color: Blue

### Category Code (TCATCD)

- Length: 4 characters
- Display-only field
- Color: Blue

### Source (TRNSRC)

- Length: 10 characters
- Display-only field
- Color: Blue

### Description (TDESC)

- Length: 60 characters
- Display-only field
- Color: Blue

### Amount (TRNAMT)

- Length: 12 characters
- Display-only field
- Color: Blue

### Original Date (TORIGDT)

- Length: 10 characters
- Display-only field
- Color: Blue

### Processing Date (TPROCDT)

- Length: 10 characters
- Display-only field
- Color: Blue

### Merchant ID (MID)

- Length: 9 characters
- Display-only field
- Color: Blue

### Merchant Name (MNAME)

- Length: 30 characters
- Display-only field
- Color: Blue

### Merchant City (MCITY)

- Length: 25 characters
- Display-only field
- Color: Blue

### Merchant Zip (MZIP)

- Length: 10 characters
- Display-only field
- Color: Blue

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
