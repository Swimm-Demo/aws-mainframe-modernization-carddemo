---
title: Transaction List Screen (COTRN00)
---
The Transaction List screen (COTRN00) allows users to view and search for transactions within the CardDemo application. Users can enter a transaction ID to search for specific transactions and navigate through the list using various function keys.

## Screen Preview

```
Tran:     Date: mm/dd/yy
Prog:     Time: hh:mm:ss

                              List Transactions

Search Tran ID: ________________

Sel  Transaction ID     Date     Description                  Amount
---  ----------------  --------  --------------------------  ------------
 _   ________________  ________  __________________________  ____________
 _   ________________  ________  __________________________  ____________
 _   ________________  ________  __________________________  ____________
 _   ________________  ________  __________________________  ____________
 _   ________________  ________  __________________________  ____________
 _   ________________  ________  __________________________  ____________
 _   ________________  ________  __________________________  ____________
 _   ________________  ________  __________________________  ____________
 _   ________________  ________  __________________________  ____________
 _   ________________  ________  __________________________  ____________

          Type 'S' to View Transaction details from the list

[Error/Status Message Area]

ENTER=Continue F3=Back F7=Backward F8=Forward
```

## Fields

### Current Date (CURDATE)

### Transaction Name (TRNNAME)

- Length: 4 characters
- Display-only field
- Color: Blue

### Program Name (PGMNAME)

- Length: 8 characters
- Display-only field
- Color: Blue
- Format: mm/dd/yy

### Current Time (CURTIME)

- Length: 8 characters
- Display-only field
- Color: Blue

### Page Number (PAGENUM)

- Length: 8 characters
- Display-only field
- Color: Blue
- Format: hh:mm:ss

### Search Transaction ID (TRNIDIN)

- Length: 8 characters
- Display-only field
- Color: Blue

### Transaction Selection (SEL0001 - SEL0010)

- Length: 16 characters
- Input field
- Color: Green
- Underlined
- Validation: Must be numeric

### Transaction ID (TRNID01 - TRNID10)

- Length: 1 character
- Input field
- Color: Green
- Underlined
- Validation: Must be 'S' or 's' to view transaction details

### Transaction Date (TDATE01 - TDATE10)

- Length: 16 characters
- Display-only field
- Color: Blue

### Transaction Description (TDESC01 - TDESC10)

- Length: 8 characters
- Display-only field
- Color: Blue

### Transaction Amount (TAMT001 - TAMT010)

- Length: 26 characters
- Display-only field
- Color: Blue

### Error/Status Message Area (ERRMSG)

- Length: 12 characters
- Display-only field
- Color: Blue

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://staging.swimm.cloud/)</sup></SwmMeta>
