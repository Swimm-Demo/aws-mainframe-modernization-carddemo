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

### Transaction Name (TRNNAME)

- Length: 4 characters
- Fixed field
- Color: Blue

### Current Date (CURDATE)

- Length: 8 characters
- Fixed field
- Color: Blue
- Initial value: 'mm/dd/yy'

### Program Name (PGMNAME)

- Length: 8 characters
- Fixed field
- Color: Blue
- Associated document: <SwmLink doc-title="List Transactions (COTRN00C)" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo" path="/.swm/list-transactions-cotrn00c.k1wp8f8o.sw.md">[List Transactions (COTRN00C)](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v/docs/k1wp8f8o)</SwmLink>

### Current Time (CURTIME)

- Length: 8 characters
- Fixed field
- Color: Blue
- Initial value: 'hh:mm:ss'

### Page Number (PAGENUM)

- Length: 8 characters
- Fixed field
- Color: Blue

### Search Transaction ID (TRNIDIN)

- Length: 16 characters
- Input field
- Color: Green
- Underlined

### Transaction ID (TRNID01 - TRNID10)

- Length: 16 characters
- Fixed field
- Color: Blue

### Transaction Date (TDATE01 - TDATE10)

- Length: 8 characters
- Fixed field
- Color: Blue

### Transaction Description (TDESC01 - TDESC10)

- Length: 26 characters
- Fixed field
- Color: Blue

### Transaction Amount (TAMT001 - TAMT010)

- Length: 12 characters
- Fixed field
- Color: Blue

### Selection Field (SEL0001 - SEL0010)

- Length: 1 character
- Input field
- Color: Green
- Underlined

&nbsp;

_This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human_

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
