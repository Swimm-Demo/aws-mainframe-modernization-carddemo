---
title: Bill Payment Screen (COBIL00)
---

The Bill Payment screen (COBIL00) provides a straightforward interface for customers to view and pay their credit card balance. After entering their account ID, customers can see their current balance and choose whether to process a payment.

## Screen Preview

```
Tran: CB00                      AWS Mainframe Modernization          Date: mm/dd/yy
Prog: COBIL00C                            CardDemo                   Time: hh:mm:ss

                                   Bill Payment

     Enter Acct ID: ___________

     -----------------------------------------------------------------

          Your current balance is: ______________


          Do you want to pay your balance now. Please confirm: _ (Y/N)




[Error/Status Message Area]

ENTER=Continue  F3=Back  F4=Clear
```

## Fields

### Transaction Name (TRNNAME)

- Length: 4 characters
- Color: Blue
- Fixed information

### Current Date (CURDATE)

- Length: 8 characters
- Color: Blue
- Initial value: 'mm/dd/yy'
- Fixed information

### Program Name (PGMNAME)

- Length: 8 characters
- Color: Blue
- Fixed information
- Associated document: <SwmLink doc-title="Bill Payment (COBIL00C)" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo" path="/.swm/bill-payment-cobil00c.vqxk83ji.sw.md">[Bill Payment (COBIL00C)](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v/docs/vqxk83ji)</SwmLink>

### Current Time (CURTIME)

- Length: 8 characters
- Color: Blue
- Initial value: 'hh:mm:ss'
- Fixed information

### Account ID Input (ACTIDIN)

- Length: 11 characters
- Color: Green
- Underlined
- User input required

### Current Balance (CURBAL)

- Length: 14 characters
- Color: Blue
- Fixed information

### Payment Confirmation (CONFIRM)

- Length: 1 character
- Color: Green
- Underlined
- User input required (Y/N)

&nbsp;

_This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human_

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
