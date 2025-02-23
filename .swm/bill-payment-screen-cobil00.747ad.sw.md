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

### Current Date (CURDATE)

### Transaction Name (TRNNAME)

This field displays the transaction name, which is fixed as 'CB00'. It is a non-editable field with a length of 4 characters and is displayed in blue color.

### Program Name (PGMNAME)

This field shows the current date in the format 'mm/dd/yy'. It is a non-editable field with a length of 8 characters and is displayed in blue color.

### Current Time (CURTIME)

This field displays the program name, which is fixed as 'COBIL00C'. It is a non-editable field with a length of 8 characters and is displayed in blue color.

### Account ID Input (ACTIDIN)

This field shows the current time in the format 'hh:mm:ss'. It is a non-editable field with a length of 8 characters and is displayed in blue color.

### Current Balance (CURBAL)

This field is used to enter the account ID. It is an editable field with a length of 11 characters, underlined, and displayed in green color. The field is mandatory and cannot be left empty.

### Payment Confirmation (CONFIRM)

This field displays the current balance of the account. It is a non-editable field with a length of 14 characters and is displayed in blue color.

### Error/Status Message (ERRMSG)

This field is used to confirm the payment. It is an editable field with a length of 1 character, underlined, and displayed in green color. The valid inputs are 'Y' or 'N', and the field is case-insensitive.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://staging.swimm.cloud/)</sup></SwmMeta>
