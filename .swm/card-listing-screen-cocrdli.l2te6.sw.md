---
title: Card Listing Screen (COCRDLI)
---
The Card Listing screen (COCRDLI) allows users to view and manage a list of credit cards associated with their account. It provides functionalities to filter cards by account number and credit card number, and navigate through the list using paging options.

## Screen Preview

```
Tran:     AWS Mainframe Modernization          Date: mm/dd/yy
Prog: COCRDLIC                            CardDemo                   Time: hh:mm:ss

                                   List Credit Cards

Account Number : ___________
Credit Card Number: ________________

Select Account Number      Card Number       Active
------ --------------- --------------- --------
_ ____________ ________________ _
_ ____________ ________________ _
_ ____________ ________________ _
_ ____________ ________________ _
_ ____________ ________________ _
_ ____________ ________________ _
_ ____________ ________________ _

[Error/Status Message Area]

 F3=Exit F7=Backward F8=Forward
```

## Fields

### Current Date (CURDATE)

### Transaction Name (TRNNAME)

This field is used to display the transaction name. It is a 4-character field, displayed in blue color, and is not modifiable by the user.

### Program Name (PGMNAME)

This field displays the current date in the format 'mm/dd/yy'. It is an 8-character field, displayed in blue color, and is not modifiable by the user.

### Current Time (CURTIME)

This field displays the program name. It is an 8-character field, displayed in blue color, and is not modifiable by the user.

### Page Number (PAGENO)

This field displays the current time in the format 'hh:mm:ss'. It is an 8-character field, displayed in blue color, and is not modifiable by the user.

### Account Number Input (ACCTSID)

This field displays the current page number. It is a 3-character field, and its value is dynamically updated based on the user's navigation.

### Credit Card Number Input (CARDSID)

This field allows the user to input an account number. It is an 11-character field, underlined, displayed in green color, and is modifiable by the user. The input must be numeric and exactly 11 digits long.

### Select Field (CRDSEL1 - CRDSEL7)

This field allows the user to input a credit card number. It is a 16-character field, underlined, displayed in green color, and is modifiable by the user. The input must be numeric and exactly 16 digits long.

### Account Number Display (ACCTNO1 - ACCTNO7)

These fields allow the user to select a record for further actions. Each field is a single character, underlined, and displayed in default color. Valid inputs are 'S' for view and 'U' for update.

### Credit Card Number Display (CRDNUM1 - CRDNUM7)

These fields display the account numbers associated with the listed credit cards. Each field is an 11-character field, displayed in default color, and is not modifiable by the user.

### Card Status Display (CRDSTS1 - CRDSTS7)

These fields display the credit card numbers associated with the listed accounts. Each field is a 16-character field, displayed in default color, and is not modifiable by the user.

### Information Message (INFOMSG)

These fields display the status of the listed credit cards. Each field is a single character, displayed in default color, and is not modifiable by the user.

### Error Message (ERRMSG)

This field displays informational messages to the user. It is a 45-character field, displayed in neutral color, and is not modifiable by the user.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://staging.swimm.cloud/)</sup></SwmMeta>
