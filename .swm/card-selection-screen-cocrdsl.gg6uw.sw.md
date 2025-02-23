---
title: Card Selection Screen (COCRDSL)
---
The Card Selection Screen (COCRDSL) allows users to view and select details of their credit cards. It provides fields for entering account and card numbers, and displays information such as the name on the card, card status, and expiry date.

## Screen Preview

```
Tran: CCDL                   AWS Mainframe Modernization          Date: mm/dd/yy
Prog: COCRDSLC               CardDemo                            Time: hh:mm:ss

                       View Credit Card Detail


                      Account Number    : ___________
                      Card Number       : ________________


   Name on card      : __________________________________________________

   Card Active Y/N   : _

   Expiry Date       : __/____ 





                         Please enter Account and Card Number




ENTER=Search Cards  F3=Exit
```

## Fields

### Current Date (CURDATE)

### Transaction Name (TRNNAME)

This field is 4 characters long and is displayed in blue color. It is a fixed field with no user input required.

### Program Name (PGMNAME)

This field is 8 characters long and is displayed in blue color. It shows the date in the format 'mm/dd/yy' and is a fixed field with no user input required.

### Current Time (CURTIME)

This field is 8 characters long and is displayed in blue color. It is a fixed field with no user input required.

### Account Number (ACCTSID)

This field is 8 characters long and is displayed in blue color. It shows the time in the format 'hh:mm:ss' and is a fixed field with no user input required.

### Card Number (CARDSID)

This field is 11 characters long, underlined, and displayed in the default color. It is an input field where the user must enter the account number. The account number must be a non-zero 11-digit numeric value. If the input is invalid, an error message will be displayed.

### Name on Card (CRDNAME)

This field is 16 characters long, underlined, and displayed in the default color. It is an input field where the user must enter the card number. The card number must be a 16-digit numeric value. If the input is invalid, an error message will be displayed.

### Card Active Status (CRDSTCD)

This field is 50 characters long, underlined, and displayed in turquoise color. It is a fixed field that displays the name on the card.

### Expiry Date (EXPMON and EXPYEAR)

This field is 1 character long, underlined, and displayed in turquoise color. It is a fixed field that displays whether the card is active (Y) or not (N).

### Information Message (INFOMSG)

The expiry date is split into two fields: EXPMON (2 characters) for the month and EXPYEAR (4 characters) for the year. Both fields are underlined and displayed in turquoise color. They are fixed fields that display the card's expiry date.

### Error Message (ERRMSG)

This field is 40 characters long and displayed in neutral color. It is a fixed field used to display informational messages to the user.

### Function Keys (FKEYS)

This field is 80 characters long and displayed in red color. It is a fixed field used to display error messages to the user.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
