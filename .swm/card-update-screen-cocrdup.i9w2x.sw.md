---
title: Card Update Screen (COCRDUP)
---
The Card Update Screen (COCRDUP) allows users to update credit card details such as account number, card number, name on the card, card status, and expiry date. This screen is essential for maintaining accurate and up-to-date credit card information within the system.

## Screen Preview

```
Tran: CCUP                       AWS Mainframe Modernization        Date: mm/dd/yy
Prog: COCRDUPC                              CardDemo                Time: hh:mm:ss

                              Update Credit Card Details


                      Account Number    : ___________
                      Card Number       : ________________

     Name on card      : __________________________________________________

     Card Active Y/N   : _

     Expiry Date       : __/____




                        [Information message area]

[Error message area                                                              ]
ENTER=Process F3=Exit      F5=Save F12=Cancel
```

## Fields

### Current Date (CURDATE)

### Transaction Name (TRNNAME)

This field is 4 characters long and is displayed in blue color. It is a fixed field and does not accept user input.

### Program Name (PGMNAME)

This field is 8 characters long and is displayed in blue color. It shows the date in the format 'mm/dd/yy' and is a fixed field.

### Current Time (CURTIME)

This field is 8 characters long and is displayed in blue color. It is a fixed field and does not accept user input.

### Account Number (ACCTSID)

This field is 8 characters long and is displayed in blue color. It shows the time in the format 'hh:mm:ss' and is a fixed field.

### Card Number (CARDSID)

This field is 11 characters long, underlined, and displayed in the default color. It is a protected field and does not accept user input.

### Name on Card (CRDNAME)

This field is 16 characters long, underlined, and displayed in the default color. It accepts user input and is validated to ensure it is numeric and 16 characters long.

### Card Active Status (CRDSTCD)

This field is 50 characters long, underlined, and accepts user input. The input is validated to ensure it contains only alphabets and spaces.

### Expiry Month (EXPMON)

This field is 1 character long, underlined, and accepts user input. The input is validated to ensure it is either 'Y' or 'N'.

### Expiry Year (EXPYEAR)

This field is 2 characters long, underlined, and right-justified. It accepts user input and is validated to ensure it is a numeric value between 1 and 12.

### Expiry Day (EXPDAY)

This field is 4 characters long, underlined, and right-justified. It accepts user input and is validated to ensure it is a numeric value between 1950 and 2099.

### Information Message (INFOMSG)

This field is 2 characters long, right-justified, and protected. It does not accept user input.

### Error Message (ERRMSG)

This field is 40 characters long and is used to display informational messages to the user. It is a protected field.

### Function Keys (FKEYS)

This field is 80 characters long and is used to display error messages to the user. It is displayed in red color.

### Function Keys Confirmation (FKEYSC)

This field is 21 characters long and is used to display function key options to the user. It is displayed in yellow color.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
