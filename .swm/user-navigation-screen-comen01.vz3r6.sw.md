---
title: User Navigation Screen (COMEN01)
---
The User Navigation screen (COMEN01) serves as the primary navigation interface for regular users, allowing them to select from various options such as viewing and updating account information, managing credit cards, and accessing transaction reports.

## Screen Preview

```
Tran: CM00                      AWS Mainframe Modernization           Date: mm/dd/yy
Prog: COMEN01C                            CardDemo                    Time: hh:mm:ss

                                    Main Menu

                    1. Account View                       
                    2. Account Update                     
                    3. Credit Card List                   
                    4. Credit Card View                   
                    5. Credit Card Update                 
                    6. Transaction List                   
                    7. Transaction View                   
                    8. Transaction Add                    
                    9. Transaction Reports                
                    10. Bill Payment                      



              Please select an option : __


ENTER=Continue  F3=Exit
```

## Fields

### Current Date (CURDATE)

### Transaction Name (TRNNAME)

- Length: 4 characters
- Color: Blue
- Field is skipped and set to normal attribute

### Program Name (PGMNAME)

- Length: 8 characters
- Color: Blue
- Initial value: 'mm/dd/yy'
- Field is skipped and set to normal attribute

### Current Time (CURTIME)

- Length: 8 characters
- Color: Blue
- Field is skipped and set to normal attribute

### Menu Options (OPTN001 - OPTN012)

- Length: 8 characters
- Color: Blue
- Initial value: 'hh:mm:ss'
- Field is skipped and set to normal attribute

### Option Selection (OPTION)

- Length: 40 characters each
- Color: Blue
- Fields are skipped and set to normal attribute
- Initial values are spaces

### Error Message (ERRMSG)

- Length: 2 characters
- Color: Blue
- Field is not protected and allows numeric input
- Underlined and right-justified with zero fill
- Initial cursor position

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
