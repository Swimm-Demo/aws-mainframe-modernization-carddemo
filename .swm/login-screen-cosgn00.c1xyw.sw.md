---
title: Login Screen (COSGN00)
---
The Login Screen (COSGN00) is the initial interface for users to access the CardDemo application. It prompts users to enter their User ID and Password to authenticate and gain access to the system.

## Screen Preview

```
Tran : CC00                  AWS Mainframe Modernization             Date : mm/dd/yy
Prog : COSGN00C                        CardDemo                      Time : Ahh:mm:ss
AppID:                                                              SysID:         

     This is a Credit Card Demo Application for Mainframe Modernization

                    +========================================+
                    |%%%%%%%  NATIONAL RESERVE NOTE  %%%%%%%%|
                    |%(1)  THE UNITED STATES OF KICSLAND (1)%|
                    |%$$              ___       ********  $$%|
                    |%$    {x}       (o o)                 $%|
                    |%$     ******  (  V  )      O N E     $%|
                    |%(1)          ---m-m---             (1)%|
                    |%%~~~~~~~~~~~ ONE DOLLAR ~~~~~~~~~~~~~%%|
                    +========================================+

               Type your User ID and Password, then press ENTER:

                            User ID     :         (8 Char)
                            Password    : ________ (8 Char)



ENTER=Sign-on  F3=Exit
```

## Fields

### Current Date (CURDATE)

### Transaction Name (TRNNAME)

This field is used to display the transaction name. It is a fixed field with a length of 4 characters and is displayed in blue color.

### Program Name (PGMNAME)

This field displays the current date in the format 'mm/dd/yy'. It is a fixed field with a length of 8 characters and is displayed in blue color.

### Current Time (CURTIME)

This field displays the program name. It is a fixed field with a length of 8 characters and is displayed in blue color.

### Application ID (APPLID)

This field displays the current time in the format 'hh:mm:ss'. It is a fixed field with a length of 9 characters and is displayed in blue color.

### System ID (SYSID)

This field displays the application ID. It is a fixed field with a length of 8 characters and is displayed in blue color.

### User ID (USERID)

This field displays the system ID. It is a fixed field with a length of 8 characters and is displayed in blue color.

### Password (PASSWD)

This field is used for the user to input their User ID. It is an unprotected field with a length of 8 characters and is displayed in green color. The field is validated to ensure it is not empty.

### Error Message (ERRMSG)

This field is used for the user to input their password. It is an unprotected field with a length of 8 characters and is displayed in green color. The field is validated to ensure it is not empty.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
