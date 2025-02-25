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

### Transaction Name (TRNNAME)

- Length: 4 characters
- Color: Blue
- Field is skipped and set to normal

### Current Date (CURDATE)

- Length: 8 characters
- Color: Blue
- Initial value: 'mm/dd/yy'
- Field is skipped and set to normal

### Program Name (PGMNAME)

- Length: 8 characters
- Color: Blue
- Field is protected and set to normal
- Associated document: <SwmLink doc-title="Sign-On Flow (COSGN00C)" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo" path="/.swm/sign-on-flow-cosgn00c.u82f6gob.sw.md">[Sign-On Flow (COSGN00C)](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v/docs/u82f6gob)</SwmLink>

### Current Time (CURTIME)

- Length: 9 characters
- Color: Blue
- Initial value: 'Ahh:mm:ss'
- Field is protected and set to normal

### Application ID (APPLID)

- Length: 8 characters
- Color: Blue
- Field is protected and set to normal

### System ID (SYSID)

- Length: 8 characters
- Color: Blue
- Field is protected and set to normal

### User ID (USERID)

- Length: 8 characters
- Color: Green
- Field is unprotected and set to normal
- Highlight is off
- Initial value: '(8 Char)'

### Password (PASSWD)

- Length: 8 characters
- Color: Green
- Field is unprotected and set to normal
- Highlight is off
- Initial value: '\_**\_\_**\_'
- Field is dark

&nbsp;

_This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human_

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
