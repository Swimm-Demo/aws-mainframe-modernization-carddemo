---
title: Sign-On Flow (COSGN00C)
---

The document describes the process of managing user sign-on by receiving credentials, validating them, and routing users to the appropriate menu based on their role. This is achieved by processing user input, validating credentials against a security file, and routing users based on their role.

# Usages

This program is used multiple times in the codebase as represented in the following diagram:

(Note - these are only some of the usages of this program)

```mermaid
graph TD
    A["Account Update Processing Flow (COACTUPC)
"] --> L
    B["Bill Payment (COBIL00C)
"] --> L
    C["Card Update Flow (COCRDUPC)"] --> L
    D["Report Processing Flow (CORPT00C)
"] --> L
    E["List Transactions (COTRN00C)
"] --> L
    L["Sign-On Flow (COSGN00C)"]
```

# Program Drill Down

```mermaid
flowchart TB
    A["PROCESS-ENTER-KEY"] --> B["READ-USER-SEC-FILE"]
    subgraph B["READ-USER-SEC-FILE"]
        direction TB
        F["Read User Security File"] --> C
        C["Check if user password matches"] --> |"Yes"| D["Route to relevant menu"]
        C --> |"No"| E["Show error message"]
    end
```

## User Input Processing

<SwmSnippet path="/app/cbl/COSGN00C.cbl" line="108" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

The `PROCESS-ENTER-KEY` function is responsible for processing the user's sign-on input. It begins by receiving the user credentials from the sign-on screen using the <SwmToken path="/app/cbl/COSGN00C.cbl" pos="110:1:5" line-data="           EXEC CICS RECEIVE" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo">`EXEC CICS RECEIVE`</SwmToken> command. This command captures the user input and stores it in the appropriate fields.

```cobol
       PROCESS-ENTER-KEY.

           EXEC CICS RECEIVE
                     MAP('COSGN0A')
                     MAPSET('COSGN00')
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC.
```

---

</SwmSnippet>

<SwmSnippet path="/app/cbl/COSGN00C.cbl" line="117" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

The function checks that both the user ID (USERIDI) and the password (PASSWDI) are filled. If not it shows an error.

```cobol
           EVALUATE TRUE
               WHEN USERIDI OF COSGN0AI = SPACES OR LOW-VALUES
                   MOVE 'Y'      TO WS-ERR-FLG
                   MOVE 'Please enter User ID ...' TO WS-MESSAGE
                   MOVE -1       TO USERIDL OF COSGN0AI
                   PERFORM SEND-SIGNON-SCREEN
               WHEN PASSWDI OF COSGN0AI = SPACES OR LOW-VALUES
                   MOVE 'Y'      TO WS-ERR-FLG
                   MOVE 'Please enter Password ...' TO WS-MESSAGE
                   MOVE -1       TO PASSWDL OF COSGN0AI
                   PERFORM SEND-SIGNON-SCREEN
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
```

---

</SwmSnippet>

<SwmSnippet path="/app/cbl/COSGN00C.cbl" line="132" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

If they are, the function converts the User ID and Password to uppercase and stores them in <SwmToken path="/app/cbl/COSGN00C.cbl" pos="133:1:5" line-data="                           WS-USER-ID" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo">`WS-USER-ID`</SwmToken> and <SwmToken path="/app/cbl/COSGN00C.cbl" pos="223:11:15" line-data="                   IF SEC-USR-PWD = WS-USER-PWD" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo">`WS-USER-PWD`</SwmToken> respectively. This ensures that the credentials are in a consistent format for further processing.

If no errors are found the function proceeds to call the <SwmToken path="/app/cbl/COSGN00C.cbl" pos="139:3:9" line-data="               PERFORM READ-USER-SEC-FILE" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo">`READ-USER-SEC-FILE`</SwmToken> function to validate the user credentials against the security file.

```
           MOVE FUNCTION UPPER-CASE(USERIDI OF COSGN0AI) TO
                           WS-USER-ID
                           CDEMO-USER-ID
           MOVE FUNCTION UPPER-CASE(PASSWDI OF COSGN0AI) TO
                           WS-USER-PWD

           IF NOT ERR-FLG-ON
               PERFORM READ-USER-SEC-FILE
           END-IF.
```

---

</SwmSnippet>

## User Credential Validation

<SwmSnippet path="/app/cbl/COSGN00C.cbl" line="209" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

The <SwmToken path="/app/cbl/COSGN00C.cbl" pos="139:3:9" line-data="               PERFORM READ-USER-SEC-FILE" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo">`READ-USER-SEC-FILE`</SwmToken> function reads the user security file to validate the user credentials. It uses the <SwmToken path="/app/cbl/COSGN00C.cbl" pos="211:1:5" line-data="           EXEC CICS READ" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo">`EXEC CICS READ`</SwmToken> command to read the security data associated with the provided User ID (<SwmToken path="/app/cbl/COSGN00C.cbl" pos="215:4:8" line-data="                RIDFLD    (WS-USER-ID)" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo">`WS-USER-ID`</SwmToken>). The response code from this operation is stored in <SwmToken path="/app/cbl/COSGN00C.cbl" pos="217:4:8" line-data="                RESP      (WS-RESP-CD)" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo">`WS-RESP-CD`</SwmToken>.

```cobol
       READ-USER-SEC-FILE.

           EXEC CICS READ
                DATASET   (WS-USRSEC-FILE)
                INTO      (SEC-USER-DATA)
                LENGTH    (LENGTH OF SEC-USER-DATA)
                RIDFLD    (WS-USER-ID)
                KEYLENGTH (LENGTH OF WS-USER-ID)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC.
```

---

</SwmSnippet>

<SwmSnippet path="/app/cbl/COSGN00C.cbl" line="223" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

If the read was successful, the function compares the stored password (<SwmToken path="/app/cbl/COSGN00C.cbl" pos="223:3:7" line-data="                   IF SEC-USR-PWD = WS-USER-PWD" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo">`SEC-USR-PWD`</SwmToken>) with the provided password (<SwmToken path="/app/cbl/COSGN00C.cbl" pos="223:11:15" line-data="                   IF SEC-USR-PWD = WS-USER-PWD" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo">`WS-USER-PWD`</SwmToken>). If the passwords match, the function sets up the user session. It also determines the user type (<SwmToken path="/app/cbl/COSGN00C.cbl" pos="227:3:7" line-data="                       MOVE SEC-USR-TYPE TO CDEMO-USER-TYPE" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo">`SEC-USR-TYPE`</SwmToken>) and routes the user to the appropriate menu based on their role. If the user is an administrator (<SwmToken path="/app/cbl/COSGN00C.cbl" pos="230:3:7" line-data="                       IF CDEMO-USRTYP-ADMIN" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo">`CDEMO-USRTYP-ADMIN`</SwmToken>), the function transfers control to the admin program (<SwmToken path="/app/cbl/COSGN00C.cbl" pos="232:5:5" line-data="                              PROGRAM (&#39;COADM01C&#39;)" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo">`COADM01C`</SwmToken>). Otherwise, it transfers control to the regular user menu program (<SwmToken path="/app/cbl/COSGN00C.cbl" pos="237:5:5" line-data="                              PROGRAM (&#39;COMEN01C&#39;)" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="aws-mainframe-modernization-carddemo">`COMEN01C`</SwmToken>).

```cobol
                   IF SEC-USR-PWD = WS-USER-PWD
                       MOVE WS-TRANID    TO CDEMO-FROM-TRANID
                       MOVE WS-PGMNAME   TO CDEMO-FROM-PROGRAM
                       MOVE WS-USER-ID   TO CDEMO-USER-ID
                       MOVE SEC-USR-TYPE TO CDEMO-USER-TYPE
                       MOVE ZEROS        TO CDEMO-PGM-CONTEXT

                       IF CDEMO-USRTYP-ADMIN
                            EXEC CICS XCTL
                              PROGRAM ('COADM01C')
                              COMMAREA(CARDDEMO-COMMAREA)
                            END-EXEC
                       ELSE
                            EXEC CICS XCTL
                              PROGRAM ('COMEN01C')
                              COMMAREA(CARDDEMO-COMMAREA)
                            END-EXEC
                       END-IF
```

---

</SwmSnippet>

<SwmSnippet path="/app/cbl/COSGN00C.cbl" line="241" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v">

---

If the passwords do not match, do user wasn't found, or any other response value, the function prompts the user to try again.

```
                   ELSE
                       MOVE 'Wrong Password. Try again ...' TO
                                                          WS-MESSAGE
                       MOVE -1       TO PASSWDL OF COSGN0AI
                       PERFORM SEND-SIGNON-SCREEN
                   END-IF
               WHEN 13
                   MOVE 'Y'      TO WS-ERR-FLG
                   MOVE 'User not found. Try again ...' TO WS-MESSAGE
                   MOVE -1       TO USERIDL OF COSGN0AI
                   PERFORM SEND-SIGNON-SCREEN
               WHEN OTHER
                   MOVE 'Y'      TO WS-ERR-FLG
                   MOVE 'Unable to verify the User ...' TO WS-MESSAGE
                   MOVE -1       TO USERIDL OF COSGN0AI
                   PERFORM SEND-SIGNON-SCREEN
```

---

</SwmSnippet>

&nbsp;

_This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human_

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
