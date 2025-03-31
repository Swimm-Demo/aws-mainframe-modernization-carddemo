---
title: Running Account Data Program (READACCT)
---
The READACCT job is designed to read account data from a VSAM Key-Sequenced Data Set (KSDS) and display it. This job utilizes the <SwmToken path="app/jcl/READACCT.jcl" pos="22:7:7" line-data="//STEP05 EXEC PGM=CBACT01C                                                      ">`CBACT01C`</SwmToken> program to execute the necessary operations, leveraging the load modules stored in the specified dataset.

For instance, when the READACCT job is executed, it accesses the account master VSAM file, processes the data, and outputs the account information.

## Running Account Data Program

Steps in this section: <SwmToken path="app/jcl/READACCT.jcl" pos="22:1:1" line-data="//STEP05 EXEC PGM=CBACT01C                                                      ">`STEP05`</SwmToken>.

This section is about running a program that reads and displays account data from a VSAM Key-Sequenced Data Set (KSDS). The program utilizes the load modules stored in the specified dataset to execute the necessary operations.

<SwmSnippet path="/app/jcl/READACCT.jcl" line="20">

---

Here we call the <SwmToken path="app/jcl/READACCT.jcl" pos="22:7:7" line-data="//STEP05 EXEC PGM=CBACT01C                                                      ">`CBACT01C`</SwmToken> program.

More about <SwmToken path="app/jcl/READACCT.jcl" pos="22:7:7" line-data="//STEP05 EXEC PGM=CBACT01C                                                      ">`CBACT01C`</SwmToken>: <SwmLink doc-title="Reading and Printing Account Data (CBACT01C)">[Reading and Printing Account Data (CBACT01C)](/.swm/reading-and-printing-account-data-cbact01c.3t7pnsm7.sw.md)</SwmLink>

```jcl
//* RUN THE PROGRAM THAT READS THE ACCOUNT MASTER VSAM FILE                     
//* *******************************************************************         
//STEP05 EXEC PGM=CBACT01C                                                      
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="kyndryl-aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
