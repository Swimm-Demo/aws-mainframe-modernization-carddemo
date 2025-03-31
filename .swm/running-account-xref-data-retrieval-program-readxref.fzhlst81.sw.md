---
title: Running Account XREF Data Retrieval Program (READXREF)
---
The READXREF job is designed to read and display account cross-reference data from a VSAM KSDS dataset. This job utilizes a specific load library and outputs the results to the system output. The program executed in this job is <SwmToken path="app/jcl/READXREF.jcl" pos="22:7:7" line-data="//STEP05 EXEC PGM=CBACT03C                                                      ">`CBACT03C`</SwmToken>, which is responsible for processing the cross-reference data.

For instance, when the READXREF job is run, it retrieves the account cross-reference data and prints it to the system output, allowing users to view the data for further analysis or processing.

## Running Account XREF Data Retrieval Program

Steps in this section: <SwmToken path="app/jcl/READXREF.jcl" pos="22:1:1" line-data="//STEP05 EXEC PGM=CBACT03C                                                      ">`STEP05`</SwmToken>.

This section is about running a program that reads and displays account cross-reference data from a VSAM KSDS dataset. The program uses a specific load library and outputs the results to the system output.

<SwmSnippet path="/app/jcl/READXREF.jcl" line="20">

---

Here we call the <SwmToken path="app/jcl/READXREF.jcl" pos="22:7:7" line-data="//STEP05 EXEC PGM=CBACT03C                                                      ">`CBACT03C`</SwmToken> program.

More about <SwmToken path="app/jcl/READXREF.jcl" pos="22:7:7" line-data="//STEP05 EXEC PGM=CBACT03C                                                      ">`CBACT03C`</SwmToken>: <SwmLink doc-title="Reading and Printing Account Cross-Reference Data (CBACT03C)">[Reading and Printing Account Cross-Reference Data (CBACT03C)](/.swm/reading-and-printing-account-cross-reference-data-cbact03c.dyyif7ya.sw.md)</SwmLink>

```jcl
//* RUN THE PROGRAM THAT READS THE XREF MASTER VSAM FILE                        
//* *******************************************************************         
//STEP05 EXEC PGM=CBACT03C                                                      
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="kyndryl-aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
