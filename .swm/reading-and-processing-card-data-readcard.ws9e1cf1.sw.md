---
title: Reading and processing card data (READCARD)
---
This document describes the READCARD job, which is responsible for executing the program that reads and prints card data files from a VSAM Key-Sequenced Data Set (KSDS). The job ensures efficient data access and management by handling file operations and errors.

For example, the READCARD job runs the <SwmToken path="app/jcl/READCARD.jcl" pos="22:7:7" line-data="//STEP05 EXEC PGM=CBACT02C                                                      ">`CBACT02C`</SwmToken> program to read the card master VSAM file and print the card data. This process involves accessing the card data file, managing file operations, and handling any errors that may occur.

## Reading and processing card data

Steps in this section: <SwmToken path="app/jcl/READCARD.jcl" pos="22:1:1" line-data="//STEP05 EXEC PGM=CBACT02C                                                      ">`STEP05`</SwmToken>.

This section is about executing a program that reads and prints card data files from a VSAM Key-Sequenced Data Set (KSDS). The program handles file operations and errors, ensuring efficient data access and management.

<SwmSnippet path="/app/jcl/READCARD.jcl" line="20">

---

Here we call the <SwmToken path="app/jcl/READCARD.jcl" pos="22:7:7" line-data="//STEP05 EXEC PGM=CBACT02C                                                      ">`CBACT02C`</SwmToken> program.

More about <SwmToken path="app/jcl/READCARD.jcl" pos="22:7:7" line-data="//STEP05 EXEC PGM=CBACT02C                                                      ">`CBACT02C`</SwmToken>: <SwmLink doc-title="Reading and Printing Card Data (CBACT02C)">[Reading and Printing Card Data (CBACT02C)](/.swm/reading-and-printing-card-data-cbact02c.7o7rda8z.sw.md)</SwmLink>

```jcl
//* RUN THE PROGRAM THAT READS THE CARD MASTER VSAM FILE                        
//* *******************************************************************         
//STEP05 EXEC PGM=CBACT02C                                                      
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="kyndryl-aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
