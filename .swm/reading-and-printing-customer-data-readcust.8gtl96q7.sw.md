---
title: Reading and Printing Customer Data (READCUST)
---
The READCUST job is responsible for reading and printing customer data from a VSAM Key-Sequenced Data Set (KSDS). This job runs a COBOL program that utilizes specific datasets for load modules and customer data, directing the output to system output streams.

For example, when the job is executed, it reads customer information stored in the VSAM file and prints it to the system output, which can then be reviewed or processed further.

## Reading and printing customer data

Steps in this section: <SwmToken path="app/jcl/READCUST.jcl" pos="6:1:1" line-data="//STEP05 EXEC PGM=CBCUS01C">`STEP05`</SwmToken>.

This section is about running a COBOL program that reads and prints customer data from a VSAM Key-Sequenced Data Set (KSDS). The program utilizes specific datasets for load modules and customer data, and directs the output to system output streams.

<SwmSnippet path="/app/jcl/READCUST.jcl" line="4">

---

Here we call the <SwmToken path="app/jcl/READCUST.jcl" pos="6:7:7" line-data="//STEP05 EXEC PGM=CBCUS01C">`CBCUS01C`</SwmToken> program.

More about <SwmToken path="app/jcl/READCUST.jcl" pos="6:7:7" line-data="//STEP05 EXEC PGM=CBCUS01C">`CBCUS01C`</SwmToken>: <SwmLink doc-title="Reading and Printing Customer Data (CBCUS01C)">[Reading and Printing Customer Data (CBCUS01C)](/.swm/reading-and-printing-customer-data-cbcus01c.aznedxgn.sw.md)</SwmLink>

```jcl
//* RUN THE PROGRAM THAT READS THE CUSTOMER MASTER VSAM FILE
//* *******************************************************************
//STEP05 EXEC PGM=CBCUS01C
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="kyndryl-aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
