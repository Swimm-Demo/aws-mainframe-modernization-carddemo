---
title: Processing Transaction Balances and Computing Interest (INTCALC)
---
This document describes the INTCALC job, which is responsible for processing the transaction balance file and computing interest and fees for accounts. The job achieves this by executing the <SwmToken path="app/jcl/INTCALC.jcl" pos="22:7:7" line-data="//STEP15 EXEC PGM=CBACT04C,PARM=&#39;2022071800&#39;                                    ">`CBACT04C`</SwmToken> program, which calculates account interest based on transactions and manages various datasets related to account data, card references, and transaction management.

For example, the INTCALC job takes the transaction balance file as input and processes it to compute the interest and fees, updating the relevant account datasets accordingly.

## Processing transaction balances and computing interest and fees

Steps in this section: <SwmToken path="app/jcl/INTCALC.jcl" pos="22:1:1" line-data="//STEP15 EXEC PGM=CBACT04C,PARM=&#39;2022071800&#39;                                    ">`STEP15`</SwmToken>.

This section is responsible for processing the transaction balance file and computing interest and fees for accounts. It involves executing a program to calculate account interest based on transactions and managing various datasets related to account data, card references, and transaction management.

<SwmSnippet path="/app/jcl/INTCALC.jcl" line="20">

---

Here we call the <SwmToken path="app/jcl/INTCALC.jcl" pos="22:7:7" line-data="//STEP15 EXEC PGM=CBACT04C,PARM=&#39;2022071800&#39;                                    ">`CBACT04C`</SwmToken> program.

More about <SwmToken path="app/jcl/INTCALC.jcl" pos="22:7:7" line-data="//STEP15 EXEC PGM=CBACT04C,PARM=&#39;2022071800&#39;                                    ">`CBACT04C`</SwmToken>: <SwmLink doc-title="Interest Calculator (CBACT04C)">[Interest Calculator (CBACT04C)](/.swm/interest-calculator-cbact04c.ocwi2mf7.sw.md)</SwmLink>

```jcl
//* Process transaction balance file and compute interest and fees.
//* *******************************************************************         
//STEP15 EXEC PGM=CBACT04C,PARM='2022071800'                                    
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="kyndryl-aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
