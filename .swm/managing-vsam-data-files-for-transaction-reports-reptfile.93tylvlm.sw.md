---
title: Managing VSAM data files for transaction reports (REPTFILE)
---
The REPTFILE job is responsible for managing VSAM data files for transaction reports. This job ensures that any existing VSAM file for transaction reports is deleted if it already exists and then defines a new generation data group for the same purpose.

For instance, if there is an existing VSAM file named AWS.M2.CARDDEMO.TRANREPT, the job will delete it and create a new generation data group with the same name, ensuring that the system always works with the latest data.

## Managing VSAM data files for transaction reports

Steps in this section: `STEP05`.

This section is responsible for deleting an existing VSAM file for transaction reports if it already exists and then defining a new generation data group for the same purpose.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBa3luZHJ5bC1hd3MtbWFpbmZyYW1lLW1vZGVybml6YXRpb24tY2FyZGRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="kyndryl-aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
