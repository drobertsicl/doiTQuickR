# doiTQuickR
A small WIP package to make processing TQ data easier

1. There will be bugs, I may even fix some of them

2. While this CAN interface to MRMprocessing.exe, distributed with the Waters software for DESI-TQ, I do not have a license to distribute this. Two methods are provided for pixel dimension estimation. When the TQ metadata format is understood scan time disjunction can also be used.

3. Unlike QuantMSImageR, this does not require vendor tools and can work from raw data. However as a result certain functionality relies on unideal methods, for example extracting strings from raw data to get MRM names. Currently this relies on the executable "strings" from GNU binutils (see elsewhere to install this)

# How to...

A brief vignette is included. Please install via:

```
remotes::install_github("drobertsicl/doiTQuickR")
```
