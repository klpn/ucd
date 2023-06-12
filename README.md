# ucd

This software is intended to be used to explore the WHO rules for selecting
underlying cause of death from a death certificate, as described in
[ICD10 Volume2](https://icd.who.int/browse10/Content/statichtml/ICD10Volume2_en_2019.pdf)
sec 4.1 and 4.2, and updated guidelines for 
[covid-19](https://apps.who.int/iris/rest/bitstreams/1279836/retrieve).

Note that the implementation of the rules is still very partial.

The program can be built and run using the
[Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).
After setting up Stack and cloning the repository (or after pulling or making 
changes to the code), run `stack build`.

The software can be tested on US mortality multiple cause files, available via
[CDC](https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm) transformed with
the script `mortus.awk`. E.g., if the 2020 data file has been unpacked as
`data/VS20MORT.DUSMCPUB_r20220105`, you may run the tool on the first,
500 records like:

```
sed 501q data/VS20MORT.DUSMCPUB_r20220105 | ./mortus.awk | stack exec ucd
```

If the underlying cause selected by the software diverges from the one recorded
in the data file, the two will be printed separated by |, with the one in the
file first. Because the pipe character does not otherwise occur in the data,
you may further pipe the output into `grep '|'` to filter out diverging
records.
