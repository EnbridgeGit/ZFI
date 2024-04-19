*&---------------------------------------------------------------------*
*&  Include           ZLPSI017_RECON_PLANT_TOP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES Declration                                                    *
*----------------------------------------------------------------------*
TABLES: faglflext.  "General Ledger: Totals

*----------------------------------------------------------------------*
* TYPES Declration                                                     *
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_faglflext,
        ryear   TYPE faglflext-ryear,   "Fiscal Year
        objnr00 TYPE faglflext-objnr00, "Object number for table group
        objnr01 TYPE faglflext-objnr01, "Object number for table group
        objnr02 TYPE faglflext-objnr02, "Object number for table group
        objnr03 TYPE faglflext-objnr03, "Object number for table group
        objnr04 TYPE faglflext-objnr04, "Object number for table group
        objnr05 TYPE faglflext-objnr05, "Object number for table group
        objnr06 TYPE faglflext-objnr06, "Object number for table group
        objnr07 TYPE faglflext-objnr07, "Object number for table group
        objnr08 TYPE faglflext-objnr08, "Object number for table group
        drcrk   TYPE faglflext-drcrk,   "Debit/Credit Indicator
        rpmax   TYPE faglflext-rpmax,   "Period
        rldnr   TYPE faglflext-rldnr,   "Ledger in General Ledger Accounting
        racct   TYPE faglflext-racct,   "Account Number
        rbukrs  TYPE faglflext-rbukrs,  "Company Code
        hslvt   TYPE faglflext-hslvt,   "Balance carried forward in local currency
        hsl01   TYPE faglflext-hsl01,   "Total of transactions of the period in local currency
        hsl02   TYPE faglflext-hsl02,   "Total of transactions of the period in local currency
        hsl03   TYPE faglflext-hsl03,   "Total of transactions of the period in local currency
        hsl04   TYPE faglflext-hsl04,   "Total of transactions of the period in local currency
        hsl05   TYPE faglflext-hsl05,   "Total of transactions of the period in local currency
        hsl06   TYPE faglflext-hsl06,   "Total of transactions of the period in local currency
        hsl07   TYPE faglflext-hsl07,   "Total of transactions of the period in local currency
        hsl08   TYPE faglflext-hsl08,   "Total of transactions of the period in local currency
        hsl09   TYPE faglflext-hsl09,   "Total of transactions of the period in local currency
        hsl10   TYPE faglflext-hsl10,   "Total of transactions of the period in local currency
        hsl11   TYPE faglflext-hsl11,   "Total of transactions of the period in local currency
        hsl12   TYPE faglflext-hsl12,   "Total of transactions of the period in local currency
        hsl13   TYPE faglflext-hsl13,   "Total of transactions of the period in local currency
        hsl14   TYPE faglflext-hsl14,   "Total of transactions of the period in local currency
        hsl15   TYPE faglflext-hsl15,   "Total of transactions of the period in local currency
        hsl16   TYPE faglflext-hsl16,   "Total of transactions of the period in local currency
  END OF ty_faglflext,

  BEGIN OF ty_data,
        ryear   TYPE faglflext-ryear,   "Fiscal Year
        rldnr   TYPE faglflext-rldnr,   "Ledger in General Ledger Accounting
        racct   TYPE faglflext-racct,   "Account Number
        rbukrs  TYPE faglflext-rbukrs,  "Company Code
        drcrk   TYPE faglflext-drcrk,   "Debit/Credit Indicator
        amount  TYPE faglflext-tsl01,   " Total Amount
  END OF ty_data,

  BEGIN OF ty_file,
    record TYPE string,
  END OF ty_file.

*----------------------------------------------------------------------*
* Work Area                                                            *
*----------------------------------------------------------------------*
DATA: gwa_faglflext  TYPE ty_faglflext,
      gwa_data       TYPE ty_data,
      gwa_xparam     TYPE zfit_xparam.

*----------------------------------------------------------------------*
* Variables                                                            *
*----------------------------------------------------------------------*
DATA: gv_err_flag(1) TYPE c,
      gv_fname       TYPE string.

*----------------------------------------------------------------------*
* Internal Table                                                       *
*----------------------------------------------------------------------*
DATA: git_faglflext     TYPE TABLE OF ty_faglflext,
      git_data          TYPE TABLE OF ty_data,
      git_file          TYPE TABLE OF ty_file,
      git_xparam        TYPE TABLE OF zfit_xparam.

*----------------------------------------------------------------------*
* Table Types                                                          *
*----------------------------------------------------------------------*
TYPES: tt_file            TYPE STANDARD TABLE OF ty_file.

*----------------------------------------------------------------------*
* CONSTANTS                                                            *
*----------------------------------------------------------------------*
CONSTANTS: gc_x(1)       TYPE c VALUE 'X',
           gc_e(1)       TYPE c VALUE 'E',
           gc_c(1)       TYPE c VALUE 'C',
           gc_paramtype  TYPE zparamtype  VALUE 'OUTBOUND_INTERFACE',
           gc_subtype    TYPE zparamtype  VALUE 'I_CL2C_PS_017',
           gc_outfile    TYPE zparamkey   VALUE 'OUTPUT_FILEPATH',
           gc_archive    TYPE zparamkey   VALUE 'ARCHIVE_FILEPATH'.
