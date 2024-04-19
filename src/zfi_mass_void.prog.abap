*&---------------------------------------------------------------------*
*& Report  ZFI_MASS_VOID
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*&-----------------------------------------------------------------------*
*& Report Name          : ZFI_MASS_VOID                                  *
*& Author               : KMB                                            *
*& Creation Date        : 12-Jun-2019                                    *
*& Transport no.        : D30K929933                                     *
*& Object ID            : CHG0147638                                     *
*& Application Area     : FI                                             *
*& Description          : Mass voiding of cheques                        *
*&-----------------------------------------------------------------------*

REPORT zfi_mass_void NO STANDARD PAGE HEADING LINE-SIZE 255.

INCLUDE zfi_mass_void_top.
INCLUDE zfi_mass_void_sel.
INCLUDE zfi_mass_void_main.
