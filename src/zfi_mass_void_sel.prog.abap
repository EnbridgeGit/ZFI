*&---------------------------------------------------------------------*
*&  Include           ZFI_MASS_VOID_SEL
*&---------------------------------------------------------------------*
*&-----------------------------------------------------------------------*
*& Report Name          : ZFI_MASS_VOID_SEL                              *
*& Author               : KMB                                            *
*& Creation Date        : 12-Jun-2019                                    *
*& Transport no.        : D30K929933                                     *
*& Object ID            : CHG0147638                                     *
*& Application Area     : FI                                             *
*& Description          : Mass voiding of cheques                        *
*&-----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
PARAMETERS: p_file   TYPE cffile-filename OBLIGATORY,  "File Name
            p_dismod TYPE ctu_params-dismode DEFAULT 'A', "Mode - A/N/E
            r_fch8   RADIOBUTTON GROUP r1,
            r_fch9   RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK b2.
