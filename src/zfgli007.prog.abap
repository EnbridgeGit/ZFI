*&---------------------------------------------------------------------*
*& Report  ZFGLI007
*&
*& Author    : Hamid Moinuddin
*& Date      : Nov 27 2020
*&---------------------------------------------------------------------*
*& Description:
*&    This program reads CIS data made available by CTDS on the application
*&    server and transforms them to creates ZBSEG-format files to be fed
*&    to ZKPAA005_NEW to generate BDC sessions for FI postings in SAP ERP
*&
*&---------------------------------------------------------------------*

REPORT  ZFGLI007 MESSAGE-ID zm.

*---------------------------------------------------------------------*
*User defined Includes                                                *
*---------------------------------------------------------------------*
INCLUDE : zfgli007_top,          " data declarations
          zfgli007_screen,       " selection screen & validations
          zfgli007_routines.     " subroutines

INITIALIZATION.

CONCATENATE '/usr/sap/interfaces/' sy-sysid '/CIS/' INTO filepath.
p_infile = p_oufile = p_erfile = filepath.


START-OF-SELECTION.

PERFORM read_file.
PERFORM data_translation.
PERFORM create_output_file.
