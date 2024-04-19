*&---------------------------------------------------------------------*
*&  Include           ZFIPS_MASTER_DATA_UPDATE_SCR
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFIPS_MASTER_DATA_UPDATE                      *
*& Author             :  KMB                                           *
*& Creation Date      :  19/11/2020                                    *
*& Object ID          :  CHG0203062                                    *
*& Application Area   :  FICO                                          *
*& Description        :  Optimization of PS master data update         *
*&-------------------------------------------------------------------- *
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 19-03-2020   KMB         D30K930800  CHG020306  Initial              *
************************************************************************
PARAMETERS: p_file   TYPE cffile-filename OBLIGATORY,  "File Name
            p_dismod TYPE ctu_params-dismode DEFAULT 'N'. "Mode - A/N/E
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-001.
PARAMETERS : r_sf        RADIOBUTTON GROUP rad2 DEFAULT 'X' USER-COMMAND cmnd, "Start date finish date fields
             r_est       RADIOBUTTON GROUP rad2,                               "Estimated In-service Date
             r_c55       RADIOBUTTON GROUP rad2.                               "C55 investment code fields
SELECTION-SCREEN END OF BLOCK b4.
