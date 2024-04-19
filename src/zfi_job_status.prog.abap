*&---------------------------------------------------------------------*
*& Report  ZFI_JOB_STATUS
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Program Name       :  ZFI_JOB_STATUS                                 *
* Author             :  Manvitha Dadi                                  *
* Date               :  12-04-2022                                     *
* Change Request     :  CHG0246247                                     *
* Purpose            :  program to monitor LUG Billing job and trigger *
*                       LEGD Billing jobs                              *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 12-Apr-2022  DADIM       D30K932118-Initial development              *
*----------------------------------------------------------------------*

REPORT  zfi_job_status.

INCLUDE zfi_job_status_top.
INCLUDE zfi_job_status_evt.
INCLUDE zfi_job_status_f01.
