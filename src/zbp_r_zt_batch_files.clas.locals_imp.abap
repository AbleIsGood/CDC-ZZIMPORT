CLASS lhc_zr_zt_batch_files DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS:
      get_global_authorizations FOR GLOBAL AUTHORIZATION
        IMPORTING
        REQUEST requested_authorizations FOR zr_zt_batch_files
        RESULT result,
      get_instance_features FOR INSTANCE FEATURES
        IMPORTING keys REQUEST requested_features FOR Files RESULT result.
ENDCLASS.

CLASS lhc_zr_zt_batch_files IMPLEMENTATION.
  METHOD get_global_authorizations.
  ENDMETHOD.
  METHOD get_instance_features.
    READ ENTITIES OF zr_zt_batch_files IN LOCAL MODE
        ENTITY Files
     FIELDS (  uuid JobName )
     WITH CORRESPONDING #( keys )
    RESULT DATA(files)
    FAILED failed.

    result = VALUE #( FOR file IN files
                   ( %tky                           = file-%tky

                     %update = COND #( WHEN file-JobName IS NOT INITIAL
                                                              THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
                     %action-Edit = COND #( WHEN file-JobName IS NOT INITIAL
                                                              THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
*                     %field-JobCount = COND #( WHEN file- IS INITIAL
*                                                              THEN if_abap_behv=>fc-f-read_only ELSE if_abap_behv=>fc-f-unrestricted   )
*                     %field-JobName = COND #( WHEN file-LastChangedBy IS INITIAL
*                                                              THEN if_abap_behv=>fc-f-read_only ELSE if_abap_behv=>fc-f-unrestricted   )
*                     %field-LogHandle = COND #( WHEN file-LastChangedBy IS INITIAL
*                                                              THEN if_abap_behv=>fc-f-read_only ELSE if_abap_behv=>fc-f-unrestricted   )
                  ) ).
  ENDMETHOD.
ENDCLASS.

CLASS lsc_zr_zt_batch_files DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

ENDCLASS.

CLASS lsc_zr_zt_batch_files IMPLEMENTATION.

  METHOD save_modified. " Trigger Job
    DATA job_template_name TYPE cl_apj_rt_api=>ty_template_name VALUE 'ZZCL_JT_BATCH'.

    DATA job_start_info TYPE cl_apj_rt_api=>ty_start_info.
    DATA job_parameters TYPE cl_apj_rt_api=>tt_job_parameter_value.
    DATA job_parameter TYPE cl_apj_rt_api=>ty_job_parameter_value.
    DATA range_value TYPE cl_apj_rt_api=>ty_value_range.
    DATA job_name TYPE cl_apj_rt_api=>ty_jobname.
    DATA job_count TYPE cl_apj_rt_api=>ty_jobcount.


    IF create-files IS NOT INITIAL.
      LOOP AT create-files ASSIGNING FIELD-SYMBOL(<file>).
        TRY.
            "trigger a job
*            GET TIME STAMP FIELD DATA(start_time_of_job).
*          job_start_info-timestamp = start_time_of_job.
            job_start_info-start_immediately = abap_true.
            job_parameter-name = 'P_ID' . "'INVENT'.
            range_value-sign = 'I'.
            range_value-option = 'EQ'.
            range_value-low = <file>-uuid.
            APPEND range_value TO job_parameter-t_value.
            APPEND job_parameter TO job_parameters.
            cl_apj_rt_api=>schedule_job(
                  EXPORTING
                  iv_job_template_name = job_template_name
                  iv_job_text = |Batch Import Job of { <file>-uuid }|
                  is_start_info = job_start_info
                  it_job_parameter_value = job_parameters
                  IMPORTING
                  ev_jobname  = job_name
                  ev_jobcount = job_count
                  ).

          CATCH cx_apj_rt INTO DATA(job_scheduling_error).

            "reported-<entity name>
            APPEND VALUE #(  uuid = <file>-uuid

                             %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = job_scheduling_error->bapimsg-message )
                            )
              TO reported-files.

          CATCH cx_root INTO DATA(root_exception).

            "reported-<entity name>
            APPEND VALUE #(  uuid = <file>-uuid
                             %msg = new_message(
                             id       = '00'
                             number   = 000
                             severity = if_abap_behv_message=>severity-error
                             v1       = |Root Exc: { root_exception->get_text(  ) }|
                             )
                           )
              TO reported-files.

        ENDTRY.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
