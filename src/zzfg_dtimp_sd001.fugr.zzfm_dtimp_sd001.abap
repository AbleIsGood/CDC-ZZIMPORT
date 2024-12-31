FUNCTION zzfm_dtimp_sd001.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IO_DATA) TYPE REF TO  DATA OPTIONAL
*"     VALUE(IV_STRUC) TYPE  ZZESTRUCTNAME OPTIONAL
*"  EXPORTING
*"     REFERENCE(EO_DATA) TYPE REF TO  DATA
*"----------------------------------------------------------------------
  .
*&---按模板创建数据，iv_struc 批导设置是提供结构参数
  CREATE DATA eo_data TYPE TABLE OF (iv_struc).
*&---数据赋值（IN ==> OUT）
  eo_data->* = io_data->*.

  DATA:lcl_struc TYPE REF TO cl_abap_structdescr,
       lt_field  TYPE abap_compdescr_tab.
  DATA:lt_tab TYPE TABLE OF zztsd_0002,
       ls_tab TYPE zztsd_0002.
  DATA:lv_matnr(18).

  lcl_struc ?= cl_abap_structdescr=>describe_by_data( ls_tab ).
  lt_field = lcl_struc->components.

  LOOP AT eo_data->* ASSIGNING FIELD-SYMBOL(<ls_data>).
    CLEAR:ls_tab.
    LOOP AT lt_field INTO DATA(ls_field).
      ASSIGN COMPONENT ls_field-name OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<fs_value>).
      IF sy-subrc = 0.
        ASSIGN COMPONENT ls_field-name OF STRUCTURE ls_tab TO FIELD-SYMBOL(<fs_data>).
        IF sy-subrc = 0.
          <fs_data> = <fs_value>.
          IF ls_field-name = 'MATNR'.
            lv_matnr = <fs_data>.
            lv_matnr = |{ lv_matnr ALPHA = IN }|.
            <fs_data> = lv_matnr.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    ls_tab-created_by = sy-uname.
    GET TIME STAMP FIELD ls_tab-created_at.
    ls_tab-last_changed_by = ls_tab-created_by.
    ls_tab-last_changed_at =  ls_tab-created_at.

    "校验物料
    SELECT COUNT(*)
      FROM i_product WITH PRIVILEGED ACCESS
     WHERE product = @ls_tab-matnr.
    IF sy-subrc <> 0.
      <ls_data>-('Type') = 'E'.
      <ls_data>-('Message') = '物料不存在！'.
      CONTINUE.
    ENDIF.

    "校验单位
    SELECT SINGLE *
      FROM i_unitofmeasurecommercialname WITH PRIVILEGED ACCESS
     WHERE unitofmeasurecommercialname = @ls_tab-meins
       AND language = 1
      INTO @DATA(ls_unit).
    IF sy-subrc <> 0.
      <ls_data>-('Type') = 'E'.
      <ls_data>-('Message') = '单位不存在！'.
      CONTINUE.
    ELSE.
      ls_tab-meins = ls_unit-unitofmeasure.
    ENDIF.


    APPEND ls_tab TO lt_tab.

    <ls_data>-('Type') = 'S'.
    <ls_data>-('Message') = 'SUCCESS'.
  ENDLOOP.

  IF lt_tab IS NOT INITIAL.
    MODIFY zztsd_0002 FROM TABLE @lt_tab.
  ENDIF.






ENDFUNCTION.
