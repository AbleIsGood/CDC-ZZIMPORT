@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View for ZR_ZT_BATCH_FILES'
define root view entity ZC_ZT_BATCH_FILES
  provider contract transactional_query
  as projection on ZR_ZT_BATCH_FILES
{
  key      UUID,
           UuidConf,
            _configuration.Objectname,
           MimeType,
           Attachment,
           FileName,
           Jobcount,
           Jobname,
           LogHandle,
           LocalLastChangedAt,

           @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZZCL_GET_JOB_STATUS'
  virtual  JobStatus            : abap.char( 1 ),
           @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZZCL_GET_JOB_STATUS'
  virtual  JobStatusText        : abap.char( 20 ),
           @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZZCL_GET_JOB_STATUS'
  virtual  JobStatusCriticality : abap.int1,

           @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZZCL_GET_JOB_STATUS'
  virtual  LogStatus            : abap.char( 1 ),
           @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZZCL_GET_JOB_STATUS'
  virtual  LogStatusText        : abap.char( 20 ),
           @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZZCL_GET_JOB_STATUS'
  virtual  LogStatusCriticality : abap.int1,
           @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZZCL_GET_JOB_STATUS'
  virtual  ApplicationLogUrl    : abap.string( 256 ),

           _configuration : redirected to ZC_ZT_BATCH_CONFIG
}
