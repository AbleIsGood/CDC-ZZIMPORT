@EndUserText.label: 'CDS of Function Modules for Data Import'
@ObjectModel.query.implementedBy:'ABAP:ZZCL_DTIMP_DDICS'
define custom entity ZZR_DTIMP_FUNC
{
      @EndUserText.label : 'Function Module Name'
  key FunctionModuleName : abap.char( 64 );
      @EndUserText.label : 'Description'
      FunctionModuleDesc : abap.sstring( 120 );
  
}
