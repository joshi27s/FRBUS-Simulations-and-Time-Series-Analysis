' Program that simulates the impact of a modified Taylor rule under the current Evan's Rule with VAR expectations

' *************************************************************
' Initial filename and parameter settings
' *************************************************************

' Subroutines
  include ..\subs\master_library

' Workfile    
  %wfstart = "1975q1"
  %wfend = "2125q4"
  %mainpage = "main"
  wfclose(noerr)
  wfcreate(wf=aaa,page={%mainpage}) q {%wfstart} {%wfend}

' FRB/US model name and location
  %varmod = "stdver"
  %model_path = "..\mods\model.xml"

' Input database
%dbin  = "..\data\longbase"

' Simulation start and length
%simstart = "2020q2"
  !nsimqtrs = 16*4  
 call dateshift(%simstart,%simend,!nsimqtrs-1)

' Policy 
  %zb = "yes"
  %threshold = "yes"
  %policy = "rffintay"


' ****************************************************************
' Retrieve data, model equations and coefficients, set
' policy options, and compute tracking residuals 
' ****************************************************************

' Load equations and coefficients
  read_xml_model(path=%model_path)

' Load data
  dbopen %dbin as longbase
  smpl @all
  fetch(d=longbase) *


' Set monetary policy rule
  smpl @all
  %policydmp = @replace(%policy,"rff","dmp")
  call set_mp(%policydmp)

' Set ZLB
  if %zb = "yes" then
    rffmin = .125
    else
    rffmin = -9999
    endif

' Set threshold variables 
  if %threshold = "yes" then
    if %zb = "no" then
      @uiprompt("When policy thresholds are imposed, the zero bound must also be imposed")
      stop
      endif
    smpl @all
    call dateshift(%simstart,%quarter4,3)

  ' thresholds (dmptrsh and dmptr) not active in first 4 qtrs
   smpl %simstart - 1 %quarter4
    dmptrsh = 0
    lurtrsh = -9999
    pitrsh = 9999
    dmptr = 0

  ' thresholds (dmptrsh and dmptr) active starting in qtr 5
    smpl %quarter4 + 1 %simend
    dmptrsh = 1
    lurtrsh = 6.5
    pitrsh = 2.5

    smpl @all
    else
    smpl @all
    dmptrsh = 0
    endif

  smpl @all
  drstar = 0

' Set fiscal policy
  'smpl @all
  'call set_fp("dfpdbt")

' Set _aerr variables to zero
  smpl @all
  {%varmod}.makegroup(a,n) endog @endog
  call groupnew("endog","_aerr")
  call group2zero("endog_aerr")



'Add the data on epop and related thresholds
{%varmod}.append rgdpch = ((xgdp - xgdp(-4))/ xgdp) * 100
{%varmod}.append epop = lfpr*(1-lur/100)



' Standard solution options
  {%varmod}.solveopt(o=b,g=12,z=1e-12)


' Assign baseline tracking add factors
  %suftrk = "_0"
  smpl %simstart 2020q2
  {%varmod}.addassign @all
  {%varmod}.addinit(v=n) @all
  {%varmod}.scenario(n,a={%suftrk}) "track"
  {%varmod}.solve
  scalar mm = @max(@abs(xgap{%suftrk}-xgap))
  if mm > .0001 then
    statusline dynamic tracking simulation failed for {%varmod}
    stop
    endif

' Set monetary policy add factors to zero when ZLB or threshold are
' imposed

  if %zb = "yes" then
    smpl @all
    {%policy}_a = 0
    rffrule_a = 0
    rff_a = 0
    if %threshold = "yes" then
      'dmptpi_a = 0
      'dmptlur_a = 0
       dmpteci_a = 0

      dmptmax_a = 0
      dmptr_a = 0
      endif
    endif




' *************************************************************
' Simulation 1 : Taylor rule after crossing pictrsh
' *************************************************************

  %sufsim = "_1"
  {%varmod}.scenario(n,a={%sufsim}) "sim"

  smpl @all
  call set_mp("dmptay")
%policy = "rffintay"


' Set fiscal policy
  smpl @all
  call set_fp("dfpsrp")

' shock values are taken from equation residuals for 2008q4-2009q3
  eco_a.fill(o=%simstart) -.006, -.006, -.011, -.001
  ecd_a.fill(o=%simstart) -.091, -.018, -.021,  .029
  eh_a.fill(o=%simstart)  -.076, -.078, -.040,  .073
  ebfi_a.fill(o=%simstart) -.114, -.108,  .050,  .049
  rbbbp_a.fill(o=%simstart) 2.70, 0.38, -0.89, -1.35


 dmptrsh = 1
dmptmax = 1 
 rffmin = 0.0 ' impose ZLB
 drstar = 1 'rstar is exogenous
 pitrsh = 2.5
 lurtrsh = 6.5
'dmpteci = 3
 dmptr = 1

  smpl %simstart %simend
  {%varmod}.solve


' *************************************************************
' Simulation 2 : Modified Taylor rule after crossing pictrsh
' *************************************************************

  %sufsim1 = "_2"
  {%varmod}.scenario(n,a={%sufsim1}) "sim2"

'@pmax: pairwise max function that returns the pairwise max of two inputs: e.g: max(x,y)
  {%varmod}.drop rfftay
  {%varmod}.append rfftay = @pmax(rstar + ( picxfe + picxfe(t-1) + picxfe(t-2) + picxfe(t-3)) / 4 + 2 * ( ( picxfe + picxfe(t-1) + picxfe(t-2) + picxfe(t-3)) / 4 -pitarg) + 2 * xgap2, 0)

  call set_mp("dmptay")
%policy = "rrftay"


  smpl @all

' Set fiscal policy
  smpl @all
  call set_fp("dfpdbt")


   ' shock values are taken from equation residuals for 2008q4-2009q3
  eco_a.fill(o=%simstart) -.006, -.006, -.011, -.001
  ecd_a.fill(o=%simstart) -.091, -.018, -.021,  .029
  eh_a.fill(o=%simstart)  -.076, -.078, -.040,  .073
  ebfi_a.fill(o=%simstart) -.114, -.108,  .050,  .049
  rbbbp_a.fill(o=%simstart) 2.70, 0.38, -0.89, -1.35



' Set threshold variables 
  if %threshold = "yes" then
    if %zb = "no" then
      @uiprompt("When policy thresholds are imposed, the zero bound must also be imposed")
      stop
      endif
    smpl @all
    call dateshift(%simstart,%quarter4,3)

  ' thresholds (dmptrsh and dmptr) not active in first 4 qtrs
   smpl %simstart - 1 %quarter4
    dmptrsh = 0
    lurtrsh = -9999
    pitrsh = 9999
    dmptr = 0

  ' thresholds (dmptrsh and dmptr) active starting in qtr 5
   smpl %quarter4 + 1 %simend
    dmptrsh = 1
    dmptmax = 1 
    rffmin = 0.0 ' impose ZLB
    lurtrsh = 6.5
    pitrsh = 2.5
   dmptr = 1

  

    smpl @all
    else
    smpl @all
    dmptrsh = 0
    endif




  smpl %simstart %simend
  {%varmod}.solve



'***********************************************************
' Make a graph
'***********************************************************

  call dateshift(%simstart,%graphstart, -5)
  call dateshift(%simstart,%graphend, 24)

smpl %graphstart %graphend
  graph fig1a.line rff rff{%sufsim} rff{%sufsim1}
  fig1a.options size(7,4.2)
  fig1a.legend display -inbox position(3.8,2.8) font("arial",15)
  fig1a.datelabel format(yyyy)
  fig1a.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1a.axis(left) font("arial",15)
  fig1a.axis(bottom) font("arial",15)
  fig1a.setelem(1) lcolor(black)  legend("Consensus baseline") lwidth(1)
  fig1a.setelem(2) lcolor(green)  legend("Original Taylor Rule") lwidth(1)
  fig1a.setelem(3) lcolor(red)  legend(" Modified Taylor Rule") lwidth(1)
  fig1a.addtext(t,just(c),font("arial",18)) Federal Funds Rate


  smpl %graphstart %graphend
  graph fig1b.line rgdpch rgdpch{%sufsim} rgdpch{%sufsim1}
  fig1b.options size(7,4.2)
  fig1b.legend display -inbox position(3.8,2.8) font("arial",15)
  fig1b.datelabel format(yyyy)
  fig1b.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1b.axis(left) font("arial",15)
  fig1b.axis(bottom) font("arial",15)
  fig1b.setelem(1) lcolor(black) legend("Consensus baseline") lwidth(1)
  fig1b.setelem(2) lcolor(green)  legend("Original Taylor Rule") lwidth(1)
  fig1b.setelem(3) lcolor(red)  legend(" Modified Taylor Rule") lwidth(1)
  fig1b.addtext(t,just(c),font("arial",18)) Year to Year % Change in Real GDP


  smpl %graphstart %graphend
  graph fig1c.line  lur lur{%sufsim} lur{%sufsim1}
  fig1c.options size(7,4.2)
  fig1c.legend display -inbox position(3.9,0.3) font("arial",15)
  fig1c.datelabel format(yyyy)
  fig1c.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1c.axis(left) font("arial",15)
  fig1c.axis(bottom) font("arial",15)
  fig1c.setelem(1) lcolor(black) legend("Consensus baseline") lwidth(1)
  fig1c.setelem(2) lcolor(green)  legend("Original Taylor Rule") lwidth(1)
  fig1c.setelem(3) lcolor(red)  legend(" Modified Taylor Rule") lwidth(1)
  fig1c.addtext(t,just(c),font("arial",18)) Unemployment Rate


  smpl %graphstart %graphend
  graph fig1d.line pic4 pic4{%sufsim} pic4{%sufsim1}
  fig1d.options size(7,4.2)
  fig1d.legend display -inbox position(0.5,0.2) font("arial",15)
  fig1d.datelabel format(yyyy)
  fig1d.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1d.axis(left) font("arial",15)
  fig1d.axis(bottom) font("arial",15)
  fig1d.setelem(1) lcolor(black)  legend("Consensus baseline") lwidth(1)
  fig1d.setelem(2) lcolor(green)  legend("Original Taylor Rule") lwidth(1)
  fig1d.setelem(3) lcolor(red)  legend(" Modified Taylor Rule") lwidth(1)
 fig1d.addtext(t,just(c),font("arial",18)) PCE Inflation Rate (4-Quarter)

smpl %graphstart %graphend
  graph fig1e.line  epop epop{%sufsim} epop{%sufsim1}
  fig1e.options size(7,4.2)
  fig1e.legend display -inbox position(3.9,0.3) font("arial",15)
  fig1e.datelabel format(yyyy)
  fig1e.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1e.axis(left) font("arial",15)
  fig1e.axis(bottom) font("arial",15)
  fig1e.setelem(1) lcolor(black) legend("Consensus baseline") lwidth(1)
  fig1e.setelem(2) lcolor(green)  legend("Original Taylor Rule") lwidth(1)
  fig1e.setelem(3) lcolor(red)  legend(" Modified Taylor Rule") lwidth(1)
  fig1e.addtext(t,just(c),font("arial",18)) Employment to Population Ratio

  smpl %graphstart %graphend
  graph fig1f.line pieci pieci{%sufsim} pieci{%sufsim1}
  fig1f.options size(7,4.2)
  fig1f.legend display -inbox position(0.5,0.2) font("arial",15)
  fig1f.datelabel format(yyyy)
  fig1f.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1f.axis(left) font("arial",15)
  fig1f.axis(bottom) font("arial",15)
  fig1f.setelem(1) lcolor(black)  legend("Consensus baseline") lwidth(1)
  fig1f.setelem(2) lcolor(green)  legend("Original Taylor Rule") lwidth(1)
  fig1f.setelem(3) lcolor(red)  legend(" Modified Taylor Rule") lwidth(1)
  fig1f.addtext(t,just(c),font("arial",18)) Annualized rate of growth of EI hourly compensation

smpl %graphstart %graphend
  graph fig1g.line rg10 rg10{%sufsim} rg10{%sufsim1}
  fig1g.options size(7,4.2)
  fig1g.legend display -inbox position(0.5,0.2) font("arial",15)
  fig1g.datelabel format(yyyy)
  fig1g.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1g.axis(left) font("arial",15)
  fig1g.axis(bottom) font("arial",15)
  fig1g.setelem(1) lcolor(black)  legend("Consensus baseline") lwidth(1)
  fig1g.setelem(2) lcolor(green)  legend("Original Taylor Rule") lwidth(1)
  fig1g.setelem(3) lcolor(red)  legend(" Modified Taylor Rule") lwidth(1)
  fig1g.addtext(t,just(c),font("arial",18)) 10-Year Treasury Rate

smpl %graphstart %graphend
  graph fig1h.line picxfe picxfe{%sufsim} picxfe{%sufsim1}
  fig1h.options size(7,4.2)
  fig1h.legend display -inbox position(0.5,0.2) font("arial",15)
  fig1h.datelabel format(yyyy)
  fig1h.addtext(6.4,-.30,font("arial",13),keep) percent
  fig1h.axis(left) font("arial",15)
  fig1h.axis(bottom) font("arial",15)
  fig1h.setelem(1) lcolor(black)  legend("Consensus baseline") lwidth(1)
  fig1h.setelem(2) lcolor(green)  legend("Original Taylor Rule") lwidth(1)
  fig1h.setelem(3) lcolor(red)  legend(" Modified Taylor Rule") lwidth(1)
  fig1h.addtext(t,just(c),font("arial",18)) Core PCE Inflation Rate






  %title = "Macroeconomic Effects of a Modified Taylor Rule with a negative AD Shock\r(VAR Expectations"
  %title = %title + "; Policy = " + %policy + ")"
  if %zb = "yes" and %threshold = "no" then
    %title = %title + "\r(ZLB Imposed)"
    endif
  if %zb = "yes" and %threshold = "yes" then
    %title = %title + "\r(ZLB and Thresholds Imposed)"
    endif

  graph fig1.merge fig1a fig1b fig1c fig1d  fig1e fig1f fig1g fig1h
  fig1.addtext(t,just(c),font("Arial",16)) {%title}


 fig1.addtext(t,just(c),font("Arial",20)) {%title}
  fig1.align(2,1,1.25)
  show fig1


